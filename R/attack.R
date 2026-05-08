#' Attacking primary suppressed cells
#'
#' Function [attack()] is used to compute lower and upper bounds for a given
#' sdcProblem instance. For all calculations the current suppression pattern
#' is used when calculating solutions of the attacker's problem.
#'
#' @param object an object of class `sdcProblem`
#' @param to_attack if `NULL` all current primary suppressed cells are attacked;
#' otherwise either an integerish (indices) or character-vector (str-ids) of
#' the cells that should be attacked.
#' @param verbose a logical scalar determining if additional output should be
#' displayed
#' @param threshold a numeric scalar defining the tolerance for the protection
#' check. A cell is considered protected if the absolute difference between the computed
#' upper and lower bound is strictly greater than this threshold. Defaults to `1e-8`.
#' @param n_workers a scalar positive integer specifying the number of parallel
#' workers to use for calculation
#' @param ... placeholder for possible additional input, currently unused;
#' @return a `data.frame` with the following columns:
#' - `sdc`: the original sdc-status code
#' - `freq`: the original value of the cell
#' - `id`: index of primary suppressed cells
#' - `low`: computed lower bound of the attacker's problem
#' - `up`: computed upper bound of the attacker's problem
#' - `protected`: logical, TRUE if the absolute difference between `up` and `low`
#' exceeds the defined `threshold`
#' @rdname attack
#' @export attack
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
#' @examples
#' \dontrun{
#' dims <- list(
#'   v1 = sdcHierarchies::hier_create("tot", letters[1:4]),
#'   v2 = sdcHierarchies::hier_create("tot", letters[5:8])
#' )
#'
#' N <- 150
#' df <- data.frame(
#'   v1 = sample(letters[1:4], N, replace = TRUE),
#'   v2 = sample(letters[5:8], N, replace = TRUE)
#' )
#'
#' sdc <- makeProblem(data = df, dimList = dims)
#'
#' # set primary suppressions
#' specs <- data.frame(
#'   v1 = c("a", "b", "a"),
#'   v2 = c("e", "e", "f")
#' )
#' sdc <- change_cellstatus(sdc, specs = specs, rule = "u")
#'
#' # attack all primary sensitive cells with a custom threshold
#' attack(sdc, to_attack = NULL, threshold = 1e-6)
#'
#' # protect the table and attack again
#' sdc <- protectTable(sdc, method = "SIMPLEHEURISTIC")
#' attack(sdc, to_attack = NULL)
#'
#' # attack only selected cells
#' attack(sdc, to_attack = c(7, 12))
#'
#' # Parallel processing (future.apply needs to be available)
#' attack(sdc, n_workers = 4)
#' }
attack <- function(object, to_attack = NULL, verbose = TRUE, threshold = 1e-8, n_workers = 1, ...) {
  stopifnot(inherits(object, "sdcProblem"))
  stopifnot(rlang::is_scalar_integerish(n_workers))
  stopifnot(n_workers >= 1)
  stopifnot(rlang::is_scalar_double(threshold))
  stopifnot(threshold > 0)

  # Fetch or compute the mathematical constraint matrix from the problem instance
  pi <- slot(object, "problemInstance")
  m <- attributes(pi)$constraint_matrix
  if (is.null(m)) {
    m <- create_m_matrix(obj = object, convert = FALSE, add_info_df = TRUE)
  }

  all_primsupps <- g_primSupps(pi)
  all_secsupps <- g_secondSupps(pi)
  all_supps <- c(all_primsupps, all_secsupps)

  # Resolve targeted cell indices based on type input validation
  if (is.null(to_attack)) {
    to_attack <- all_primsupps
  } else {
    to_attack <- sort(unique(to_attack))
    if (rlang::is_integerish(to_attack)) {
      stopifnot(all(to_attack %in% all_supps))
    } else if (rlang::is_character(to_attack)) {
      stopifnot(all(to_attack %in% g_strID(pi)[all_supps]))
    } else {
      stop("invalid input detected (argument `to_attack)`)", call. = FALSE)
    }
  }

  df <- data.frame(
    to_attack = FALSE,
    sdc = slot(pi, "sdcStatus"),
    freq = slot(pi, "Freq")
  )
  df$to_attack[to_attack] <- TRUE

  # Execute processing path based on requested worker limits
  parallel <- n_workers > 1
  if (parallel) {
    if (!rlang::is_installed("future.apply")) {
      stop("Package `future.apply` is required for parallel processing. Please install.", call. = FALSE)
    }
    with(future::plan(future::multisession, workers = n_workers), local = TRUE)
  }
  if (verbose) {
    results <- progressr::with_progress({
      if (n_workers > 1) {
        results <- .attack_worker_batched(m = m, df = df, verbose = verbose, n_workers = n_workers)
      } else {
        results <- .attack_worker(m = m, df = df, verbose = verbose)
      }
    })
  } else {
    # If not verbose, run without progressr overhead
    if (n_workers > 1) {
      results <- .attack_worker_batched(m = m, df = df, verbose = verbose, n_workers = n_workers)
    } else {
      results <- .attack_worker(m = m, df = df, verbose = verbose)
    }
  }
  results$protected <- abs(results$up - results$low) > threshold
  rownames(results) <- NULL
  return(results)
}

# Utility-Fn to run dual-sense optimization bounds on a given model instance
.solve_bounds <- function(prob, cell_index, num_vars) {
  out_row <- list(id = cell_index, low = NA, up = NA)

  tryCatch({
    obj <- rep(0, num_vars)
    obj[cell_index] <- 1
    highs::hi_model_set_objective(prob, obj)

    # Minimize objective
    highs::hi_model_set_sense(prob, maximum = FALSE)
    solver_min <- highs::highs_solver(prob)
    solver_min$solve()
    out_row$low <- solver_min$solution()$col_value[cell_index]

    # Maximize objective
    highs::hi_model_set_sense(prob, maximum = TRUE)
    solver_max <- highs::highs_solver(prob)
    solver_max$solve()
    out_row$up <- solver_max$solution()$col_value[cell_index]
  }, error = function(e) {
    warning(paste("Optimization failed at cell", cell_index, ":", e$message))
  })
  return(out_row)
}

# Utility-Fn to construct LP-model for highs solver instances
.create_lp_model <- function(m, df) {
  freqs <- df$freq
  sdc <- df$sdc
  nr_vars <- ncol(m)

  l <- u <- freqs
  l[sdc %in% c("u", "x", "w")] <- 0
  u[sdc %in% c("u", "x", "w")] <- max(freqs)

  prob <- highs::highs_model(
    Q = NULL, L = rep(0, nr_vars),
    lower = l, upper = u, A = m,
    lhs = rep(0, nrow(m)), rhs = rep(0, nrow(m)),
    types = rep("C", nr_vars), maximum = FALSE, offset = 0
  )
  highs::hi_model_set_objective(model = prob, objective = rep(0, nr_vars))
  return(prob)
}

# Sequential worker function
.attack_worker <- function(m, df, verbose = FALSE) {
  idx <- which(df$to_attack)
  out <- df[idx, , drop = FALSE]
  out$id <- idx
  out$to_attack <- NULL

  highs::highs_control(log_to_console = verbose)
  prob <- .create_lp_model(m, df)

  if (verbose) {
    p <- progressr::progressor(steps = nrow(out))
  }

  low_vals <- numeric(nrow(out))
  up_vals <- numeric(nrow(out))

  for (i in seq_len(nrow(out))) {
    cell_res <- .solve_bounds(prob, out$id[i], ncol(m))
    low_vals[i] <- cell_res$low
    up_vals[i] <- cell_res$up
    if (verbose) {
      p(amount = 1)
    }
  }

  out$low <- low_vals
  out$up <- up_vals
  return(out)
}

# Multicore chunked worker processor splitting indices evenly across workers
.attack_worker_batched <- function(m, df, verbose = FALSE, n_workers = 2) {
  .dev_mode <- function() {
    tryCatch({
      pkg_path <- find.package("sdcTable")
      lib_paths <- .libPaths()
      !any(vapply(lib_paths, function(lp) grepl(paste0("^", lp), pkg_path), logical(1)))
    }, error = function(e) NA)
  }

  idx <- which(df$to_attack)

  if (verbose) {
    p <- progressr::progressor(steps = length(idx))
  }

  chunks <- split(idx, cut(seq_along(idx), n_workers, labels = FALSE))
  if (.dev_mode()) {
    # DEVELOPMENT MODE: Manual environment mapping necessary
    f_list <- list(
      .create_lp_model = .create_lp_model,
      .solve_bounds = .solve_bounds
    )
    f_globals <- lapply(f_list, function(fn) {
      environment(fn) <- .GlobalEnv
      return(fn)
    })
    f_globals$m <- m
    f_globals$df <- df
    f_globals$verbose <- verbose
    f_globals$p <- p
    f_pkgs <- c("highs", "progressr")
  } else {
    f_globals <- TRUE
    f_pkgs <- c("highs", "progressr", "sdcTable")
  }

  # Parallel batch processing
  results_list <- future.apply::future_lapply(chunks, function(chunk_indices) {
    prob <- .create_lp_model(m, df)
    nr_vars <- ncol(m)

    chunk_res <- lapply(chunk_indices, function(i) {
      out_row <- .solve_bounds(prob, i, nr_vars)
      if (verbose) p(amount = 1)
      return(out_row)
    })
    return(do.call(rbind, lapply(chunk_res, as.data.frame)))
  },
  future.seed = TRUE,
  future.packages = f_pkgs,
  future.globals = f_globals
  )

  res_df <- do.call(rbind, results_list)
  res_df <- res_df[order(res_df$id), ]

  out <- df[idx, , drop = FALSE]
  out$id <- res_df$id
  out$low <- res_df$low
  out$up <- res_df$up
  out$to_attack <- NULL
  return(out)
}
