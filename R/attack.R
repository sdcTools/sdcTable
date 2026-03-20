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
#' @param verbose a logical scalar determing if additional output should be
#' displayed
#' @param ... placeholder for possible additional input, currently unused;
#' @return a `data.frame` with the following columns:
#' - `prim_supps`: index of primary suppressed cells
#' - `status`: the original sdc-status code
#' - `val` the original value of the cell
#' - `low`: computed lower bound of the attacker's problem
#' - `up`: computed upper bound of the attacker's problem
#' - `protected` shows if a given cell is accordingly protected
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
#' # attack all primary sensitive cells
#' # the cells can be recomputed exactly
#' attack(sdc, to_attack = NULL)
#'
#' # protect the table and attack again
#' sdc <- protectTable(sdc, method = "SIMPLEHEURISTIC")
#' attack(sdc, to_attack = NULL)
#'
#' # attack only selected cells
#' attack(sdc, to_attack = c(7, 12))
#' }
attack <- function(object, to_attack = NULL, verbose = FALSE, ...) {
  stopifnot(inherits(object, "sdcProblem"))

  # get/compute constraint matrix
  pi <- slot(object, "problemInstance")
  m <- attributes(pi)$constraint_matrix
  if (is.null(m)) {
    m <- create_m_matrix(obj = object, convert = FALSE, add_info_df = TRUE)
  }

  all_primsupps <- g_primSupps(pi)
  all_secsupps <- g_secondSupps(pi)
  all_supps <- c(all_primsupps, all_secsupps)

  if (is.null(to_attack)) {
    to_attack <- all_primsupps
  } else {
    to_attack <- sort(unique(to_attack))
    if (rlang::is_integerish(to_attack)) {
      stopifnot(all(to_attack %in% all_supps))
    } else if (rlang::is_character(to_attack)) {
      stopifnot(all(to_attack %in% g_strID(pi)[all_supps]))
    } else {
      stop("invalid input detected (argument `to_attack)`", call. = FALSE)
    }
  }

  df <- data.frame(
    to_attack = FALSE,
    sdc = slot(pi, "sdcStatus"),
    freq = slot(pi, "Freq")
  )
  df$to_attack[to_attack] <- TRUE
  return(.attack_worker(
    m = m,
    df = df,
    verbose = verbose
  ))
}

# m: constraint matrix
# df: data.frame with ids to attack, its frequencies and sdc-stati
.attack_worker <- function(m, df, verbose = FALSE) {
  # create problem

  freqs <- df$freq
  sdc <- df$sdc
  idx <- which(df$to_attack)

  out <- df[idx, , drop = FALSE]
  out$id <- idx
  out$to_attack <- NULL
  out$up <- out$low <- out$freq

  nr_cells <- nrow(out)
  nr_vars <- ncol(m)
  highs::highs_control(log_to_console = verbose)

  l <- u <- freqs
  l[sdc %in% c("u", "x", "w")] <- 0
  u[sdc %in% c("u", "x", "w")] <- max(freqs)

  prob <- highs::highs_model(
    Q = NULL,
    L = rep(0, nr_vars),
    lower = l,
    upper = u,
    A = m,
    lhs = rep(0, nrow(m)),
    rhs = rep(0, nrow(m)),
    types = rep("C", nr_vars),
    maximum = FALSE,
    offset = 0
  )

  # objective
  highs::hi_model_set_objective(
    model = prob,
    objective = rep(0, nr_vars)
  )

  if (verbose) {
    pb <- progress::progress_bar$new(total = nr_cells)
  }
  for (i in seq_len(nr_cells)) {
    if (verbose) {
      pb$tick(1)
    }
    primsupp_to_attack <- out$id[i]
    obj <- rep(0, nr_vars)
    obj[primsupp_to_attack] <- 1
    highs::hi_model_set_objective(
      model = prob,
      objective = obj
    )

    # minimize
    highs::hi_model_set_sense(
      model = prob,
      maximum = FALSE
    )

    solver <- highs::highs_solver(prob)
    solver$solve()
    out$low[i] <- solver$solution()$col_value[primsupp_to_attack]

    # maximize
    highs::hi_model_set_sense(
      model = prob,
      maximum = TRUE
    )
    solver <- highs::highs_solver(prob)
    solver$solve()
    out$up[i] <- solver$solution()$col_value[primsupp_to_attack]
  }
  if (verbose) {
    pb$terminate()
  }
  out$protected <- out$up > out$low
  rownames(out) <- NULL
  out
}
