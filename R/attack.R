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
attack <- function(object, to_attack = NULL, verbose = FALSE, ...) {
  stopifnot(inherits(object, "sdcProblem"))

  # get/compute constraint matrix
  pi <- slot(object, "problemInstance")
  m <- attributes(pi)$constraint_matrix
  if (is.null(m)) {
    m <- .gen_contraint_matrix(object)
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

  if (verbose) {
    glpkAPI::termOutGLPK(glpkAPI::GLP_ON)
    glpkAPI::setSimplexParmGLPK("MSG_LEV", glpkAPI::GLP_MSG_ON)
  } else {
    glpkAPI::termOutGLPK(glpkAPI::GLP_OFF)
    glpkAPI::setSimplexParmGLPK("MSG_LEV", glpkAPI::GLP_MSG_OFF)
  }

  prob <- glpkAPI::initProbGLPK()
  glpkAPI::setProbNameGLPK(prob, "attackersProblem")

  nr_vars <- ncol(m)
  nr_constraints <- nrow(m)
  glpkAPI::addColsGLPK(prob, nr_vars)
  glpkAPI::addRowsGLPK(prob, nr_constraints)
  glpkAPI::loadMatrixGLPK(
    lp = prob,
    ne = length(m$i),
    ia = m$i ,
    ja = m$j ,
    ra = m$v
  )

  # set obj-to zero
  glpkAPI::setObjCoefsGLPK(prob, j = seq_len(nr_vars), rep(0, nr_vars))
  # bounds by variable

  for (j in seq_len(nr_vars)) {
    if (sdc[j] %in% c("u", "x", "w")) {
      glpkAPI::setColBndGLPK(prob, j = j, type = glpkAPI::GLP_LO, 0, Inf)
    } else {
      glpkAPI::setColBndGLPK(prob, j = j, type = glpkAPI::GLP_FX, freqs[j], freqs[j])
    }
  }
  for (i in seq_len(nr_constraints)) {
    glpkAPI::setRowBndGLPK(prob, i, type = glpkAPI::GLP_FX, 0, 0)
  }

  # solve
  nr_cells <- nrow(out)
  if (verbose) {
    pb <- progress::progress_bar$new(total = nr_cells)
  }
  for (i in seq_len(nr_cells)) {
    if (verbose) {
      pb$tick(1)
    }
    primsupp_to_attack <- out$id[i]
    glpkAPI::setObjCoefGLPK(prob, j = primsupp_to_attack, obj_coef = 1)

    # minimize
    glpkAPI::setObjDirGLPK(prob, glpkAPI::GLP_MIN)
    #glpkAPI::writeLPGLPK(prob, paste0("prob-min-", primsupp_to_attack,".txt"))
    glpkAPI::solveSimplexGLPK(prob)
    out$low[i] <- glpkAPI::getObjValGLPK(prob)

    # maximize
    glpkAPI::setObjDirGLPK(prob, glpkAPI::GLP_MAX)
    #glpkAPI::writeLPGLPK(prob, paste0("prob-max-", primsupp_to_attack,".txt"))
    glpkAPI::solveSimplexGLPK(prob)

    # unbounded solution: possible if entire (sub)table is suppressed
    if (glpkAPI::getSolStatGLPK(prob) == glpkAPI::GLP_UNBND) {
      out$up[i] <- Inf
    } else {
      out$up[i] <- glpkAPI::getObjValGLPK(prob)
    }
    # reset obj
    glpkAPI::setObjCoefGLPK(prob, j = primsupp_to_attack, obj_coef = 0)
  }
  if (verbose) {
    pb$terminate()
  }
  out$protected <- out$up > out$low
  rownames(out) <- NULL
  out
}
