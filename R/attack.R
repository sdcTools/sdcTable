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

  pI <- g_problemInstance(object)

  ff <- g_freq(pI)
  sdc <- g_sdcStatus(pI)

  all_primsupps <- g_primSupps(pI)
  all_secsupps <- g_secondSupps(pI)
  all_supps <- c(all_primsupps, all_secsupps)

  if (is.null(to_attack)) {
    to_attack <- all_primsupps
  } else {
    to_attack <- sort(unique(to_attack))
    if (rlang::is_integerish(to_attack)) {
      stopifnot(all(to_attack %in% all_supps))
    } else if (rlang::is_character(to_attack)) {
      stopifnot(all(to_attack %in% g_strID(pI)[all_supps]))
    } else {
      stop("invalid input detected (argument `to_attack)`", call. = FALSE)
    }
  }

  # verbosity
  if (verbose) {
    glpkAPI::termOutGLPK(glpkAPI::GLP_ON)
    glpkAPI::setSimplexParmGLPK("MSG_LEV", glpkAPI::GLP_MSG_ON)
  } else {
    glpkAPI::termOutGLPK(glpkAPI::GLP_OFF)
    glpkAPI::setSimplexParmGLPK("MSG_LEV", glpkAPI::GLP_MSG_OFF)
  }

  prob <- glpkAPI::initProbGLPK()
  glpkAPI::setProbNameGLPK(prob, "attackersProblem")

  # get/compute constraint matrix
  m <- attributes(object@problemInstance)$constraint_matrix
  if (is.null(m)) {
    m <- .gen_contraint_matrix(object)
  }

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

  # bounds by variable
  for (j in seq_len(nr_vars)) {
    if (sdc[j] %in% c("u", "x", "w")) {
      glpkAPI::setColBndGLPK(prob, j = j, type = glpkAPI::GLP_LO, 0, Inf)
    } else {
      glpkAPI::setColBndGLPK(prob, j = j, type = glpkAPI::GLP_FX, ff[j], ff[j])
    }
    glpkAPI::setObjCoefGLPK(prob, j = j, 0)
  }
  for (i in seq_len(nr_constraints)) {
    glpkAPI::setRowBndGLPK(prob, i, type = glpkAPI::GLP_FX, 0, 0)
  }

  nr_cells <- length(to_attack)
  out <- vector("list", nr_cells)
  if (verbose) {
    pb <- progress::progress_bar$new(total = nr_cells)
  }
  for (i in seq_len(nr_cells)) {
    if (verbose) {
      pb$tick(1)
    }
    primsupp_to_attack <- to_attack[i]

    glpkAPI::setObjCoefGLPK(prob, j = primsupp_to_attack, 1)

    # minimize
    glpkAPI::setObjDirGLPK(prob, glpkAPI::GLP_MIN)

    #glpkAPI::writeLPGLPK(prob, paste0("prob-min-", primsupp_to_attack,".txt"))
    glpkAPI::solveSimplexGLPK(prob)
    lower_bnd <- glpkAPI::getObjValGLPK(prob)

    # maximize
    glpkAPI::setObjDirGLPK(prob, glpkAPI::GLP_MAX)
    #glpkAPI::writeLPGLPK(prob, paste0("prob-max-", primsupp_to_attack,".txt"))
    glpkAPI::solveSimplexGLPK(prob)

    # unbounded solution: possible if entire (sub)table is suppressed
    if (glpkAPI::getSolStatGLPK(prob) == glpkAPI::GLP_UNBND) {
      upper_bnd <- max(ff)
    } else {
      upper_bnd <- glpkAPI::getObjValGLPK(prob)
    }

    # reset obj
    glpkAPI::setObjCoefGLPK(prob, j = primsupp_to_attack, 0)
    out[[i]] <-  data.frame(
      "prim_supps" = primsupp_to_attack,
      "status" = sdc[primsupp_to_attack],
      "val" = ff[primsupp_to_attack],
      "low" = lower_bnd,
      "up" = upper_bnd,
      "protected" = upper_bnd > lower_bnd)
  }
  if (verbose) {
    pb$terminate()
  }
  do.call("rbind", out)
}
