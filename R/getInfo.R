#' Retrieve information in `sdcProblem` or `problemInstance` objects
#'
#' Function [getInfo()] is used to extract values from
#' `sdcProblem` or `problemInstance` objects
#'
#' @param object an object of class `sdcProblem` or `problemInstance`
#' @param type a scalar character specifying the information which should be
#' returned. If `object` inherits class `problemInstance`, the
#' slots are directly accessed, otherwise the values within slot `problemInstance`
#' of the `sdcProblem` object are queried. Valid choices are:
#' - the object has not yet been protected
#'   * `lb` and `ub`: current possible lower and upper bounds
#'   * `LPL`, `SPL`, `UPL`: current lower, sliding and upper protection levels
#'   * `sdcStatus`:  current sdc-status of cells
#'   * `freq`: cell frequencies
#'   * `strID`: standardized cell ids (chr)
#'   * `numVars`: `NULL` or a list with a slot for each tabulated numerical variable;
#'   * `w`: sampling weights or `NULL`
#' - the table has already been protected
#'   * `finalData`: protected table as a `data.table`
#'   * `nrNonDuplicatedCells`: number of unique (non-bogus) cells in the table
#'   * `nrPrimSupps`: number of primary sensitive cells that were protected
#'   * `nrSecondSupps`: number of additional secondary suppressions
#'   * `nrPublishableCells`: number of cells (status `"s` or `"z") that may
#'   be published
#'   * `suppMethod`: name of the algorithm used to protect the table
#' @return manipulated data depending on arguments `object` and `type`
#' @md
#' @examples
#' # define an example problem with two hierarchies
#' p <- sdc_testproblem(with_supps = FALSE)
#'
#' # apply primary suppression
#' p <- primarySuppression(p, type = "freq", maxN = 3)
#'
#' # `p` is an `sdcProblem` object
#' print(class(p))
#'
#' for (slot in c("lb", "ub", "LPL", "SPL", "UPL", "sdcStatus",
#'   "freq", "strID", "numVars", "w")) {
#'   message("slot: ", shQuote(slot))
#'   print(getInfo(p, type = slot))
#' }
#'
#' # protect the cell and extract results
#' p_protected <- protectTable(p, method = "SIMPLEHEURISTIC")
#' for (slot in c("finalData", "nrNonDuplicatedCells", "nrPrimSupps",
#'   "nrSecondSupps", "nrPublishableCells", "suppMethod")) {
#'   message("slot: ", shQuote(slot))
#'   print(getInfo(p_protected, type = slot))
#' }
#' @export
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
getInfo <- function(object, type) {
  if (!class(object) %in% c("sdcProblem", "problemInstance", "safeObj")) {
    stop("getInfo:: argument `object` must be of class `sdcProblem` or `problemInstance`!", call. = FALSE)
  }

  if (!is.null(object@results)) {
    ok <- c(
      "finalData",
      "nrNonDuplicatedCells",
      "nrPrimSupps",
      "nrSecondSupps",
      "nrPublishableCells",
      "suppMethod"
    )
    if (!type %in% ok) {
      stop("getInfo:: type must be one of", paste(shQuote(ok), collapse = ", "), call. = FALSE)
    }
    return(get_safeobj(object = object, type = type))
  }
  else {
    ok <- c("lb", "ub", "LPL", "SPL", "UPL", "sdcStatus", "freq", "strID", "numVars", "w")
    if (!type %in% ok) {
      stop("getInfo:: type must be one of", paste(shQuote(ok), collapse = ", "), call. = FALSE)
    }
    if (inherits(object, "sdcProblem")) {
      pI <- g_problemInstance(object)
    } else {
      pI <- object
    }
    return(get.problemInstance(pI, type = type))
  }
}
