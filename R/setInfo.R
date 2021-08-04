#' Set/Update information in `sdcProblem` or `problemInstance` objects
#'
#' Function [setInfo()] is used to update values in
#' `sdcProblem` or `problemInstance` objects
#'
#' @param object an object of class `sdcProblem` or `problemInstance`
#' @param type a scalar character specifying the kind of information that
#' should be changed or modified; if `object` inherits class `problemInstance`, the
#' slots are directly changed, otherwise the values within slot `problemInstance`
#' are updated. Valid choices are:
#' - `lb`: lower possible bounds for the cell
#' - `ub`: max. upper bound for the given cell
#' - `LPL`: lower protection level
#' - `SPL`: sliding protection level
#' - `UPL`: upper protection level
#' - `sdcStatus`:  cell-status
#' @param index numeric vector defining cell-indices for which which values in a specified slot should be changed|modified
#' @param input numeric or character vector depending on argument `type` with its length matching the length of argument `index`
#' - character vector if type matches 'sdcStatus'
#' - a numeric vector if type matches 'lb', 'ub', 'LPL', 'SPL' or 'UPL'
#' @return a `sdcProblem`- or `problemInstance` object
#' @md
#' @examples
#' # load example-problem with suppressions
#' # (same as example from ?primarySuppression)
#' p <- sdc_testproblem(with_supps = TRUE)
#'
#' # which is the overall total?
#' idx <- which.max(getInfo(p, "freq")); idx
#'
#' # we see that the cell with idx = 1 is the overall total and its
#' # anonymization state of the total can be extracted as follows:
#' print(getInfo(p, type = "sdcStatus")[idx])
#'
#' # we want this cell to never be suppressed
#' p <- setInfo(p, type = "sdcStatus", index = idx, input = "z")
#'
#' # we can verify this:
#' print(getInfo(p, type = "sdcStatus")[idx])
#'
#' # changing slot 'UPL' for all cells
#' inp <- data.frame(
#'   strID = getInfo(p, "strID"),
#'   UPL_old = getInfo(p, "UPL")
#' )
#' inp$UPL_new <- inp$UPL_old + 1
#' p <- setInfo(p, type = "UPL", index = 1:nrow(inp), input = inp$UPL_new)
#' @rdname setInfo
#' @export
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
setInfo <- function(object, type, index, input) {
  if (!class(object) %in% c("sdcProblem", "problemInstance")) {
    stop("setInfo:: argument `object` must be of class `sdcProblem` or `problemInstance`.", call. = FALSE)
  }

  ok <- c("lb", "ub", "LPL", "SPL", "UPL", "sdcStatus")
  if (!type %in% ok) {
    stop("setInfo:: type must be one of", paste(shQuote(ok), collapse = ", "), call. = FALSE)
  }

  if (class(object) == "sdcProblem") {
    pI <- g_problemInstance(object)
  } else {
    pI <- object
  }

  pI <- set.problemInstance(
    object = pI,
    type = type,
    input = list(index = index, values = input)
  )

  if (class(object) == "sdcProblem") {
    s_problemInstance(object) <- pI
  } else {
    object <- pI
  }
  object
}
