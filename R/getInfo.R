#' query information from objects
#'
#' Function \code{\link{getInfo}} is used to query information from objects of class
#' \code{\link{sdcProblem-class}}, \code{\link{problemInstance-class}} or \code{\link{safeObj-class}}
#'
#' @param object a \code{\link{sdcProblem-class}} object, \code{\link{problemInstance-class}} object or \code{\link{safeObj-class}} object.
#' @param type a character vector of length 1 specifying the information which should be returned.
#' \itemize{
#' \item if argument \code{object} is of class \code{sdcProblem-class} or \code{\link{problemInstance-class}}, valid choices are:
#' \itemize{
#' \item \code{lb}: slot 'lb' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}}
#' \item \code{ub}: slot 'ub' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}}
#' \item \code{LPL}: slot 'LPL' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}}
#' \item \code{SPL}: slot 'SPL' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}}
#' \item \code{UPL}: slot 'UPL' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}}
#' \item \code{sdcStatus}:  slot 'sdcStatus' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}}
#' \item \code{freq}: slot 'freq' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}}
#' \item \code{strID}: slot 'strID' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}}
#' \item \code{numVars}: slot 'numVars' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}}
#' \item \code{w}: slot 'w' of input \code{object} if it is of class \code{\link{problemInstance-class}} or this slot within slot 'problemInstance' if \code{object} is of class \code{\link{sdcProblem-class}} }
#' \item if argument \code{object} is of class \code{\link{safeObj-class}}, valid choices are:
#' \itemize{
#' \item \code{finalData}: slot 'finalData' of input \code{object} of class \code{\link{safeObj-class}}
#' \item \code{nrNonDuplicatedCells}: slot 'nrNonDuplicatedCells' of input \code{object} of class \code{\link{safeObj-class}}
#' \item \code{nrPrimSupps}: slot 'nrPrimSupps' of input \code{object} of class \code{\link{safeObj-class}}
#' \item \code{nrSecondSupps}: slot 'nrSecondSupps' of input \code{object} of class \code{\link{safeObj-class}}
#' \item \code{nrPublishableCells}: slot 'nrPublishableCells' of input \code{object} of class \code{\link{safeObj-class}}
#' \item \code{suppMethod}: slot 'suppMethod' of input \code{object} of class \code{\link{safeObj-class}}}
#' }
#'
#' @return manipulated data dependend on arguments \code{object} and \code{type}
#'
#' @examples
#' # define an example problem with two hierarchies
#' p <- testprob_a(with_supps = FALSE)
#'
#' # apply primary suppression
#' p <- primarySuppression(p, type = "freq", maxN = 3)
#'
#' # `p` is an object of class \code{\link{sdcProblem-class}}
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
#' @rdname getInfo
#' @export getInfo
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
    if (class(object) == "sdcProblem") {
      pI <- g_problemInstance(object)
    } else {
      pI <- object
    }
    return(get.problemInstance(pI, type = type))
  }
}
