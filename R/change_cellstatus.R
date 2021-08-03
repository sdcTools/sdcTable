#' Change anonymization status of a specific cell
#'
#' Function [changeCellStatus()] allows to change|modify the anonymization state
#' of single table cells for objects of class [sdcProblem-class].
#'
#' @inheritParams cell_info
#' @param rule scalar character vector specifying a valid
#' anonymization code ('u', 'z', 'x', 's') to which all the desired cells
#' under consideration should be set.
#' @param verbose scalar logical value defining verbosity, defaults to `FALSE`
#' @return a [sdcProblem-class] object
#' @md
#' @examples
#' # load example-problem
#' # (same as example from ?makeProblem)
#' p <- testprob_a(with_supps = FALSE)
#'
#' # goal: set cells with region = "D" and gender != "total" as primary sensitive
#'
#' # using a data.frame as input
#' specs <- data.frame(
#'   region = "D",
#'   gender = c("male", "female", "total")
#' )
#'
#' # marking the cells as sensitive
#' p <- change_cellstatus(
#'   object = p,
#'   specs = specs,
#'   rule = "u"
#' )
#'
#' # check
#' cell_info(p, specs = specs)
#'
#' # using a named vector for a single cell to revert
#' # setting D/total as primary-sensitive
#'
#' specs <- c(gender = "total", region = "D")
#'
#' p <- change_cellstatus(
#'   object = p,
#'   specs = specs,
#'   rule = "s"
#' )
#'
#' # and check again
#' cell_info(p, specs = specs)
#' @export
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
change_cellstatus <- function(object, specs, rule, verbose=FALSE, ...) {
  if (!inherits(object, "sdcProblem")) {
    stop("argument `object` is not a `sdcProblem`", call. = FALSE)
  }

  # reset previous solution
  if (!is.null(object@results)) {
    object@results <- NULL
  }

  df <- cell_info(object, specs = specs)
  if (length(rule) == 1) {
   rule <- rep(rule, nrow(df))
  }

  pI <- g_problemInstance(object)
  for (i in seq_len(nrow(df))) {
    cell_id <- df$id[i]
    s_sdcStatus(pI) <- list(index = cell_id, vals = rule[i])
    if (verbose) {
      message("--> the cell with ID=", cell_id, " and Frequency ", g_freq(pI)[cell_id], " has been set to ", rule[i], ".")
    }
  }
  s_problemInstance(object) <- pI
  object
}
