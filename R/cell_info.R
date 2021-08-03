#' Get information about specific cells
#'
#' Function [cellInfo()] can be used to query information of a single cell
#' from a [sdcProblem-class] object. If the instance has already been protected
#' using [protectTable()], the information is retrieved from the final protected
#' dataset, otherwise from the current state of the instance.
#'
#' @param object an object of class [sdcProblem-class]
#' @param specs input that defines which cells to query; the function expects
#' either (see examples below)
#' - a named character vector: with names referring to the names of the dimensional
#' variables and the values to its labels. In this case each vector-element must
#' contain a single value (label)
#' - a `data.frame` where the column-names refer to the names of the dimensional
#' variables and the values to the labels
#' @param ... additional parameters for potential future use, currently unused.
#'
#' @return a `data.frame` with a row for each of the queried cells; the object
#' contains the following columns:
#' - id: numeric vector of length 1 specifying the numerical index of the cell
#' - a column `strID` if `object` has not yet been protected
#' - one column for each dimensional variable
#' - a column `freq` containing the cell-frequencies
#' - if available, one column for each (possible) numerical value that was tabulated
#' - a column `sdcStatus` with the current sdc code
#' - is_primsupp: is `TRUE` if the cell is a primary sensitive cell
#' - is_secondsupp: is `TRUE` if the cell is a secondary suppressed cell
#' @md
#' @examples
#' # as in makeProblem() with a single primary suppression
#' p <- testprob_a(with_supps = TRUE)
#' sdcProb2df(p)
#'
#' # vector input
#' specs_vec <- c(region = "D", gender = "male")
#' cell_info(p, specs = specs_vec)
#'
#' # data.frame input
#' specs_df <- data.frame(
#'   region = c("A", "D", "A"),
#'   gender = c("male", "female", "female")
#' )
#' cell_info(p, specs = specs_df)
#'
#' # protect the table
#' p_safe <- protectTable(p, method = "SIMPLEHEURISTIC")
#'
#' # re-apply
#' cell_info(p_safe, specs = specs_df)
#'
#' @export
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
cell_info <- function(object, specs, ...) {
  is_protected <- !is.null(slot(object, "results"))

  if (inherits(specs, "data.frame")) {
    res <- lapply(seq_len(nrow(specs)), function(x) {
      res <- cell_id(x = object, specs = unlist(specs[x, ]))
    })
    res <- data.table::rbindlist(res)
  } else if (rlang::is_named(specs)) {
    res <- cell_id(x = object, specs = specs)
  } else {
    stop("invalid input detected.", call. = FALSE)
  }

  ids <- res$id
  res$id <- NULL
  dt <- data.table::data.table(id = ids)
  if (!is_protected) {
    strids <- res$strID
    dt$strID <- res$strID
    res$strID <- NULL
  } else {
    dt$strID <- NA_character_
    data.table::setnames(res, old = "Freq", new = "freq")
  }
  res <- cbind(dt, res)

  dv <- g_var_name(object@dataObj)
  nv <- g_numvar_names(object@dataObj)
  data.table::setcolorder(res, c("id", "strID", dv, "freq", nv, "sdcStatus"))

  if (is_protected) {
    res$strID <- NULL
  }
  res$is_primsupp <- res$sdcStatus == "u"
  res$is_secondsupp <- res$sdcStatus == "x"
  return(res)
}
