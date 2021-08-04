#' Compute contributing units to table cells
#'
#' This function computes (with respect to the raw input data) the indices of all
#' contributing units to given cells identified by `ids`.
#'
#' @param prob a [sdcProblem-class] object created with [makeProblem()]
#' @param ids a character vector containing default ids (strIDs) that define table
#' cells. Valid inputs can be extracted by using [sdcProb2df()] and looking at
#' column `strID`. If this argument is `NULL`, the corresponding units are computed
#' for all cells in the table.
#'
#' @return a named `list where names correspond to the given `ids` and the values
#' to the row numbers within the raw input data.
#' @export
#' @md
#' @examples
#' # loading test problem
#' p <- sdc_testproblem(with_supps = FALSE)
#' dt <- sdcProb2df(p, dimCodes = "original")
#'
#' # question: which units contribute to cell region = "A" and gender = "female"?
#'
#' # compute the id ("0102")
#' dt[region == "A" & gender == "female", strID]
#'
#' # which indices contribute to the cell?
#' ids <- contributing_indices(prob = p, ids = "0101")
#'
#' # check
#' dataObj <- get.sdcProblem(p, "dataObj")
#' rawData <- slot(dataObj, "rawData")
#' rawData[ids[["0101"]]]
#'
#' # compute contributing ids for all cells
#' contributing_indices(p)
contributing_indices <- function(prob, ids = NULL) {
  . <- NULL
  dt <- sdcProb2df(prob, addDups = FALSE, dimCodes = "original")
  poss_ids <- dt$strID

  if (is.null(ids)) {
    ids <- poss_ids
  } else {
    if (!is.character(ids)) {
      stop("Please provide a character vector in argument `ids`.", call. = FALSE)
    }
    if (!all(ids %in% poss_ids)) {
      e <- c(
        "Some values provided in `ids` are not valid. ",
        "See column `strID` in `sdcProb2df()` for valid ids."
      )
      stop(paste(e, collapse = " "), call. = FALSE)
    }
  }

  dimvars <- slot(prob, "dimInfo")@vNames
  nr_dims <- length(dimvars)

  dt <- dt[, c("strID", "freq", dimvars), with = FALSE]
  data.table::setnames(dt, old = "strID", new = "id")

  # get contributing codes
  contr_codes <- .get_all_contributing_codes(prob)

  # inner cells are raw data!
  dt_inner <- prob@dataObj@rawData
  dt_inner$idx <- 1:nrow(dt_inner)

  # subsetting dt to those ids, we want to compute the contributing indices from
  dt <- dt[.(ids), on = "id"]

  # prepare output
  res <- vector("list", length(ids))
  names(res) <- ids

  message("computing contributing indices | rawdata <--> table; this might take a while")
  # compute contributing ids by dimvar and code
  for (dv in dimvars) {
    ll <- contr_codes[[dv]]
    for (code in names(ll)) {
      if (contr_codes[[dv]][[code]]$is_root) {
        contr_codes[[dv]][[code]]$idx <- rep(TRUE, nrow(dt_inner))
      } else {
        contr_codes[[dv]][[code]]$idx <- data.table::`%chin%`(dt_inner[[dv]], ll[[code]]$contr_codes)
      }
    }
  }
  for (i in seq_len(nrow(dt))) {
    strid <- dt$id[i]
    if (dt$freq[i] == 0) {
      res[[strid]] <- integer()
    } else {
      for (dv in dimvars) {
        code <- dt[[dv]][i]
        if (dv == dimvars[1]) {
          ii <- contr_codes[[dv]][[code]]$idx
        } else {
          if (!contr_codes[[dv]][[code]]$is_root) {
            ii <- ii & contr_codes[[dv]][[code]]$idx
          }
        }
      }
      res[[strid]] <- dt_inner$idx[ii]
    }
  }
  return(res)
}
