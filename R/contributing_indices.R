#' Compute contributing units to table cells
#'
#' This function computes (with respect to the raw input data) the indices of all
#' contributing units to given cells identified by `ids`.
#'
#' @param prob a [sdcProblem-class] object created with [makeProblem()]
#' @param ids a character vector containing default ids (strIDs) that define table
#' cells. Valid inputs can be extracted by using [sdcProb2df()] and looking at
#' column `strID`. If this argument is `NULL`, the correspondig units are computed
#' for all cells in the table.
#'
#' @return a named `list where names correspond to the given `ids` and the values
#' to the row numbers within the raw input data.
#' @export
#' @md
#' @examples
#' # loading test data
#' data("microData1", package="sdcTable")
#'
#' # specify hierarchies for `age` and `region`
#' dim_region <- hier_create(root = "Total", nodes = LETTERS[1:4])
#' dim_gender <- hier_create(root = "Total", nodes = c("male", "female"))
#' dl <- list(region = dim_region, gender = dim_gender)
#'
#' # no variables holding counts, numeric values, weights or sampling
#' # weights are available in the input data
#'
#' # using variable names is also possible
#' prob <- makeProblem(
#'   data = microData1,
#'   dimList = dl
#' )
#'
#' df <- sdcProb2df(prob, dimCodes = "original")
#'
#' # which units contribute to cell region = "A" and gender = "female"?
#'
#' # compute the id ("0101")
#' df[region == "A" & gender == "female", strID]
#'
#' # which indices contribute to the cell?
#' ids <- contributing_indices(prob = prob, ids = "0101")
#'
#' # check
#' dataObj <- get.sdcProblem(prob, "dataObj")
#' rawData <- slot(dataObj, "rawData")
#' rawData[ids[["0101"]]]
#'
#' # compute contributing ids for each cell
#' contributing_indices(prob)
#'

# alte hierarchie in dimobj -> neue hierarchie umwandeln
# gibt eine liste mit einem element pro dimension und
# fuer alle codes die entsprechenden beitragenden einheiten
contributing_indices = function(prob, ids = NULL) {
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

  # we compute all unique codes once
  unique_codes <- lapply(dt[, dimvars, with = FALSE], function(x) {
    sort(unique(x))
  })

  # get contributing codes
  contr_codes <- .get_all_contributing_codes(prob)

  # positions in strID
  str_info <- prob@dimInfo@strInfo
  names(str_info) <- dimvars

  # merge inner cell-info to data
  dt_inner <- data.table(id = g_str_id(prob@dimInfo), is_inner = TRUE)
  dt_inner$idx <- 1:nrow(dt_inner)
  dt_inner <- dt[dt_inner, on = "id"]

  dt_inner$tmp <- apply(dt_inner[, dimvars, with = FALSE], 1, paste0, collapse = "")
  setkeyv(dt_inner, "tmp")

  # subsetting dt to those ids, we want to compute the contributing indices from
  dt <- dt[.(ids), on = "id"]

  # prepare output
  res <- vector("list", length(ids))
  names(res) <- ids

  message("computing contributing indices | rawdata <--> table; this might take a while")
  for (i in seq_len(nrow(dt))) {
    strID <- dt$id[i]
    if (dt$freq[i] == 0) {
      res[[strID]] <- integer()
    } else {
      index_vec <- which(dt_inner$id == strID)
      if (length(index_vec) > 0) {
        res[[strID]] <- dt_inner$idx[index_vec]
      } else {
        lev_info <- vector("list", length = nr_dims)
        names(lev_info) <- dimvars
        for (dv in dimvars) {
          code <- dt[[dv]][i]
          info <- contr_codes[[dv]][[code]]
          if (!info$is_root) {
            lev_info[[dv]] <- info$contr_codes
          } else {
            lev_info[[dv]] <- unique_codes[[dv]]
          }
        }
        cell_indices <- pasteStrVec(unlist(expand.grid(lev_info)), nr_dims)
        res[[strID]] <- which(dt_inner$tmp %in% cell_indices)
      }
    }
  }
  return(res)
}
