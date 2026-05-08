#' Compute contributing units to table cells
#'
#' This function maps aggregated table cells back to their constituent microdata.
#' It returns the row indices of the raw input data that contribute to specific
#' cells identified by `ids`.
#'
#' @param prob a [sdcProblem-class] object created with [makeProblem()].
#' @param ids a character vector of cell identifiers (`strID`). If `NULL`,
#' indices are computed for all cells in the table. Valid identifiers can
#' be found in the `strID` column of the data frame returned by [sdcProb2df()].
#'
#' @return a named list where each element contains integer indices of the
#' contributing rows in the raw microdata.
#' @export
#' @md
#' @examples
#' # load test problem
#' p <- sdc_testproblem(with_supps = FALSE)
#' dt <- sdcProb2df(p, dimCodes = "original")
#'
#' # find the strID for a specific cell (e.g., region "A" and gender "female")
#' target_id <- dt[region == "A" & gender == "female", strID]
#'
#' # compute contributing indices for this cell
#' contr_list <- contributing_indices(prob = p, ids = target_id)
#'
#' # view the contributing raw data rows
#' rawData <- slot(get.sdcProblem(p, "dataObj"), "rawData")
#' rawData[contr_list[[target_id]]]
#'
#' # compute indices for all cells in the table
#' all_indices <- contributing_indices(p)
contributing_indices <- function(prob, ids = NULL) {
  strID <- NULL
  dt <- sdcProb2df(prob, addDups = FALSE, dimCodes = "original")

  # Validate input and restrict table to requested identifiers
  if (!is.null(ids)) {
    if (!is.character(ids)) {
      stop("Please provide a character vector in argument `ids`.", call. = FALSE)
    }

    # Check for valid identifiers and subset table
    if (!all(data.table::`%chin%`(ids, dt$strID))) {
      stop(paste0(
        "Some values provided in `ids` are not valid. ",
        "See column `strID` in `sdcProb2df()` for valid ids."
      ), call. = FALSE)
    }
    dt <- dt[data.table::`%chin%`(strID, ids)]
  }

  dimvars <- slot(prob, "dimInfo")@vNames

  # Convert micro data to list for faster column access
  dt_inner_list <- as.list(prob@dataObj@rawData)
  n_inner <- length(dt_inner_list[[1]])
  inner_idx <- seq_len(n_inner)

  # Fetch codes that contribute to each hierarchical node
  contr_codes <- .get_all_contributing_codes(prob)

  # Precompute boolean masks for each unique code
  mask_cache <- lapply(dimvars, function(dv) {
    unique_codes <- unique(dt[[dv]])
    masks <- lapply(unique_codes, function(co) {
      node <- contr_codes[[dv]][[co]]
      # Skip filtering for root nodes
      if (node$is_root) {
        return(NULL)
      }
      return(dt_inner_list[[dv]] %in% node$contr_codes)
    })
    names(masks) <- unique_codes
    return(masks)
  })
  names(mask_cache) <- dimvars

  # Mapping of cellls to microdata indices
  res <- lapply(seq_len(nrow(dt)), function(i) {
    # Return empty for cells with no observations
    if (dt$freq[i] == 0) {
      return(integer(0))
    }

    final_mask <- NULL
    for (dv in dimvars) {
      code <- dt[[dv]][i]
      m <- mask_cache[[dv]][[code]]

      if (is.null(m)) {
        next
      }

      if (is.null(final_mask)) {
        # Initialize mask for the first constrained dimension
        final_mask <- m
      } else {
        # Intersect current constraints with previous dimensions
        final_mask <- final_mask & m
      }
    }

    # Return all indices if no dimensional constraints exist
    if (is.null(final_mask)) {
      return(inner_idx)
    }

    # Filter original row positions
    return(inner_idx[final_mask])
  })

  names(res) <- dt$strID
  return(res)
}
