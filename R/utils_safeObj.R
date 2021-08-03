#' Query information from protected problem instances
#'
#' [get_safeobj()] allows to extract information from protected [sdcProblem-class]
#' instances.
#'
#' @param object an object of class [sdcProblem-class]
#' @param type a character vector defining what should be returned. Possible
#' choices are:
#' - `"dimInfo`": get infos on dimensional variables that formed the base of the protected data
#' - `"finalData`": return final data object
#' - `"nrNonDuplicatedCells`": total number of cells that are duplicates
#' - `"nrPrimSupps`": total number of primary suppressed cells
#' - `"nrSecondSupps`": total number of secondary cell suppressions
#' - `"nrPublishableCells`": total number of cells that can be published
#' - `"suppMethod`": suppression method that has been used
#' - `"cellInfo`": extract information about a specific cell
#' - `"cellID`": calculate ID of a specific cell defined by level-codes and
#' variable names
#' @param ... additional argument required for choices `"cellInfo"` and `"cellID"`
#' - `"specs"`: a named character vector with names relating to the names of the
#' dimensional variables and values to levels of the hierarchies.
#' - `"complete"`: if `TRUE`, the entire row is returned in `"cellID"`, otherwise
#' only the cell id (number)
#' - `"verbose"`: toggles additional output
#'
#' @return the required information.
#' @docType methods
#' @keywords internal
#' @note internal function
#' @md
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
get_safeobj <- function(object, type, ...) {
  ok <-  c(
    "dimInfo",
    "finalData",
    "nrNonDuplicatedCells",
    "nrPrimSupps",
    "nrSecondSupps",
    "nrPublishableCells",
    "suppMethod",
    "cellInfo",
    "cellID"
  )
  if (!type %in% ok) {
    stop("type must be one of", paste(shQuote(ok), collapse = ", "), call. = FALSE)
  }

  stopifnot(inherits(object, "sdcProblem"))
  results <- slot(object, "results")
  if (is.null(results)) {
    stop("the problem is not yet protected; please run `protectTable()`", call. = FALSE)
  }

  args <- list(...)
  verbose <- args$verbose
  if (is.null(verbose)) {
    verbose <- FALSE
  } else {
    stopifnot(rlang::is_scalar_logical(verbose))
  }


  if (type == "dimInfo") {
    return(object@dimInfo)
  }
  if (type == "finalData") {
    attr(results, "supp_method") <- NULL
    attr(results, "nr_nondup") <- NULL
    attr(results, "index") <- NULL
    return(results)
  }
  if (type == "nrNonDuplicatedCells") {
    return(attributes(results)$nr_nondup)
  }
  if (type == "nrPrimSupps") {
    return(sum(results$sdcStatus == "u"))
  }
  if (type == "nrSecondSupps") {
    return(sum(results$sdcStatus == "x"))
  }
  if (type == "nrPublishableCells") {
    return(sum(results$sdcStatus %in% c("z", "s")))
  }
  if (type == "suppMethod") {
    return(attributes(results)$supp_method)
  }
  if (type == "cellInfo") {
    df <- cell_id(
      x = object,
      specs = args$specs,
      complete = TRUE
    )

    primsupp <- secsupp <- FALSE
    if (df$sdcStatus == "u") {
      primsupp <- TRUE
      if (verbose) {
        message("The cell is primary sensitive.")
      }
    }
    if (df$sdcStatus == "s") {
      if (verbose) {
        message("The cell can be published.")
      }
    }
    if (df$sdcStatus == "z") {
      if (verbose) {
        message("The cell will be enforced for publication.")
      }
    }
    if (df$sdcStatus == "x") {
      secsupp <- TRUE
      if (verbose) {
        message("The cell has been secondary suppressed.")
      }
    }
    cell_id <- df$id
    df$id <- NULL
    return(list(
      cellID = cell_id,
      data = df,
      primSupp = primsupp,
      secondSupp = secsupp
    ))
  }
  if (type == "cellID") {
    return(cell_id(
      x = object,
      specs = args$specs,
      complete = args$complete
    ))
  }
}

# summary-function; will be used in the summary-method for sdcProblem obj
# if slot "results" is not NULL
summarize_safeobj <- function(object, ...) {
  final_data <- get_safeobj(object, "finalData")
  supp_method <- get_safeobj(object, "suppMethod")
  nr_ps <- get_safeobj(object, "nrPrimSupps")
  nr_ss <- get_safeobj(object, "nrSecondSupps")
  nr_pub <- get_safeobj(object, "nrPublishableCells")

  message("\n#####################################")
  message("### Summary of the protected data ###")
  message("#####################################")
  message(paste0("--> The input data have been protected using algorithm ", shQuote(supp_method)))
  message(paste0("--> To protect ", nr_ps, " primary sensitive cells, ", nr_ss," cells were additionally suppressed"))
  message(paste0("--> A total of ", nr_pub," cells may be published"))

  nr_cells <- nrow(final_data)
  nr_nondup <- get_safeobj(object, "nrNonDuplicatedCells")
  nr_rem <- nr_cells - nr_nondup
  if (nr_cells > nr_nondup) {
    message(paste0("--> Duplicated cells: only ", nr_nondup, " table cells are unique, the remaining ", nr_rem," cells are duplicates"))
  }
  message("\n###################################")
  message("### Structure of protected data ###")
  message("###################################")
  print(str(final_data))
  return(invisible(NULL))
}
