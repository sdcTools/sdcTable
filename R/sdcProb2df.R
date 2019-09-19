#' Transform a problem instance
#'
#' [sdcProb2df()] returns a `data.table` given an [sdcProblem-class] input object.
#'
#' @param obj an [sdcProblem-class] object
#' @param addDups (logical), if `TRUE`, duplicated cells are included in the output
#' @param addNumVars (logical), if `TRUE`, numerical variables (if defined in 
#' [makeProblem()] will be included in the output.
#' @param dimCodes (character) allows to specify in which coding the dimensional variables should be returned. Possible choices are:
#' - `"both"`: both original and internally used, standardized codes are included
#'  in the output
#' - `"original"`: only original codes of dimensional variables 
#' are included in the output
#' - `"default"`: only internally used, standardized codes 
#' are included in the output
#' @return a `data.table` containing information about all cells of the given problem
#' @export
#' @md
#' @inherit makeProblem examples 
sdcProb2df <- function(obj, addDups = TRUE, addNumVars = FALSE, dimCodes = "both") {
    if (!rlang::is_scalar_character(dimCodes)) {
    stop("Argument `dimCodes` is not a scalar character.", call. = FALSE)
  }
  if (!dimCodes %in% c("both", "original", "default")) {
    e <- "Argument `dimCodes`  must be either `both`, `original` or `default`."
    stop(e, call. = FALSE)
  }
  
  dt <- g_df(
    object = obj,
    addDups = addDups, 
    addNumVars = addNumVars)
  
  dimV_d <- obj@dimInfo@vNames
  dimV_o <- paste0(obj@dimInfo@vNames, "_o")
  
  if (dimCodes == "original") {
    dt <- dt[,-c(match(dimV_d, names(dt))), with = F]
    cn <- names(dt)
    cn[match(dimV_o, cn)] <- dimV_d
    setnames(dt, cn)
  }
  if (dimCodes == "default") {
    dt <- dt[,-c(match(dimV_o, names(dt))), with = F]
  }
  dt
}
