# Documentation: http://research.cbs.nl/casc/Software/TauManualV4.1.pdf

#' Create input files for tauArgus
#'
#' create required input-files and batch-file for tau-argus given an [sdcProblem-class] object
#'
#' @param obj an object of class [sdcProblem-class] from `sdcTable`
#' @param typ (character) either `"microdata"` or `"tabular"`
#' @param verbose (logical) if TRUE, the contents of the batch-file are written to the prompt
#' @param path path, into which (temporary) files will be written to (amongst them being the batch-files).
#' Each file written to this folder belonging to the same problem contains a random id in its filename.
#' @param solver which solver should be used. allowed choices are
#' - `"FREE"`
#' - `"CPLEX"`
#' - `"XPRESS"`
#'
#' In case `"CPLEX"` is used, it is also mandatory to specify argument `licensefile` which needs to be
#' the absolute path the the cplex license file
#' @param method secondary cell suppression algorithm, possible choices include:
#' - `"MOD"`: modular approach. If specified, the following arguments in `...` can additionally be set:
#'   * `MaxTimePerSubtable`: number specifiying max. time (in minutes) spent for each subtable
#'   * `SingleSingle`: 0/1 (default=1)
#'   * `SingleMultiple`: 0/1 (default=1)
#'   * `MinFreq`: 0/1 (default=1)
#' - `"GH"`: hypercube. If specified, the following arguments in `...` can additionally be set:
#'   * `BoundPercentage`: Default percentage to proctect primary suppressed cells, default 75
#'   * `ModelSize`: are we dealing with a small (0) or large (1) model? (default=1)
#'   * `ApplySingleton`: should singletons be additionally protected? 0/1 (default=1)
#' - `"OPT"`: optimal cell suppression. If specified, the following arguments in `...` can additionally be set:
#'   * `MaxComputingTime`: number specifiying max. allowed computing time (in minutes)
#' @param primSuppRules rules for primary suppression, provided as a
#' `list`. For details, please have a look at the examples below.
#' @param responsevar which variable should be tabulated (defaults to frequencies). For details see tau-argus manual section 4.4.4.
#' @param shadowvar if specified, this variable is used to apply the safety rules, defaults to `responsevar`. For details
#' see tau-argus manual section 4.4.4.
#' @param costvar if specified, this variable describes the costs of suppressing each individual cell. For details see tau-argus
#' manual section 4.4.4.
#' @param requestvar if specified, this variable (0/1-coded) contains information about records that request protection.
#' Records with 1 will be protected in case a corresponding request rule matches. It is ignored, if tabular input is used.
#' @param holdingvar if specified, this variable contains information about records that should be grouped together.
#' It is ignored, if tabular input is used.
#' @param ... allows to specify additional parameters for selected suppression-method as described above
#' as well as `licensefile` in clase `"CPLEX"` was specified in argument `solver`.
#' @return the filepath to the batch-file
#' @export
#' @md
#' @examples
#' # loading micro data from sdcTable
#' data("microData1", package="sdcTable")
#' microData1$num1 <- rnorm(mean=100, sd=25, nrow(microData1))
#' microData1$num2 <- round(rnorm(mean=500, sd=125, nrow(microData1)),2)
#' microData1$weight <- sample(10:100, nrow(microData1), replace=TRUE)
#'
#' dim_region <- hier_create(root = "Total", nodes = LETTERS[1:4])
#'
#' dim_region_dupl <- hier_create(root = "Total", nodes = LETTERS[1:4])
#' dim_region_dupl <- hier_add(dim_region_dupl, root = "B", nodes = c("b1"))
#' dim_region_dupl <- hier_add(dim_region_dupl, root = "D", nodes = c("d1"))
#'
#' dim_gender <- hier_create(root = "Total", nodes = c("male", "female"))
#'
#' dimList <- list(region = dim_region, gender = dim_gender)
#' dimList_dupl  <- list(region = dim_region_dupl, gender = dim_gender)
#' dimVarInd <- 1:2
#' numVarInd <- 3:5
#' sampWeightInd <- 6
#'
#' # creating an object of class \code{\link{sdcProblem-class}}
#' obj <- makeProblem(
#'   data = microData1,
#'   dimList = dimList,
#'   dimVarInd = dimVarInd,
#'   numVarInd = numVarInd,
#'   sampWeightInd = sampWeightInd)
#'
#' # creating an object of class \code{\link{sdcProblem-class}} containing "duplicated" codes
#' obj_dupl <- makeProblem(
#'   data = microData1,
#'   dimList = dimList_dupl,
#'   dimVarInd = dimVarInd,
#'   numVarInd = numVarInd,
#'   sampWeightInd = sampWeightInd)
#'
#' ## create primary suppression rules
#' primSuppRules <- list()
#' primSuppRules[[1]] <- list(type = "freq", n = 5, rg = 20)
#' primSuppRules[[2]] <- list(type = "p", n = 5, p = 20)
#' # other supported formats are:
#' # list(type = "nk", n=5, k=20)
#' # list(type = "zero", rg = 5)
#' # list(type = "mis", val = 1)
#' # list(type = "wgt", val = 1)
#' # list(type = "man", val = 20)
#'
#' ## create batchInput object
#' bO_md1 <- createArgusInput(
#'   obj = obj,
#'   typ = "microdata",
#'   path = tempdir(),
#'   solver = "FREE",
#'   method = "OPT",
#'   primSuppRules = primSuppRules,
#'   responsevar = "num1")
#'
#' bO_td1 <- createArgusInput(
#'   obj = obj,
#'   typ = "tabular",
#'   path = tempdir(),
#'   solver = "FREE",
#'   method = "OPT")
#'
#' bO_td2 <- createArgusInput(
#'   obj = obj_dupl,
#'   typ = "tabular",
#'   path = tempdir(),
#'   solver = "FREE",
#'   method = "OPT")
#'
#' \dontrun{
#' ## in case CPLEX should be used, it is required to specify argument licensefile
#' bO_md2 <- createArgusInput(
#'   obj = obj,
#'   typ = "microdata",
#'   path = tempdir(),
#'   solver = "CPLEX",
#'   method = "OPT",
#'   primSuppRules = primSuppRules,
#'   responsevar = "num1",
#'   licensefile = "/path/to/my/cplexlicense")
#' }
createArgusInput <- function(
  obj, typ="microdata", verbose=FALSE, path=getwd(), solver="FREE", method,
  primSuppRules=NULL, responsevar=NULL, shadowvar=NULL, costvar=NULL,
  requestvar=NULL, holdingvar=NULL, ...) {

  if (class(obj) != "sdcProblem") {
    stop("argument 'obj' must be of class 'sdcProblem'.\n")
  }
  if (!typ %in% c("microdata", "tabular")) {
    stop("argument 'type' must be either 'microdata' or 'tabular'.\n")
  }

  args <- list(...)
  if (solver == "CPLEX" & is.null(args$licensefile)) {
    e <- c(
      dQuote("CPLEX"), "should be used, but",
      shQuote("licensefile"), "was not set."
    )
    stop(paste(e, collapse = " "), call. = FALSE)
  }

  if (typ == "microdata") {
    if (is.null(primSuppRules)) {
      e <- c(
        "primary suppression rules (argument 'primSuppRules') must be",
        "specified when using microdata as input!"
      )
      stop(paste(e, collapse = " "), call. = FALSE)
    }
    batchObj <- tauBatchInput_microdata(
      obj = obj,
      verbose = verbose,
      path = path,
      solver = solver,
      method = method,
      primSuppRules = primSuppRules,
      responsevar = responsevar,
      shadowvar = shadowvar,
      costvar = costvar,
      requestvar = requestvar,
      holdingvar = holdingvar, ...
    )
  }
  if (typ == "tabular") {
    if (!is.null(primSuppRules)) {
      message("ignoring argument 'primSuppRules'")
      primSuppRules <- NULL
    }
    if (!is.null(requestvar)) {
      message("ignoring argument 'requestvar'")
      requestvar <- NULL
    }
    if (!is.null(holdingvar)) {
      message("ignoring argument 'holdingvar'")
      holdingvar <- NULL
    }

    batchObj <- tauBatchInput_table(
      obj = obj,
      verbose = verbose,
      path = path,
      solver = solver,
      method = method,
      responsevar = responsevar,
      shadowvar = shadowvar,
      costvar = costvar, ...
    )
  }
  ## write required files
  batchF <- writeBatchFile(batchObj)
  batchF <- normalizePath(batchF, winslash = "/", mustWork = TRUE)
  if (verbose) {
    ff <- paste("The batch-file", dQuote(batchF), "has the following content:")
    cat(paste0("\n", ff, "\n"))
    cat(paste(rep("-", nchar(ff)), collapse = ""), "\n")
    rr <- readLines(batchF, warn = FALSE)[-c(1:3)]
    cat(rr, sep = "\n")
  }
  return(invisible(batchF))
}
