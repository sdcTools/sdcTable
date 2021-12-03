#' Apply primary suppression
#'
#' Function [primarySuppression()] is used to identify and suppress primary
#' sensitive table cells in [sdcProblem-class] objects.
#' Argument `type` allows to select a rule that should be used to identify
#' primary sensitive cells. At the moment it is possible to identify and
#' suppress sensitive table cells using the frequency-rule, the nk-dominance
#' rule and the p-percent rule.
#'
#' @details since versions `>= 0.29` it is no longer possible to specify underlying
#' variables for dominance rules (`"p"`, `"pq"` or `"nk"`) by index; these variables must
#' be set by name using argument `numVarName`.
#' @param object a [sdcProblem-class] object
#' @param type character vector of length 1 defining the primary suppression
#' rule. Allowed types are:
#' - `freq`: apply frequency rule with parameters `maxN` and `allowZeros`
#' - `nk`: apply nk-dominance rule with parameters `n`, `k`
#' - `p`: apply p-percent rule with parameter `p`
#' - `pq`: apply pq-rule with parameters `p` and `q`
#' @param ... parameters used in the identification of primary sensitive cells.
#' Parameters that can be modified|changed are:
#' - `maxN`: numeric vector of length 1 used when applying the frequency rule.
#' All cells having counts <= `maxN` are set as primary suppressed. The default
#' value of `maxN` is `3`.
#' - `allowZeros`: logical value defining if empty cells (with frequency = 0)
#' should be considered sensitive when using the frequency rule. Empty cells are
#' never considered as sensitive when applying dominance rules; The default
#' value of `allowZeros` is `FALSE` so that empty cells are not
#' considered primary sensitive by default. Such cells (frequency 0) are then
#' flagged as `z` which indicates such a cell may be published but should (internally)
#' not be used for (secondary) suppression in the heuristic algorithms.
#' - `p`: numeric vector of length 1 specifying parameter `p` that is used
#' when applying the p-percent rule with default value of `80`.
#' - `pq`: numeric vector of length 2 specifying parameters `p` and `q` that
#' are used when applying the pq-rule with the default being c(`25`, `50`).
#' - `n`: numeric vector of length 1 specifying parameter `n` that is used
#' when applying the nk-dominance rule. Parameter `n` is set to `2` by default.
#' - `k`: scalar numeric specifying parameter `k` that is used
#' when applying the nk-dominance rule. Parameter `n` is set to `85` by default.
#' - `numVarName`: character scalar specifying the name
#' of the numerical variable that should be used to identify cells that are
#' dominated by dominance rules (`p-rule`, `pq-rule` or `nk-rule`). This setting
#' is mandatory in package versions `>= 0.29`
#' If `type` is either 'nk', 'p' or 'pq', it is mandatory to
#' specify either `numVarInd` or `numVarName`.
#' - `numVarInd`: same as `numVarName` but a scalar numeric
#' specifying the index of the variable is expected. If both `numVarName`
#' and `numVarInd` are specified, `numVarName` is used. The index refers to the
#' index of the specified numvars in [makeProblem()]. This argument is no longer
#' respected in versions `>= 0.29` where `numVarName` must be used.
#' @return a [sdcProblem-class] object
#' @md
#' @export
#' @note
#' the nk-dominance rule, the p-percent rule and the pq-rule can only
#' be applied if micro data have been used as input data to function [makeProblem()]
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
#' @md
#' @examples
#' # load micro data
#' utils::data("microdata1", package = "sdcTable")
#'
#' # load problem (as it was created in the example in ?makeProblem
#' p <- sdc_testproblem(with_supps = FALSE)
#'
#' # we have a look at the frequency table by gender and region
#' xtabs(rep(1, nrow(microdata1)) ~ gender + region, data = microdata1)
#'
#' # 2 units contribute to cell with region=='A' and gender=='female'
#' # --> this cell is considered sensitive according the the
#' # freq-rule with 'maxN' equal to 2!
#' p1 <- primarySuppression(
#'   object = p,
#'   type = "freq",
#'   maxN = 2
#' )
#'
#' # we can also apply a p-percent rule with parameter "p" being 30 as below.
#' # This is only possible if we are dealing with micro data and we also
#' # have to specify the name of a numeric variable.
#' p2 <- primarySuppression(
#'   object = p,
#'   type = "p",
#'   p = 30,
#'   numVarName = "val"
#' )
#'
#' # looking at anonymization states we see, that one cell is primary
#' # suppressed (sdcStatus == "u")
#' # the remaining cells are possible candidates for secondary cell
#' # suppression (sdcStatus == "s") given the frequency rule with
#' # parameter "maxN = 2".
#' #
#' # Applying the p-percent rule with parameter 'p = 30' resulted in
#' # two primary suppressions.
#' data.frame(
#'   p1_sdc = getInfo(p1, type = "sdcStatus"),
#'   p2_sdc = getInfo(p2, type = "sdcStatus")
#' )
primarySuppression <- function(object, type, ...) {
  if (!type %in% c("nk", "freq", "p", "pq")) {
    stop("valid types are 'nk', 'freq', 'p' or 'pq'!\n")
  }

  data_obj <- g_dataObj(object)
  dt <- g_raw_data(data_obj)

  numVarsIndices <- g_numvar_ind(g_dataObj(object))

  paraList <- genParaObj(
    selection = "control.primary",
    numVarIndices = numVarsIndices, ...
  )

  # starting-pattern
  pat <- g_suppPattern(object@problemInstance)

  if (type == "freq") {
    object <- c_rule_freq(object, input = paraList)
  }

  if (type %in% c("nk", "p", "pq")) {
    pp <- list(...)
    if (is.null(pp$numVarName)) {
      stop("Please specify argument `numVarName`.", call. = FALSE)
    }

    numVarName <- pp$numVarName
    if (!rlang::is_scalar_character(numVarName)) {
      stop("`numVarName` must be a scalar character.")
    }
    numvars <- names(dt)[numVarsIndices]
    if (!numVarName %in% numvars) {
      e <- "Variable specified in `numVarName` does not exist in the data."
      stop(e, call. = FALSE)
    }
    paraList$numVarName <- numVarName
  }
  if (type == "nk") {
    object <- domRule(object = object, params = paraList, type = "nk")
  }

  if (type == "p") {
    object <- domRule(object = object, params = paraList, type = "p")
  }

  if (type == "pq") {
    object <- domRule(object = object, params = paraList, type = "pq")
  }

  # reset previous solution if suppression-pattern has changed
  if (!identical(pat, g_suppPattern(object@problemInstance))) {
    object@results <- NULL
  }
  return(object)
}
