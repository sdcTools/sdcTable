#' Apply primary suppression
#'
#' [primarySuppression()] identifies and suppresses primary sensitive table cells
#' within [sdcProblem-class] objects based on specified rules.
#'
#' @details Currently, the `frequency` rule, `nk`-dominance rule, `p`-percent rule
#' and `pq`-rule are supported.
#'
#' Note: Since version `0.29`, dominance-based rules (`"p"`, `"pq"`, or `"nk"`)
#' require the identification of the numerical variable by name using the
#' `numVarName` argument. Identification by index is no longer supported.
#'
#' @param object an [sdcProblem-class] object.
#' @param type character string defining the primary suppression rule.
#' Allowed values are:
#'
#' - `"freq"`: apply frequency rule (uses `maxN` and `allowZeros`).
#' - `"nk"`: apply nk-dominance rule (uses `n` and `k`).
#' - `"p"`: apply p-percent rule (uses `p`).
#' - `"pq"`: apply pq-rule (uses `p` and `q`).
#'
#' @param ... parameters for the selected primary suppression rule:
#'
#' - `maxN`: scalar integerish; cells with counts <= `maxN` are suppressed. Defaults to `3`.
#' - `allowZeros`: scalar logical; if `TRUE`, empty cells (frequency `0`) are considered sensitive.
#' Defaults to `FALSE`. Empty cells not flagged as sensitive are marked as `z`
#' (published, but not used for secondary suppression).
#' - `p`: scalar integerish; threshold parameter for `p`-percent or `pq`-rules.
#' Defaults to `80`.
#' - `q`: scalar numeric; threshold parameter for pq-rule. Defaults to `50`.
#' - `n`: scalar integerish; parameter `n` for the nk-dominance rule. Defaults to `2`.
#' - `k`: scalar integerish; parameter `k` for the nk-dominance rule. Defaults to `85`.
#' - `numVarName`: scalar character; the name of the numerical variable used
#' for dominance-based rules. This is mandatory for `nk`, `p`, and `pq` rules.
#'
#' @return an [sdcProblem-class] object.
#' @export
#' @note
#' The ``nk`-dominance, `p`-percent, and `pq`-rules require microdata as input to
#' [makeProblem()].
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
#' @md
#' @examples
#' # load micro data and create a problem instance
#' utils::data("microdata1", package = "sdcTable")
#' p <- sdc_testproblem(with_supps = FALSE)
#'
#' # apply frequency rule
#' p1 <- primarySuppression(
#'   object = p,
#'   type = "freq",
#'   maxN = 2
#' )
#'
#' # apply p-percent rule (requires specifying a numeric variable)
#' p2 <- primarySuppression(
#'   object = p,
#'   type = "p",
#'   p = 30,
#'   numVarName = "val"
#' )
#'
#' # compare results
#' data.frame(
#'   p1_sdc = getInfo(p1, type = "sdcStatus"),
#'   p2_sdc = getInfo(p2, type = "sdcStatus")
#' )
primarySuppression <- function(object, type, ...) {
  if (!type %in% c("nk", "freq", "p", "pq")) {
    stop("Valid types are 'nk', 'freq', 'p', or 'pq'.", call. = FALSE)
  }

  # Extract info from object
  data_obj <- g_dataObj(object)
  dt       <- g_raw_data(data_obj)
  numVarsIndices <- g_numvar_ind(data_obj)

  # Configuration setup
  paraList <- genParaObj(selection = "control.primary", ...)

  pat <- g_suppPattern(object@problemInstance)

  if (type == "freq") {
    object <- c_rule_freq(object, input = paraList)
  } else {
    # Rule-specific logic for nk, p, pq
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
      stop("Variable in `numVarName` not found in data.", call. = FALSE)
    }
    paraList$numVarName <- numVarName
    paraList$numVarInd <- which(numvars == numVarName)
    object <- domRule(object = object, params = paraList, type = type)
  }

  if (!identical(pat, g_suppPattern(object@problemInstance))) {
    object@results <- NULL
  }
  return(object)
}
