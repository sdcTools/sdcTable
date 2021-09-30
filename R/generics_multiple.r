#' perform calculations on multiple objects depending on argument `type`
#'
#' @param type a character vector of length 1 defining what to calculate|return|modify. Allowed types are:
#' - makePartitions: information on subtables required for HITAS and HYPECUBE algorithms
#' - makeAttackerProblem: set up the attackers problem for a given (sub)table
#' - calcFullProblem: calculate a complete problem object containing all information required to solve
#' the secondary cell suppression problem
#' @param input a list depending on argument `type` with two elements `"objectA"` and `"objectB"`
#' - if type matches 'makePartitions':
#'   * `"object A"`: a `problemInstance` object
#'   * `"object B"`: a `dimInfo` object
#' - if `type` matches 'makeAttackerProblem':
#'   * `"object A"`: a `sdcProblem` object
#'   * `"object B"`: ignored
#' - `type` matches 'calcFullProblem'
#'   * `"object A"`: a `dataObj` object
#'   * `"object B"`: a `dimInfo` object
#' @return manipulated data based on argument `type`
#' - list with elements 'groups', 'indices', 'strIDs', 'nrGroups' and 'nrTables'
#' if argument `type` matches 'makePartitions'
#' - object of class `linProb` if argument `type` matches 'makeAttackerProblem'
#' - object of class `sdcProblem` if argument `type` matches 'calcFullProblem'
#' @md
#' @keywords internal
#' @docType methods
#' @rdname calc.multiple-method
#'
#' @note internal functions/methods
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
setGeneric("calc.multiple", function(type, input) {
  standardGeneric("calc.multiple")
})

setGeneric("c_make_partitions", function(input) {
  standardGeneric("c_make_partitions")
})
setGeneric("c_make_att_prob", function(input) {
  standardGeneric("c_make_att_prob")
})
setGeneric("c_calc_full_prob", function(input) {
  standardGeneric("c_calc_full_prob")
})
