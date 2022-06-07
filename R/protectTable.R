#' Protecting [sdcProblem-class] objects
#'
#' Function [protectTable()] is used to protect primary sensitive table cells
#' (that usually have been identified and set using
#' [primarySuppression()]). The function protects primary
#' sensitive table cells according to the method that has been chosen and the
#' parameters that have been set. Additional parameters that are used to control
#' the protection algorithm are set using parameter `...`.
#'
#' The implemented methods may have bugs that yield in not-fully protected tables. Especially
#' the usage of `"OPT"`, `"HITAS"` and `"HYPERCUBE"` in production is not
#' suggested as these methods may eventually be removed completely. In case you encounter any problems,
#' please report it or use Tau-Argus (\url{https://research.cbs.nl/casc/tau.htm}).
#' @param method a character vector of length 1 specifying the algorithm that should be
#' used to protect the primary sensitive table cells. Allowed values are:
#' - `"OPT"`: protect the complete problem at once using a cut and branch algorithm. The optimal
#' algorithm should be used for small problem-instances only.
#' - `"HITAS"`: split the overall problem in smaller problems. These problems are
#' protected using a top-down approach.
#' - `"HYPERCUBE"`: protect the complete problem by protecting sub-tables with a fast
#' heuristic that is based on finding and suppressing geometric structures (n-dimensional cubes)
#' that are required to protect primary sensitive table cells.
#' - `"SIMPLEHEURISTIC"` and `"SIMPLEHEURISTIC_OLD"`: heuristic procedures which might be applied to
#' large(r) problem instances;
#'   * `"SIMPLEHEURISTIC"` is based on constraints; it also solves attacker problems to make sure
#'   each primary sensitive cell cannot be recomputed;
#'   * `"SIMPLEHEURISTIC_OLD"` was the implementation in `sdcTable` versions prior to `0.32`; this
#'   implementation is possibly unsafe but very fast; it is advised to check results using [attack()]
#'   afterwards.
#' @param object a [sdcProblem-class] object that has created using [makeProblem()] and has
#' been modified by [primarySuppression()]
#' @param ... parameters used in the protection algorithm that has been selected. Parameters that
#' can be changed are:
#' - **general parameters**:
#'    * `verbose`: logical scalar (default is `FALSE`) defining if verbose output should be produced
#'    * `save`: logical scalar defining if temporary results should be saved in the current
#'    working directory (`TRUE`) or not (`FALSE`) which is the default value.
#' - parameters used for **"HITAS"** and **"OPT"** algorithms:
#'    * `solver`: character vector of length 1 defining the solver to be used. Currently available choices
#'    are limited to `"glpk"`.
#'    * `timeLimit`: numeric vector of length 1 (or NULL) defining a time limit in minutes after which the
#'    cut and branch algorithm should stop and return a possible non-optimal solution. Parameter `safe` has
#'    a default value of `NULL`
#'    * `maxVars`: a integerish number (or `NULL`) defining the maximum problem size in terms of
#'    decision variables for which an optimization should be tried. If the number of decision variables
#'    in the current problem are larger than parameter `maxVars`, only a possible non-optimal, heuristic solution
#'    is calculated. Parameter `maxVars` has a default value of `NULL` (no restrictions)
#'    * `fastSolution`: logical scalar defining (default `FALSE`) if or if not the cut and branch algorithm will be started or if
#'    the possibly non-optimal heuristic solution is returned independent of parameter `maxVars`.
#'    * `fixVariables`: logical scalar (default `TRUE`) defining whether or not it should be tried to fix some variables
#'    to `0` or `1` based on reduced costs early in the cut and branch algorithm.
#'    * `approxPerc`: integerish scalar that defines a percentage for which a integer solution of the cut and branch
#'    algorithm is accepted as optimal with respect to the upper bound given by the (relaxed) solution of the
#'    master problem. Its default value is set to `10`
#'    * `useC`: logical scalar defining if c++ implementation of the secondary cell
#'    suppression problem should be used, defaults to `FALSE`
#' - parameters used for **"HYPERCUBE"** procedure:
#'    * `protectionLevel`: numeric vector of length 1 specifying the required protectionlevel for the procedure. Its default value is `80`
#'    * `suppMethod`: character vector of length 1 defining the rule on how to select the
#'       'optimal' cube to protect a single sensitive cells. Possible choices are:
#'       + `minSupps`: minimize the number of additional secondary suppressions (this is also the default setting).
#'       + `minSum`: minimize the sum of counts of additional suppressed cells
#'       + `minSumLogs`: minimize the log of the sum of additional suppressed cells
#'    * `suppAdditionalQuader`: logical vector of length 1 specfifying if additional cubes should be
#'    suppressed if any secondary suppressions in the 'optimal' cube are 'singletons'. Parameter
#'    `suppAdditionalQuader` has a default value of `FALSE`
#' - parameter(s) used for [protect_linked_tables()]:
#'    * `maxIter`: integerish number specifying the maximal number of interations that should be make
#'    while trying to protect common cells of two different tables. The default value of parameter is `10`
#'
#' - parameters used for the **"SIMPLEHEURISTIC"** and **"SIMPLEHEURISTIC_OLD"** procedure:
#'    * `detectSingletons`: logical, should a singleton-detection procedure be run before protecting the
#'    data, defaults to `FALSE`.
#'    * `threshold`: if not `NULL` (the default) an integerish number (> `0`). If specified, a procedure similar
#'    to the singleton-detection procedure is run that makes sure that for all (simple) rows in the table instance that
#'    contains primary sensitive cells the suppressed number of contributors is `>=` the specified threshold.
#'
#' - parameters used for the **"GAUSS"** procedure; for details please see `?SSBtools::GaussSuppression` as
#' the default values are the same as in this function:
#'   * `removeDuplicated`: should duplicated columns be removed before running the protection algorithm
#'   * `whenEmptySuppressed`: a function to be called when primary suppressed input is problematic; `NULL`
#'   (default) does not apply any function
#'   * `whenEmptyUnsuppressed`: a function to be called when empty candidate cells aredevto problematic; `NULL`
#'   (default) does not apply any function
#'   * `singletonMethod`: parameter `singletonMethod` in [SSBtools::GaussSuppression()]; default `"anySum`
#' @return an [safeObj-class] object
#' @md
#' @examples
#' # load example-problem with with a single primary suppression
#' # (same as example from ?primarySuppression)
#' p <- sdc_testproblem(with_supps = TRUE)
#'
#' # protect the table using the 'GAUSS' algorithm with verbose output
#' res1 <- protectTable(p, method = "GAUSS", verbose = TRUE)
#' res1
#'
#' # protect the table using the 'HITAS' algorithm with verbose output
#' res2 <- protectTable(p, method = "HITAS", verbose = TRUE, useC = TRUE)
#' res2
#'
#' # protect using the heuristic algorithm
#' res3 <- protectTable(p, method = "SIMPLEHEURISTIC")
#' res3
#'
#' # protect using the old implmentation of the heuristic algorithm
#' # used in sdcTable versions <0.32
#' res4 <- protectTable(p, method = "SIMPLEHEURISTIC_OLD")
#' res4
#'
#' # looking at the final table with result suppression pattern
#' print(getInfo(res1, type = "finalData"))
#' @rdname protectTable
#' @export protectTable
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
protectTable <- function(object, method, ...) {
  paraList <- genParaObj(
    selection = "control.secondary",
    method = method, ...
  )
  supps_u <- length(g_primSupps(object@problemInstance))
  supps_x <- length(g_secondSupps(object@problemInstance))
  if (supps_u + supps_x == 0) {
    return(c_finalize(object = object, input = paraList))
  }

  if (method == "SIMPLEHEURISTIC") {
    out <- .protect_simpleheuristic(object, input = paraList)$object
  } else if (method == "SIMPLEHEURISTIC_OLD") {
    out <- .protect_simpleheuristic_old(object, input = paraList)$object
  } else if (method == "GAUSS") {
    out <- .protect_gauss(object, input = paraList)
  } else {
    if (paraList$useC) {
      if (method == "OPT") {
        out <- c_opt_cpp(object = object, input = paraList)
      }
      if (method == "HITAS") {
        out <- c_hitas_cpp(object = object, input = paraList)
      }
    } else {
      out <- c_anon_worker(object, input = paraList)
    }
  }
  invisible(c_finalize(object = out, input = paraList))
}
