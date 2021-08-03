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
#' please report it or use Tau-Argus (\url{http://research.cbs.nl/casc/tau.htm}).
#' @param method a character vector of length 1 specifying the algorithm that should be
#' used to protect the primary sensitive table cells. Allowed values are:
#' - `"OPT"`: protect the complete problem at once using a cut and branch algorithm. The optimal
#' algorithm should be used for small problem-instances only.
#' - `"HITAS"`: split the overall problem in smaller problems. These problems are
#' protected using a top-down approach.
#' - `"HYPERCUBE"`: protect the complete problem by protecting sub-tables with a fast
#' heuristic that is based on finding and suppressing geometric structures (n-dimensional cubes)
#' that are required to protect primary sensitive table cells.
#' - `"SIMPLEHEURISTIC"`: heuristic, quick procedure which might be applied to
#' large problem instances
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
#' - parameter(s) used for [protectLinkedTables()]:
#'    * `maxIter`: integerish number specifying the maximal number of interations that should be make
#'    while trying to protect common cells of two different tables. The default value of parameter is `10`
#'
#' - parameters used for the **"SIMPLEHEURISTIC"** procedure:
#'    * `detectSingletons`: logical, should a singleton-detection procedure be run before protecting the
#'    data, defaults to `FALSE`.
#'    * `threshold`: if not `NULL` (the default) an integerish number (> `0`). If specified, a procedure similar
#'    to the singleton-detection procedure is run that makes sure that for all (simple) rows in the table instance that
#'    contains primary sensitive cells the suppressed number of contributors is `>=` the specified threshold.
#' @return an [safeObj-class] object
#' @md
#' @examples
#' # load example-problem with with a single primary suppression
#' # (same as example from ?primarySuppression)
#' p <- testprob_a(with_supps = TRUE)
#'
#' # protect the table using the 'HITAS' algorithm with verbose output
#' p_protected <- protectTable(p, method = "HITAS", verbose = TRUE, useC = TRUE)
#'
#' # showing a summary
#' summary(p_protected)
#'
#' # looking at the final table with result suppression pattern
#' print(getInfo(p_protected, type= "finalData"))
#' @rdname protectTable
#' @export protectTable
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
protectTable <- function(object, method, ...) {
  if (!method %in% c("HITAS", "OPT", "HYPERCUBE", "SIMPLEHEURISTIC")) {
    stop("valid methods are 'SIMPLEHEURISTIC', 'HITAS', 'HYPERCUBE' or 'OPT'!\n")
  }

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
    out <- c_quick_suppression(object, input = paraList)
    out <- out$object
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
