#' A Problem-Instance used for examples/testing
#'
#' [sdc_testproblem()] returns a sdc-problem instance with `2` hierarchies and
#' optionally with a single suppressed cell that is used in various examples
#' and tests.
#'
#' @param with_supps if `TRUE`, a single cell (violating minimal-frquency rule
#' with `n` = 2) is marked as primary sensitive.
#'
#' @return a problem instance
#' @export
#' @md
#' @examples
#' p1 <- sdc_testproblem(); p1
#' sdcProb2df(p1)
#'
#' # a single protected cell
#' p2 <- sdc_testproblem(with_supps = TRUE); p2
#' sdcProb2df(p2)
#'
#' # cell status differs in one cell
#' specs <- c(gender = "female", region = c("A"))
#' cell_info(p1, specs = specs)
#' cell_info(p2, specs = specs)
sdc_testproblem <- function(with_supps = FALSE) {
  microdata1 <- NULL; rm(microdata1); # fix check-warning
  utils::data(microdata1)
  p <- makeProblem(
    data = microdata1,
    dimList =  list(
      region = sdcHierarchies::hier_create("total", LETTERS[1:4]),
      gender = sdcHierarchies::hier_create("total", c("male", "female"))
    ),
    numVarInd = "val")

  if (with_supps) {
    p <- primarySuppression(p, type = "freq", maxN = 3)
  }
  p
}


problemdata <- function() {
  freq <- NULL
  v1 <- sdcHierarchies::hier_create("tot", c("w", "x", "y", "z"))
  v2 <- sdcHierarchies::hier_create("tot", c("a", "b", "c", "d"))

  dt <- sdcHierarchies::hier_grid(v1, v2)
  dt$freq <- NA_real_
  dt[v1 == "w" & v2 == "d", freq := 89]
  dt[v1 == "w" & v2 == "tot", freq := 230]

  dt[v1 == "x" & v2 == "b", freq := 124]
  dt[v1 == "x" & v2 == "d", freq := 31]
  dt[v1 == "x" & v2 == "tot", freq := 250]

  dt[v1 == "y" & v2 == "a", freq := 92]
  dt[v1 == "y" & v2 == "c", freq := 59]
  dt[v1 == "y" & v2 == "tot", freq := 336]

  dt[v1 == "z" & v2 == "a", freq := 800]
  dt[v1 == "z" & v2 == "c", freq := 651]
  dt[v1 == "z" & v2 == "tot", freq := 3127]

  dt[v1 == "tot" & v2 == "a", freq := 1021]
  dt[v1 == "tot" & v2 == "b", freq := 1262]
  dt[v1 == "tot" & v2 == "c", freq := 770]
  dt[v1 == "tot" & v2 == "d", freq := 890]
  dt[v1 == "tot" & v2 == "tot", freq := 3943]

  # problem due toe constraints, we can compute w|b
  # rows 1+2 = (230 + 250) – (89 + 124 + 31) = 236
  # columns 1+2: (1021 + 770) – (92 + 59 + 800 + 651) = 189
  dt[v1 == "w" & v2 == "b", freq := 47]

  # complete data
  dt[v1 == "w" & v2 == "a", freq := 30]
  dt[v1 == "w" & v2 == "c", freq := 64]

  dt[v1 == "x" & v2 == "a", freq := 90]
  dt[v1 == "x" & v2 == "c", freq := 5]

  dt[v1 == "y" & v2 == "b", freq := 100]
  dt[v1 == "y" & v2 == "d", freq := 85]

  dt[v1 == "z" & v2 == "b", freq := 991]
  dt[v1 == "z" & v2 == "d", freq := 685]

  # create sdcProb
  dt_inner <- dt[v1 != "tot" & v2 != "tot"]
  p <- makeProblem(dt_inner, dimList = list(v1 = v1, v2 = v2), freqVarInd = "freq")

  specs <- data.frame(
    v1 = c(rep("w", 3), rep("x", 2), rep("y", 2), rep("z", 2)),
    v2 = c("a", "b", "c", "a", "c", "b", "d", "b", "d")
  )
  p <- change_cellstatus(
    object = p,
    specs = specs,
    rule = "u"
  )

  # while there are >= 2 supps in each constraint, a cell (w|b)
  # can be exactly recomputed; the following additional suppression
  # would make the table safe; this cell should be identified in
  # `protectTable()`
  #p <- change_cellstatus(sdc, specs = c(v1 = "x", v2 = "b"), rule = "u")
  p
}
