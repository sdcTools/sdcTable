context("test makeProblem()")

test_that("makeProblem works", {
  skip_on_cran()
  utils::data("microdata1", package = "sdcTable")
  dim.region <- data.frame(
    levels = c("@", "@@", "@@", "@@", "@@"),
    codes = c("Total", "A", "B", "C", "D"),
    stringsAsFactors = FALSE
  )
  dim.gender <- data.frame(
    levels = c("@", "@@", "@@"),
    codes = c("Total", "male", "female"),
    stringsAsFactors = FALSE
  )
  dimList <- list(region = dim.region, gender = dim.gender)
  dimVarInd <- c(1, 2)
  freqVarInd <- numVarInd <- weightInd <- sampWeightInd <- NULL

  problem <- makeProblem(
    data = microdata1,
    dimList = dimList,
    dimVarInd = dimVarInd,
    freqVarInd = freqVarInd,
    numVarInd = numVarInd,
    weightInd = weightInd,
    sampWeightInd = sampWeightInd
  )

  expect_is(problem, "sdcProblem")
  expect_equal(get.problemInstance(problem@problemInstance, "nrVars"), 15)
})
