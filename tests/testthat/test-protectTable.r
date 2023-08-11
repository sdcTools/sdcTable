context("test protectTable()")

test_that("protectTable works", {
  skip_on_cran()
  # create a test-problem without any suppressions
  testprob <- function() {
    utils::data("microdata1", package = "sdcTable")
    p <- makeProblem(
      data = microdata1,
      dimList =  list(
        region = sdcHierarchies::hier_create("total", LETTERS[1:4]),
        gender = sdcHierarchies::hier_create("total", c("male", "female"))
      ),
      numVarInd = "val")
    p
  }

  # create testproblem and mark a single cell as primary suppressed
  p <- testprob()
  p <- change_cellstatus(
    object = p,
    specs = c(region = "A", gender = "female"),
    rule = "u"
  )

  p_opt <- protectTable(object = p, method = "OPT", useC = FALSE, verbose = FALSE)
  p_opt_c <- protectTable(object = p, method = "OPT", useC = TRUE, verbose = FALSE)
  p_hyper <- protectTable(object = p, method = "HYPERCUBE", verbose = FALSE)
  p_hitas <- protectTable(object = p, method = "HITAS", useC = TRUE, verbose = FALSE)
  p_gauss <- protectTable(object = p, method = "GAUSS", verbose = FALSE)

  expect_equivalent(p_opt@results, p_opt_c@results)
  expect_is(p_opt@results, "data.frame")
  expect_equal(sum(p_opt@results$sdcStatus %in% c("s", "z")), 11) # nr_publishable
  expect_equal(sum(p_opt@results$sdcStatus == c("x")), 3) # second_supps

  expect_equal(which(p_opt@results$sdcStatus != "s"), c(5, 6, 11, 12))
  expect_equal(which(p_hyper@results$sdcStatus != "s"), c(5, 6, 11, 12))
  expect_equal(which(p_hitas@results$sdcStatus != "s"), c(5, 6, 11, 12))
  expect_equal(which(p_gauss@results$sdcStatus != "s"), c(5, 6, 14, 15))

  # test SIMPLEHEURISTIC
  p_simple <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE)
  expect_is(p_simple@results, "data.frame")
  expect_equal(which(p_simple@results$sdcStatus != "s"), c(5, 6, 11, 12))

  # create dataset with singletons
  df <- data.frame(
    region = c("a", "b", "b", "c"),
    stringsAsFactors = FALSE
  )
  d_region <- sdcHierarchies::hier_create(root = "tot", nodes = letters[1:4])

  p <- makeProblem(data = df, dimList = list(region = d_region))
  p <- primarySuppression(p, type = "freq", maxN = 1)

  p_simple1 <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE, detectSingletons = FALSE)
  p_simple2 <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE, detectSingletons = TRUE)

  expect_equal(sum(p_simple1@results$sdcStatus == "x"), 0)
  expect_equal(sum(p_simple2@results$sdcStatus == "x"), 1)

  # threshold
  p_simple3 <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE, threshold = 3)
  expect_equal(sum(p_simple3@results$sdcStatus == "x"), 1)

  # due to threshold setting, the entire table needs to be suppressed
  p_simple4 <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE, threshold = 5)
  expect_equal(sum(p_simple4@results$sdcStatus == "x"), 2)

  # create testproblem without suppressions
  p <- testprob()
  expect_is(p, "sdcProblem")
  expect_equal(sum(p@problemInstance@sdcStatus == "s"), 15)

  p_opt <- protectTable(
    object = p,
    method = "OPT",
    useC = TRUE,
    verbose = FALSE
  )
  expect_is(p_opt, "sdcProblem")
  expect_is(p_opt@results, "data.frame")
  expect_equal(sum(p_opt@results$sdcStatus == "s"), 15)
  expect_equal(sum(p_opt@results$sdcStatus != "s"), 0)

  # test gauss-pattern (changes if a cell is set to "z")
  p <- change_cellstatus(
    object = p,
    specs = c(region = "A", gender = "female"),
    rule = "u"
  )
  p <- change_cellstatus(
    object = p,
    specs = c(region = "D", gender = "male"),
    rule = "z"
  )
  p_gauss <- protectTable(object = p, method = "GAUSS", verbose = FALSE)
  expect_equal(which(p_gauss@results$sdcStatus != "s"), c(5, 6, 11, 12))
})
