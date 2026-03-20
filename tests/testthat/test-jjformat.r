context("test jj_format")

test_that("jj-format works", {
  skip_on_cran()
  utils::data("microdata1", package = "sdcTable")

  # create hierarchies
  dimList <- list(
    region = sdcHierarchies::hier_create(
      root = "Total",
      nodes = LETTERS[1:4]
    ),
    gender = sdcHierarchies::hier_create(
      root = "Total",
      nodes = c("female", "male")
    )
  )

  # creating an problem instance
  prob <- makeProblem(
    data = microdata1,
    dimList = dimList,
    numVarInd = "val"
  )

  # check errors
  expect_error(createJJFormat(x = 5))

  # create inputs for jj format
  inp <- createJJFormat(prob)
  expect_identical(digest::digest(inp), "05d866e2cdd0929aa1fffb4c694f3bb2")

  # no numvar
  prob <- makeProblem(
    data = microdata1,
    dimList = dimList
  )
  inp <- createJJFormat(prob)
  expect_identical(digest::digest(inp), "72f5e82733d640a302cc8e55d2d0ec44")
})

