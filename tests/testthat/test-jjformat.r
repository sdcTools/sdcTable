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
  inp_a <- createJJFormat(prob)
  expect_snapshot(inp_a)

  # no numvar
  prob <- makeProblem(
    data = microdata1,
    dimList = dimList
  )
  inp_b <- createJJFormat(prob)
  expect_snapshot(inp_b)
})

