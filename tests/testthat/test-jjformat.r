context("test jj_format")

data("microdata1", package = "sdcTable")

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
expect_identical(digest::digest(inp), "accb193e8f09427be7f8ccd1787a9815")

# no numvar
prob <- makeProblem(
  data = microdata1,
  dimList = dimList
)
inp <- createJJFormat(prob)
expect_identical(digest::digest(inp), "dbba270f1edc3cedff83e97f82eeae49")
