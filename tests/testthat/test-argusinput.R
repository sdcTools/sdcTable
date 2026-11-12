test_that("holdingvar with integer-valued numeric data works (#344)", {
  skip_on_cran()

  dat <- data.frame(
    IDvar = 1:8,
    NACE = c("A", "A", "B", "B", "A", "B", "A", "B"),
    Region = c("R1", "R1", "R1", "R1", "R2", "R2", "R2", "R2"),
    Profit = c(10, 20, 30, 40, 50, 60, 70, 80)
  )
  dimList <- list(
    NACE = sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")),
    Region = sdcHierarchies::hier_create(root = "Total", nodes = c("R1", "R2"))
  )
  rules <- list(list(type = "freq", n = 2, rg = 20))

  prob <- makeProblem(
    data = dat, dimList = dimList, dimVarInd = c(2, 3), numVarInd = c(1, 4)
  )
  # numeric variables are always stored as doubles in the raw data
  expect_false(is.integer(prob@dataObj@rawData$IDvar))

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  batch_file <- createArgusInput(
    prob, typ = "microdata", method = "MOD", primSuppRules = rules,
    responsevar = "Profit", holdingvar = "IDvar", path = path
  )
  expect_true(file.exists(batch_file))

  meta <- readLines(list.files(path, pattern = "^metadata_.*\\.rda$", full.names = TRUE))
  expect_true(any(grepl("^IDvar ", meta)))
  expect_true(any(grepl("<HOLDING>", meta)))

  data <- readLines(list.files(path, pattern = "^microdata_.*\\.asc$", full.names = TRUE))
  ids <- vapply(strsplit(data, ",", fixed = TRUE), function(x) x[4], character(1))
  expect_equal(sort(as.integer(ids)), 1:8)
})

test_that("holdingvar works for IDs larger than .Machine$integer.max (#302)", {
  skip_on_cran()

  dat <- data.frame(
    IDvar = seq(8552000023159205, length.out = 8),
    NACE = c("A", "A", "B", "B", "A", "B", "A", "B"),
    Region = c("R1", "R1", "R1", "R1", "R2", "R2", "R2", "R2"),
    Profit = c(10, 20, 30, 40, 50, 60, 70, 80)
  )
  dimList <- list(
    NACE = sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")),
    Region = sdcHierarchies::hier_create(root = "Total", nodes = c("R1", "R2"))
  )
  rules <- list(list(type = "freq", n = 2, rg = 20))

  prob <- makeProblem(
    data = dat, dimList = dimList, dimVarInd = c(2, 3), numVarInd = c(1, 4)
  )

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  createArgusInput(
    prob, typ = "microdata", method = "MOD", primSuppRules = rules,
    responsevar = "Profit", holdingvar = "IDvar", path = path
  )

  data <- readLines(list.files(path, pattern = "^microdata_.*\\.asc$", full.names = TRUE))
  expect_true(any(grepl("8552000023159205", data, fixed = TRUE)))
  expect_false(any(grepl("NA", data, fixed = TRUE)))
})

test_that("non-integer holding variables are rejected", {
  skip_on_cran()

  dat <- data.frame(
    IDvar = 1:8 + 0.5,
    NACE = c("A", "A", "B", "B", "A", "B", "A", "B"),
    Region = c("R1", "R1", "R1", "R1", "R2", "R2", "R2", "R2"),
    Profit = c(10, 20, 30, 40, 50, 60, 70, 80)
  )
  dimList <- list(
    NACE = sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")),
    Region = sdcHierarchies::hier_create(root = "Total", nodes = c("R1", "R2"))
  )
  rules <- list(list(type = "freq", n = 2, rg = 20))

  prob <- makeProblem(
    data = dat, dimList = dimList, dimVarInd = c(2, 3), numVarInd = c(1, 4)
  )
  expect_error(
    createArgusInput(
      prob, typ = "microdata", method = "MOD", primSuppRules = rules,
      responsevar = "Profit", holdingvar = "IDvar", path = tempdir()
    ),
    "must contain integer values"
  )

  prob@dataObj@rawData$IDvar[2] <- NA
  expect_error(
    createArgusInput(
      prob, typ = "microdata", method = "MOD", primSuppRules = rules,
      responsevar = "Profit", holdingvar = "IDvar", path = tempdir()
    ),
    "must contain integer values"
  )
})

test_that("warning is issued if holdingvar is combined with sampling weights", {
  skip_on_cran()

  dat <- data.frame(
    IDvar = 1:8,
    NACE = c("A", "A", "B", "B", "A", "B", "A", "B"),
    Region = c("R1", "R1", "R1", "R1", "R2", "R2", "R2", "R2"),
    Profit = c(10, 20, 30, 40, 50, 60, 70, 80),
    sw = c(1, 2, 1, 2, 1, 2, 1, 2)
  )
  dimList <- list(
    NACE = sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")),
    Region = sdcHierarchies::hier_create(root = "Total", nodes = c("R1", "R2"))
  )
  rules <- list(list(type = "freq", n = 2, rg = 20))

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  prob <- makeProblem(
    data = dat, dimList = dimList, dimVarInd = c(2, 3),
    numVarInd = c(1, 4), sampWeightInd = "sw"
  )
  expect_warning(
    createArgusInput(
      prob, typ = "microdata", method = "MOD", primSuppRules = rules,
      responsevar = "Profit", holdingvar = "IDvar", path = path
    ),
    "combined with sampling weights"
  )

  # no warning if no sampling weights are used
  prob <- makeProblem(
    data = dat[, c("IDvar", "NACE", "Region", "Profit")],
    dimList = dimList, dimVarInd = c(2, 3), numVarInd = c(1, 4)
  )
  expect_silent(
    createArgusInput(
      prob, typ = "microdata", method = "MOD", primSuppRules = rules,
      responsevar = "Profit", holdingvar = "IDvar", path = path
    )
  )
})
