context("test protectTable()")

sp <- searchpaths()
fn <- file.path(sp[grep("sdcTable", sp)], "data", "problemWithSupps.RData")

p <- get(load(file = fn))

p_opt <- protectTable(object = p, method = "OPT", useC = FALSE, verbose = FALSE)
p_opt_c <- protectTable(object = p, method = "OPT", useC = TRUE, verbose = FALSE)
p_hyper <- protectTable(object = p, method = "HYPERCUBE", verbose = FALSE)
p_hitas <- protectTable(object = p, method = "HITAS", useC = TRUE, verbose = FALSE)

expect_equivalent(p_opt@finalData, p_opt_c@finalData)
expect_is(p_opt, "safeObj")
expect_equal(p_opt@nrPublishableCells, 11)
expect_equal(p_opt@nrSecondSupps, 3)
expect_equal(which(p_opt@finalData$sdcStatus != "s"), c(5, 6, 11, 12))
expect_equal(which(p_hyper@finalData$sdcStatus != "s"), c(5, 6, 11, 12))
expect_equal(which(p_hitas@finalData$sdcStatus != "s"), c(5, 6, 11, 12))

# test SIMPLEHEURISTIC
p_simple <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE)
expect_is(p_simple, "safeObj")
expect_equal(which(p_simple@finalData$sdcStatus != "s"), c(5, 6, 11, 12))

# create dataset with singletons
df <- data.frame(
  region = c("a", "b", "b", "c"),
  stringsAsFactors = FALSE
)
d_region <- hier_create(root = "tot", nodes = letters[1:4])

p <- makeProblem(data = df, dimList = list(region = d_region))
p <- primarySuppression(p, type = "freq", maxN = 1)

p_simple1 <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE, detectSingletons = FALSE)
p_simple2 <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE, detectSingletons = TRUE)

expect_equal(p_simple1@nrSecondSupps, 0)
expect_equal(p_simple2@nrSecondSupps, 1)

# threshold
p_simple3 <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE, threshold = 3)
expect_equal(p_simple3@nrSecondSupps, 1)

# due to threshold setting, the entire table needs to be suppressed
p_simple4 <- protectTable(object = p, method = "SIMPLEHEURISTIC", verbose = FALSE, threshold = 5)
expect_equal(p_simple4@nrSecondSupps, 2)

rm(p)

# test file with no suppressions
fn <- file.path(sp[grep("sdcTable", sp)], "data", "problem.RData")
p <- get(load(file = fn))

p_opt <- protectTable(
  problem,
  method = "OPT",
  useC = TRUE,
  verbose = FALSE
)
expect_is(p_opt, "safeObj")
expect_equal(p_opt@nrPublishableCells, 15)
expect_equal(p_opt@nrSecondSupps, 0)
expect_equal(sum(p_opt@finalData$sdcStatus != "s"), 0)
