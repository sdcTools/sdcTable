context("test linked-tables")

# Data including two aggregating variables.
df <- data.frame(
  region = c("A", "B", "I", "J", "K", "L", "M", "N"),
  county = c(rep("county-1", 4), rep("county-2", 4)),
  group = c(rep("small", 5), "BIG", "BIG", "small"),
  n = c(2, 2, 8, 8, 3, 3, 3, 7),
  stringsAsFactors = FALSE
)

# defining hierarchies
r1 <- sdcHierarchies::hier_create(root = "Total", nodes = c("county-1", "county-2"))
r1 <- sdcHierarchies::hier_add(r1, root = "county-1", nodes = c("A", "B", "I", "J"))
r1 <- sdcHierarchies::hier_add(r1, root = "county-2", nodes = c("K", "L", "M", "N"))
dl1 <- list(region = r1)

r2 <- sdcHierarchies::hier_create(root = "Total", nodes = c("big", "small"))
r2 <- sdcHierarchies::hier_add(r2, root = "big", nodes = c("L", "M"))
r2 <- sdcHierarchies::hier_add(r2, root = "small", nodes = c("A", "B", "I", "J", "K", "N"))
dl2 <- list(region = r2)

# defining common cells
common_cells <- list(
  region = list("region", c("Total", "A", "B", "I", "J", "K", "L", "M", "N"))[c(1, 1, 2, 2)]
)

# makeProblem and primarySuppression in sdcTable
p1 <- makeProblem(data = df, dimList = dl1, dimVarInd = 1, freqVarInd = 4)
p2 <- makeProblem(data = df, dimList = dl2, dimVarInd = 1, freqVarInd = 4)
p1 <- primarySuppression(p1, type = "freq", maxN = 3)
p2 <- primarySuppression(p2, type = "freq", maxN = 3)

# check that if protected individually, both tables are safe
p1_prot <- protectTable(p1, method = "SIMPLEHEURISTIC")
p2_prot <- protectTable(p2, method = "SIMPLEHEURISTIC")

# individually, these cells are safe and no additional suppressions are required
expect_identical(getInfo(p1_prot, "nrPrimSupps"), 5L)
expect_identical(getInfo(p2_prot, "nrPrimSupps"), 5L)
expect_identical(getInfo(p1_prot, "nrSecondSupps"), 0L)
expect_identical(getInfo(p2_prot, "nrSecondSupps"), 0L)
expect_identical(all(attack(p1_prot)$protected), TRUE)
expect_identical(all(attack(p2_prot)$protected), TRUE)

# however: suppressed cells can be revealed
# by looking at both tables. The difference between "county-1" and "small"
# is two cells, K and N. The difference is 10 and N is 7.
# -> B can be calculated as 10-7=3

# Secondary suppression of linked tables
out <- protect_linked_tables(
  x = p1,
  y = p2,
  common_cells = common_cells,
  doSingletons = TRUE
)

# we check that an additional suppression was found
expect_identical(getInfo(out$x, "nrPrimSupps"), 5L)
expect_identical(getInfo(out$y, "nrPrimSupps"), 5L)
expect_identical(getInfo(out$x, "nrSecondSupps"), 1L)
expect_identical(getInfo(out$y, "nrSecondSupps"), 1L)

region <- sdcStatus <- NULL
fd1 <- getInfo(out$x, "finalData")
fd2 <- getInfo(out$y, "finalData")
expect_equal(fd1[region == "N", sdcStatus], "x")
expect_equal(fd2[region == "N", sdcStatus], "x")
expect_equal(fd1[region == "N", Freq], 7)
expect_equal(fd2[region == "N", Freq], 7)
