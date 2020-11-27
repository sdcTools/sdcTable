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
r1 <- hier_create(root = "Total", nodes = c("county-1", "county-2"))
r1 <- hier_add(r1, root = "county-1", nodes = c("A", "B", "I", "J"))
r1 <- hier_add(r1, root = "county-2", nodes = c("K", "L", "M", "N"))
dl1 <- list(region = r1)

r2 <- hier_create(root = "Total", nodes = c("big", "small"))
r2 <- hier_add(r2, root = "big", nodes = c("L", "M"))
r2 <- hier_add(r2, root = "small", nodes = c("A", "B", "I", "J", "K", "N"))
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

# Secondary suppression of linked tables
out <- protectLinkedTables(
  objectA = p1,
  objectB = p2,
  commonCells = common_cells,
  doSingletons = TRUE
)

#  The result is that no secondary suppression is needed. Although the tables are safe
#  individually, suppressed cells can be revealed by looking at both. The difference between
#  "county-1" and "small" is two cells, K and N. The difference is 10 and N is 7.
#  Thus B can be calculated as 10-7 = 3.

# Look at output
res_a <- getInfo(out$outObj1, "finalData")
res_b <- getInfo(out$outObj2, "finalData")

# without any additional secondary suppression, both tables would (individually be safe).
# however: the difference between "county-1" and "small" is two cells, K and N and
# the difference is 10 and N is 7. Thus B could be calculated as 10-7 = 3 if "N" was not suprpessed.
# in suppConstraints() (c++), such cases are identified and additional constraints are added
expect_equal(sum(res_a$sdcStatus == "x"), 1)
expect_equal(subset(res_a, region == "N", select = "sdcStatus")[[1]], "x")
expect_equal(subset(res_b, region == "N", select = "sdcStatus")[[1]], "x")
