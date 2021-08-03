context("test problem generation with sampling weights")

set.seed(10)

data("microdata1")
microdata1$sampweights <- sample(1:10, nrow(microdata1), replace = TRUE)

dims <- list(
  region = sdcHierarchies::hier_create(root = "Total", nodes = LETTERS[1:4]),
  gender = sdcHierarchies::hier_create(root = "Total", nodes = c("male", "female")))

p <- makeProblem(
  data = microdata1,
  dimList = dims,
  numVarInd = "val",
  sampWeightInd = "sampweights")

expect_is(p, "sdcProblem")
expect_equal(get.problemInstance(p@problemInstance, "nrVars"), 15)

df <- sdcProb2df(p, addDups = TRUE, addNumVars = TRUE, dimCodes = "original")
expect_equal(df$freq[1], sum(microdata1$sampweights))
expect_equal(df$val[1], sum(microdata1$sampweights * microdata1$val))

# starting from a complete table
df_full <- df[, c("region", "gender", "freq", "val")]
p <- makeProblem(
  data = df_full,
  dimList = dims,
  numVarInd = "val",
  freqVarInd = "freq")
expect_is(p, "sdcProblem")
expect_equal(get.problemInstance(p@problemInstance, "nrVars"), 15)
df <- sdcProb2df(p, addDups = TRUE, addNumVars = TRUE, dimCodes = "original")
expect_equal(df$freq[1], sum(microdata1$sampweights))
expect_equal(df$val[1], sum(microdata1$sampweights * microdata1$val))

# check that sampling weights are ignored in this case
df_full$sampweights <- sample(1:10, nrow(df_full), replace = TRUE)
p <- makeProblem(
  data = df_full,
  dimList = dims,
  numVarInd = "val",
  freqVarInd = "freq",
  sampWeightInd = "sampweights")
expect_is(p, "sdcProblem")
expect_equal(get.problemInstance(p@problemInstance, "nrVars"), 15)
df <- sdcProb2df(p, addDups = TRUE, addNumVars = TRUE, dimCodes = "original")
expect_equal(df$freq[1], sum(microdata1$sampweights))
expect_equal(df$val[1], sum(microdata1$sampweights * microdata1$val))

