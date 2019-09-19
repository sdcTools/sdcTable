context("test problem generation with sampling weights")

set.seed(10)

sp <- searchpaths()
fn <- file.path(sp[grep("sdcTable", sp)], "data", "microData1.RData")
microData <- get(load(fn))
microData$sampweights <- sample(1:10, nrow(microData), replace = TRUE)

dims <- list(
  region = hier_create(root = "Total", nodes = LETTERS[1:4]), 
  gender = hier_create(root = "Total", nodes = c("male", "female")))

p <- makeProblem(
  data = microData,
  dimList = dims,
  numVarInd = "val",
  sampWeightInd = "sampweights")

expect_is(p, "sdcProblem")
expect_equal(get.problemInstance(p@problemInstance, "nrVars"), 15)

df <- sdcProb2df(p, addDups = TRUE, addNumVars = TRUE, dimCodes = "original")
expect_equal(df$freq[1], sum(microData$sampweights))
expect_equal(df$val[1], sum(microData$sampweights * microData$val))

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
expect_equal(df$freq[1], sum(microData$sampweights))
expect_equal(df$val[1], sum(microData$sampweights * microData$val))

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
expect_equal(df$freq[1], sum(microData$sampweights))
expect_equal(df$val[1], sum(microData$sampweights * microData$val))

