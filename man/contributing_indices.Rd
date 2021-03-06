% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contributing_indices.R
\name{contributing_indices}
\alias{contributing_indices}
\title{Compute contributing units to table cells}
\usage{
contributing_indices(prob, ids = NULL)
}
\arguments{
\item{prob}{a \linkS4class{sdcProblem} object created with \code{\link[=makeProblem]{makeProblem()}}}

\item{ids}{a character vector containing default ids (strIDs) that define table
cells. Valid inputs can be extracted by using \code{\link[=sdcProb2df]{sdcProb2df()}} and looking at
column \code{strID}. If this argument is \code{NULL}, the correspondig units are computed
for all cells in the table.}
}
\value{
a named \verb{list where names correspond to the given }ids` and the values
to the row numbers within the raw input data.
}
\description{
This function computes (with respect to the raw input data) the indices of all
contributing units to given cells identified by \code{ids}.
}
\examples{
# loading test data
data("microData1", package="sdcTable")

# specify hierarchies for `age` and `region`
dim_region <- hier_create(root = "Total", nodes = LETTERS[1:4])
dim_gender <- hier_create(root = "Total", nodes = c("male", "female"))
dl <- list(region = dim_region, gender = dim_gender)

# no variables holding counts, numeric values, weights or sampling
# weights are available in the input data

# using variable names is also possible
prob <- makeProblem(
  data = microData1,
  dimList = dl
)

df <- sdcProb2df(prob, dimCodes = "original")

# which units contribute to cell region = "A" and gender = "female"?

# compute the id ("0101")
df[region == "A" & gender == "female", strID]

# which indices contribute to the cell?
ids <- contributing_indices(prob = prob, ids = "0101")

# check
dataObj <- get.sdcProblem(prob, "dataObj")
rawData <- slot(dataObj, "rawData")
rawData[ids[["0101"]]]

# compute contributing ids for each cell
contributing_indices(prob)

}
