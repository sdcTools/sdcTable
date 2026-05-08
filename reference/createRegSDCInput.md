# Create input for RegSDC/other Tools

This function transforms a
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
object into an object that can be used as input for
[RegSDC::SuppressDec](https://olangsrud.github.io/RegSDC/reference/SuppressDec.html)
(among others).

## Usage

``` r
createRegSDCInput(x, chk = FALSE)
```

## Arguments

- x:

  a
  [sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
  object

- chk:

  a logical value deciding if computed linear relations should be
  additionally checked for validity

## Value

an `list` with the following elements:

- `mat`: linear combinations depending on inner-cells of the given
  problem instance.

- `y`: a 1-column matrix containing the frequencies of inner cells

- `z`: a 1-column matrix containing the frequencies of all cells

- `z_supp`: a 1-column matrix containing the frequencies of all cells
  but suppressed cells have a value of `NA`

- `info`: a `data.frame` with the following columns:

  - `cell_id`: internal cell-id used in sdcTable

  - `is_innercell`: a binary indicator if the cell is an internal cell
    (`TRUE`) or a (sub)total (`FALSE`)

## Author

Bernhard Meindl (bernhard.meindl@gmail.com)

## Examples

``` r
if (FALSE) { # \dontrun{
utils::data("microdata1", package = "sdcTable")
head(microdata1)

# define the problem
dim_region <- hier_create(root = "total", nodes = sort(unique(microdata1$region)))
dim_gender <- hier_create(root = "total", nodes = sort(unique(microdata1$gender)))

prob <- makeProblem(
  data = microdata1,
  dimList = list(region = dim_region, gender = dim_gender),
  freqVarInd = NULL
)

# suppress some cells
prob <- primarySuppression(prob, type = "freq", maxN = 15)

# compute input for RegSDC-package
inp_regsdc <- createRegSDCInput(x = prob, chk = TRUE)

# estimate innner cells based on linear dependencies
res_regsdc <- RegSDC::SuppressDec(
  x = as.matrix(inp_regsdc$x),
  z = inp_regsdc$z_supp,
  y = inp_regsdc$y)[, 1]

# check if inner cells are all protected
df <- data.frame(
  freqs_orig = inp_regsdc$z[inp_regsdc$info$is_innercell == TRUE, ],
  freqs_supp = inp_regsdc$z_supp[inp_regsdc$info$is_innercell == TRUE, ],
  regsdc = res_regsdc
)

subset(df, df$regsdc == df$freqs_orig & is.na(freqs_supp))

} # }
```
