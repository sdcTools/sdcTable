# Attacking primary suppressed cells

Function \[attack()\] is used to compute lower and upper bounds for a
given sdcProblem instance. For all calculations the current suppression
pattern is used when calculating solutions of the attacker's problem.

## Usage

``` r
attack(
  object,
  to_attack = NULL,
  verbose = TRUE,
  threshold = 1e-08,
  n_workers = 1,
  ...
)
```

## Arguments

- object:

  an object of class \`sdcProblem\`

- to_attack:

  if \`NULL\` all current primary suppressed cells are attacked;
  otherwise either an integerish (indices) or character-vector (str-ids)
  of the cells that should be attacked.

- verbose:

  a logical scalar determining if additional output should be displayed

- threshold:

  a numeric scalar defining the tolerance for the protection check. A
  cell is considered protected if the absolute difference between the
  computed upper and lower bound is strictly greater than this
  threshold. Defaults to \`1e-8\`.

- n_workers:

  a scalar positive integer specifying the number of parallel workers to
  use for calculation

- ...:

  placeholder for possible additional input, currently unused;

## Value

a \`data.frame\` with the following columns: - \`sdc\`: the original
sdc-status code - \`freq\`: the original value of the cell - \`id\`:
index of primary suppressed cells - \`low\`: computed lower bound of the
attacker's problem - \`up\`: computed upper bound of the attacker's
problem - \`protected\`: logical, TRUE if the absolute difference
between \`up\` and \`low\` exceeds the defined \`threshold\`

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>

## Examples

``` r
if (FALSE) { # \dontrun{
dims <- list(
  v1 = sdcHierarchies::hier_create("tot", letters[1:4]),
  v2 = sdcHierarchies::hier_create("tot", letters[5:8])
)

N <- 150
df <- data.frame(
  v1 = sample(letters[1:4], N, replace = TRUE),
  v2 = sample(letters[5:8], N, replace = TRUE)
)

sdc <- makeProblem(data = df, dimList = dims)

# set primary suppressions
specs <- data.frame(
  v1 = c("a", "b", "a"),
  v2 = c("e", "e", "f")
)
sdc <- change_cellstatus(sdc, specs = specs, rule = "u")

# attack all primary sensitive cells with a custom threshold
attack(sdc, to_attack = NULL, threshold = 1e-6)

# protect the table and attack again
sdc <- protectTable(sdc, method = "SIMPLEHEURISTIC")
attack(sdc, to_attack = NULL)

# attack only selected cells
attack(sdc, to_attack = c(7, 12))

# Parallel processing (future.apply needs to be available)
attack(sdc, n_workers = 4)
} # }
```
