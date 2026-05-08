# Apply primary suppression

`primarySuppression()` identifies and suppresses primary sensitive table
cells within
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
objects based on specified rules.

## Usage

``` r
primarySuppression(object, type, ...)
```

## Arguments

- object:

  an
  [sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
  object.

- type:

  character string defining the primary suppression rule. Allowed values
  are:

  - `"freq"`: apply frequency rule (uses `maxN` and `allowZeros`).

  - `"nk"`: apply nk-dominance rule (uses `n` and `k`).

  - `"p"`: apply p-percent rule (uses `p`).

  - `"pq"`: apply pq-rule (uses `p` and `q`).

- ...:

  parameters for the selected primary suppression rule:

  - `maxN`: scalar integerish; cells with counts \<= `maxN` are
    suppressed. Defaults to `3`.

  - `allowZeros`: scalar logical; if `TRUE`, empty cells (frequency `0`)
    are considered sensitive. Defaults to `FALSE`. Empty cells not
    flagged as sensitive are marked as `z` (published, but not used for
    secondary suppression).

  - `p`: scalar integerish; threshold parameter for `p`-percent or
    `pq`-rules. Defaults to `80`.

  - `q`: scalar numeric; threshold parameter for pq-rule. Defaults to
    `50`.

  - `n`: scalar integerish; parameter `n` for the nk-dominance rule.
    Defaults to `2`.

  - `k`: scalar integerish; parameter `k` for the nk-dominance rule.
    Defaults to `85`.

  - `numVarName`: scalar character; the name of the numerical variable
    used for dominance-based rules. This is mandatory for `nk`, `p`, and
    `pq` rules.

## Value

an
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
object.

## Details

Currently, the `frequency` rule, `nk`-dominance rule, `p`-percent rule
and `pq`-rule are supported.

Note: Since version `0.29`, dominance-based rules (`"p"`, `"pq"`, or
`"nk"`) require the identification of the numerical variable by name
using the `numVarName` argument. Identification by index is no longer
supported.

## Note

The “nk`-dominance, `p\`-percent, and \`pq\`-rules require microdata as
input to
[`makeProblem()`](https://sdctools.github.io/sdcTable/reference/makeProblem.md).

## Cell Status Codes

When an object of class `sdcProblem` is created, every cell is assigned
an internal anonymization state. These codes track the lifecycle of a
cell throughout the suppression process:

- `"s"` (*"Publishable"*): The cell is safe and can be published
  (default).

- `"z"`(*"Forced Publishable"*): The cell must not be suppressed under
  any circumstances.

- `"u"` (\*"Primary Suppressed"): The cell is identified as sensitive
  and requires protection (typically assigned via
  `primarySuppression()`).

- `"x"` (*"Secondary Suppressed"*): The cell is suppressed to protect
  primary sensitive cells (typically assigned via
  [`protectTable()`](https://sdctools.github.io/sdcTable/reference/protectTable.md)).

- `"w"` (*"Extension Cell"*): The cell exists in the object but will
  never be published. It is treated as a suppressed cell by internal
  algorithms. This is useful if it is known in advance that specific
  codes of a hierarchy will be excluded from publication.

Initially in
[`makeProblem()`](https://sdctools.github.io/sdcTable/reference/makeProblem.md),
cells are coded as `"s"` or `"z"`. Their status transitions to `"u"` or
`"x"` as suppression algorithms are applied.

While
[`setInfo()`](https://sdctools.github.io/sdcTable/reference/setInfo.md)
or
[`change_cellstatus()`](https://sdctools.github.io/sdcTable/reference/change_cellstatus.md)
allow for manual state modifications, `primarySuppression()`
automatically identifies primary sensitive cells and assigns state
`"u"`. In
[`protectTable()`](https://sdctools.github.io/sdcTable/reference/protectTable.md),
additional cells are identified and set to `"x"` to ensure all primary
sensitive cells are adequately protected.

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>

## Examples

``` r
# load micro data and create a problem instance
utils::data("microdata1", package = "sdcTable")
p <- sdc_testproblem(with_supps = FALSE)

# apply frequency rule
p1 <- primarySuppression(
  object = p,
  type = "freq",
  maxN = 2
)

# apply p-percent rule (requires specifying a numeric variable)
p2 <- primarySuppression(
  object = p,
  type = "p",
  p = 30,
  numVarName = "val"
)

# compare results
data.frame(
  p1_sdc = getInfo(p1, type = "sdcStatus"),
  p2_sdc = getInfo(p2, type = "sdcStatus")
)
#>    p1_sdc p2_sdc
#> 1       s      s
#> 2       s      s
#> 3       s      s
#> 4       s      s
#> 5       s      s
#> 6       u      u
#> 7       s      s
#> 8       s      s
#> 9       s      s
#> 10      s      s
#> 11      s      s
#> 12      s      s
#> 13      s      s
#> 14      s      u
#> 15      s      s
```
