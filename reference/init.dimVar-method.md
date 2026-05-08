# initialize `dimVar`-object

initialize `dimVar`-object

## Usage

``` r
init.dimVar(input)

# S4 method for class 'list'
init.dimVar(input)
```

## Arguments

- input:

  a list with 2 elements

&nbsp;

- first element: either an object of class 'matrix' or a data.frame or a
  link to a file. The input data need to be in a specific format (2
  columns) with the first column defining the level-structure and the
  second column defining the level-codes.

- second element: a character vector of length 1 specifying a variable
  name

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
