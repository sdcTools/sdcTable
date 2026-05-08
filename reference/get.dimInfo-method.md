# query `dimInfo`-objects depending on argument `type`

query `dimInfo`-objects depending on argument `type`

## Usage

``` r
get.dimInfo(object, type)

# S4 method for class 'dimInfo,character'
get.dimInfo(object, type)
```

## Arguments

- object:

  an object of class `dataObj`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- strInfo: info on how many digits in the default codes ach dimensional
  variable allocates

- dimInfo: a list object with each slot containing an object of class
  `dimVar`

- varName: variable names

- strID: character vector of ID's defining table cells

- posIndex vector showing the index of the elements of `dimInfo` in the
  underlying data

## Value

information from objects of class `dimInfo` depending on argument `type`

- a list (or NULL) if argument `type` matches 'strInfo', 'dimInfo'

- numeric vector (or NULL) if argument `type` matches 'posIndex'

- character vector (or NULL) if argument `type` matches 'varName' or
  'strID'

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
