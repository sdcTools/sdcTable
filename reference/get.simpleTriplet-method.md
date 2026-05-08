# query `simpleTriplet`-objects depending on argument `type`

query `simpleTriplet`-objects depending on argument `type`

## Usage

``` r
get.simpleTriplet(object, type, input)

# S4 method for class 'simpleTriplet,character,list'
get.simpleTriplet(object, type, input)
```

## Arguments

- object:

  an object of class `simpleTriplet`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- rowInd: extract all row-indices

- colInd: extract all column-indices

- values: extract all values

- nrRows: return the number of rows of the input object

- nrCols: return the number of columns of the input object

- nrCells: return the number of cells (different from 0!)

- duplicatedRows: return a numeric vector showing indices of duplicated
  rows

- transpose: transpose input `object` and return the transposed matrix

- getRow: return a specific row of input `object`

- getCol: return a specific column of input `object`

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type == 'getRow': input is a list of length 1

  - first element: numeric vector of length 1 defining index of row that
    is to be returned

- type == 'getCol': input is a list of length 1

  - first element: numeric vector of length 1 defining index of column
    that is to be returned

- else: input is not used at all (empty list)

## Value

information from `object` depending on `type`

- a numeric vector if type matches 'rowInd', 'colInd', 'values',
  'nrRows', 'nrCols', 'nrCells' or 'duplicatedRows'

- an object of class `simpleTriplet` if type matches 'transpose',
  'getRow' or 'getCol'

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
