# modify `simpleTriplet`-objects depending on argument `type`

modify `simpleTriplet`-objects depending on argument `type`

## Usage

``` r
calc.simpleTriplet(object, type, input)

# S4 method for class 'simpleTriplet,character,list'
calc.simpleTriplet(object, type, input)
```

## Arguments

- object:

  an object of class `simpleTriplet`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- removeRow: remove a row with given index from `object`

- removeCol: remove a column with given index from `object`

- addRow: add a row to `object`

- addCol: add a column to `object`

- modifyRow: change specified row of `object`

- modifyCol: change specified column of `object`

- modifyCell: change specified cell of `object`

- bind: bind two objects of class `simpleTriplet` together

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type==removeRow: input is a list of length 1

  - first element: numeric vector of length 1 defining the index of the
    row that should be removed

- type==removeCol: input is a list of length 1

  - first element: numeric vector of length 1 defining the index of the
    column that should be removed

- type==addRow: input is a list of length 2

  - first element: numeric vector of column-indices

  - second element: numeric vector defining the cell-values of the row
    that will be added

- type==addCol: input is a list of length 2

  - first element: numeric vector of row-indices

  - second element: numeric vector defining the cell-values of the
    column that will be added

- type==modifyRow: input is a list of length 3

  - first element: numeric vector of length 1 specifying the the
    row-index of the row that will be modified

  - second element: numeric vector specifying the column-indices that
    should be modified

  - third element: numeric vector defining values that should be set in
    the given row

- type==modifyCol: input is a list of length 3

  - first element: numeric vector specifying the row-indices that should
    be modified

  - second element: numeric vector of length 1 specifying the the
    column-index of the column that will be modified

  - third element: numeric vector defining values that should be set in
    the given column

- type==modifyCell: input is a list of length 3

  - first element: numeric vector of length 1 defining the column-index

  - second element: numeric vector of length 1 defining the row-index

  - third element: numeric vector of length 1 holding the value that
    should be set in the given cell

- type==bind: input is a list of length 2

  - first element: an object of class `simpleTriplet`

  - second argument: is a logical vector of length 1 being TRUE if a
    'rbind' or 'FALSE' if a 'cbind' should be done

## Value

an object of class `simpleTriplet`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
