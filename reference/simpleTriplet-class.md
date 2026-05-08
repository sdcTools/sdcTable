# S4 class describing a simpleTriplet-object

Objects of class `simpleTriplet` define matrices that are stored in a
sparse format. Only the row- and column indices and the corresponding
values of non-zero cells are stored. Additionally, the dimension of the
matrix given by the total number of rows and columns is stored.

## Details

- slot `i`::

  a numeric vector specifying row-indices with each value being geq 1
  and leq of the value in `nrRows`

- slot `j`::

  a numeric vector specifying column-indices with each value being geq 1
  and leq of the value in `nrCols`

- slot `v`::

  a numeric vector specifying the values of the matrix in cells
  specified by the corresponding row- and column indices

- slot `nrRows`::

  a numeric vector of length 1 holding the total number of rows of the
  matrix

- slot `nrCols`::

  a numeric vector of length 1 holding the total number of columns of
  the matrix

## Note

objects of class `simpleTriplet` are input of slot `constraints` in
class
[`linProb-class`](https://sdctools.github.io/sdcTable/reference/linProb-class.md)
and slot slot `con` in class
[`cutList-class`](https://sdctools.github.io/sdcTable/reference/cutList-class.md)

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
