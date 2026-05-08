# initialize `simpleTriplet`-objects depending on argument `type`

init.simpleTriplet should be used to create objects of class
`simpleTriplet`. It is possible to create an object from class
`simpleTriplet` from an existing matrix (using type=='simpleTriplet'). A
positive (or negative) identity matrix stored as an object of class
`simpleTriplet` can be created by specifying type=='simpleTripletDiag'.

## Usage

``` r
init.simpleTriplet(type, input)

# S4 method for class 'character,list'
init.simpleTriplet(type, input)
```

## Arguments

- type:

  a character vector of length 1 defining what\|how to initialize.
  Allowed types are:

&nbsp;

- simpleTriplet: a simple triplet matrix

- simpleTripletDiag: identity matrix

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type == 'simpleTriplet': input is a list of length 1

  - first element: object of class 'matrix'

- type == 'simpleTripletDiag': input is a list of length 2

  - first element: numeric vector of length 1 defining the desired
    number of rows of the identiy matrix

  - second element: logical vector of length 1 being TRUE if a positive
    and FALSE if a negative identity matrix should be returned

## Value

an object of class `simpleTriplet`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
