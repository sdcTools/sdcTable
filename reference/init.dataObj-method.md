# initialize `dataObj`-objects

initialize `dataObj`-objects

## Usage

``` r
init.dataObj(input)

# S4 method for class 'list'
init.dataObj(input)
```

## Arguments

- input:

  a list with element described below:

&nbsp;

- element 'inputData': a list object holding data

- element 'dimVarInd': index (within `inputData`) of variables that
  define the table to protect

- element 'freqVarInd': index (within `inputData`) of variable holding
  frequencies

- element 'numVarInd' index (within `inputData`) of numerical variables
  (or NULL)

- element 'weightInd': index (within `inputData`) of variable holding
  weights (or NULL)

- element 'sampWeightInd': index (within `inputData`) of variable
  holding sampling weights (or NULL)

- element 'isMicroData': logical vector of length 1

## Value

an object of class `dataObj`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
