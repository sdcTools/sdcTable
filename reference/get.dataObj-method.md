# query `dataObj`-objects depending on argument `type`

query `dataObj`-objects depending on argument `type`

## Usage

``` r
get.dataObj(object, type)

# S4 method for class 'dataObj,character'
get.dataObj(object, type)
```

## Arguments

- object:

  an object of class `dataObj`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- rawData: raw input data

- dimVarInd: indices of dimensional variables

- freqVarInd: index of frequency variable

- numVarInd: indices of numerical variables

- weightVarInd index of weight variable

- sampWeightInd index of variable holding sampling weights

- isMicroData does `object` consist of microdata?

- numVarNames variable names of numerical variables

- freqVarName variable name of frequency variable

- varName variable names of dimensional variables

## Value

information from objects of class `dataObj` depending on argument `type`

- a list if argument `type` matches 'rawData'

- numeric vector if argument `type` matches 'dimVarInd', 'freqVarInd',
  'numVarInd', 'weightVarInd' or 'sampWeightInd'

- character vector if argument `type` matches 'numVarNames',
  'freqVarName' or 'varName'

- logical vector of length 1 if argument `type` matches 'isMicroData'

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
