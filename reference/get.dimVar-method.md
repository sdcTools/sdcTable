# query `dimVar`-objects depending on argument `type`

query `dimVar`-objects depending on argument `type`

## Usage

``` r
get.dimVar(object, type)

# S4 method for class 'dimVar,character'
get.dimVar(object, type)
```

## Arguments

- object:

  an object of class `dimVar`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- varName: variable name of the variable from which `object` was
  calculated

- codesOriginal: original codes (as specified by the user)

- codesDefault: calculated, default codes

- codesMinimal: all codes required to calculate the complete hierarchy
  (no sub-totals)

- levels: level-structure of the dimensional variable

- structure: vector showing how many digits in the default codes are
  required for each level

- dims: list showing the complete hierarchy of the dimensional variable

- dups: vector of duplicated codes

- dupsUp: vector of codes that are the 'upper' levels to which the codes
  in `dups` correspond

- hasDuplicates: does the dimensional variable has codes that can be
  (temporarily) removed

- nrLevels: the total number of levels of a dimensional variable

- minimalCodesDefault: the standardized codes of the minimal set of
  required level-codes

## Value

information from objects of class `dataObj` depending on argument `type`

- a list if argument `type` matches 'dims'

- numeric vector if argument `type` matches 'levels' or 'nrLevels'

- character vector if argument `type` matches 'codesOriginal',
  'codesDefault', 'vName', 'dups', 'dupsUp' or 'minimalCodesDefault'

- logical vector of length 1 if argument `type` matches 'hasDuplicates'

- a logical vector if argument `type` matches 'codesMinimal'

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
