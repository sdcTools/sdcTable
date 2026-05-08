# modify `dimVar`-objects depending on argument `type`

modify `dimVar`-objects depending on argument `type`

## Usage

``` r
calc.dimVar(object, type, input)

# S4 method for class 'dimVar,character,character'
calc.dimVar(object, type, input)
```

## Arguments

- object:

  an object of class `dimVar`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- hasDefaultCodes: calculates if a vector of codes (specified by
  argument `input`) corresponds to default codes in `object`

- matchCodeOrig: obtain default\|standard codes for a vector of original
  codes specified by argument `input`

- matchCodeDefault: obtain original codes for a vector of
  default\|standard codes specified by argument `input`

- standardize: perform standardization of level-codes (temporarily
  removing duplicates,..)

- requiredMinimalCodes: calculate a set of minimal codes that are
  required to calculate a specific (sub)total specified by argument
  `input`

&nbsp;

- input:

  a character vector

## Value

information from `object` depending on `type`

- a character vector if type matches 'matchCodeOrig',
  'matchCodeDefault', 'standardize' or 'requiredMinimalCodes'

- a logical vector of length 1 if type matches 'hasDefaultCodes' being
  TRUE if argument `input` are default codes and FALSE otherwise

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
