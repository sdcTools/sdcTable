# modify `dimInfo`-objects depending on argument `type`

modify `dimInfo`-objects depending on argument `type`

## Usage

``` r
set.dimInfo(object, type, input)

# S4 method for class 'dimInfo,character,character'
set.dimInfo(object, type, input)
```

## Arguments

- object:

  an object of class `dimInfo`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- strID: set slot 'strID' of argument `object`

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type==strID: a character vector containing ID's

## Value

an object of class `dimInfo`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
