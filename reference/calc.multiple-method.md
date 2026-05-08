# perform calculations on multiple objects depending on argument `type`

perform calculations on multiple objects depending on argument `type`

## Usage

``` r
calc.multiple(type, input)

# S4 method for class 'character,list'
calc.multiple(type, input)
```

## Arguments

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

  - makePartitions: information on subtables required for HITAS and
    HYPECUBE algorithms

  - makeAttackerProblem: set up the attackers problem for a given
    (sub)table

  - calcFullProblem: calculate a complete problem object containing all
    information required to solve the secondary cell suppression problem

- input:

  a list depending on argument `type` with two elements `"objectA"` and
  `"objectB"`

  - if type matches 'makePartitions':

    - `"object A"`: a `problemInstance` object

    - `"object B"`: a `dimInfo` object

  - if `type` matches 'makeAttackerProblem':

    - `"object A"`: a `sdcProblem` object

    - `"object B"`: ignored

  - `type` matches 'calcFullProblem'

    - `"object A"`: a `dataObj` object

    - `"object B"`: a `dimInfo` object

## Value

manipulated data based on argument `type`

- list with elements 'groups', 'indices', 'strIDs', 'nrGroups' and
  'nrTables' if argument `type` matches 'makePartitions'

- object of class `linProb` if argument `type` matches
  'makeAttackerProblem'

- object of class `sdcProblem` if argument `type` matches
  'calcFullProblem'

## Note

internal functions/methods

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
