# perform calculations on `problemInstance`-objects depending on argument `type`

perform calculations on `problemInstance`-objects depending on argument
`type`

## Usage

``` r
calc.problemInstance(object, type, input)

# S4 method for class 'problemInstance,character,list'
calc.problemInstance(object, type, input)
```

## Arguments

- object:

  an object of class `problemInstance`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- makeMasterProblem: create the master problem that is the core of the
  secondary cell suppression problem

- isProtectedSolution: check if a solution violates any required
  (upper\|lower\|sliding) protection levels

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type==makeMasterProblem: input is not used (empty list)

- type==isProtectedSolution: input is a list of length 2 with elements
  'input1' and 'input2'

  - element 'input1': numeric vector of calculated known lower cell
    bounds (from attacker's problem)

  - element 'input2': numeric vector of known upper cell bounds (from
    attacker's problem)

## Value

information from objects of class `problemInstance` depending on
argument `type`

- an object of class `linProb` if argument `type` matches
  'makeMasterProblem'

- logical vector of length 1 if argument `type` matches
  'isProtectedSolution' with TRUE if all primary suppressed cells are
  adequately protected, FALSE otherwise

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
