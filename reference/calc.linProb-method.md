# perform calculations on `linProb`-objects depending on argument `type`

perform calculations on `linProb`-objects depending on argument `type`

## Usage

``` r
calc.linProb(object, type, input)

# S4 method for class 'linProb,character,list'
calc.linProb(object, type, input)
```

## Arguments

- object:

  an object of class `linProb`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- solveProblem: solve the linear problem (minimize objective function)

- fixVariables: try to fix objective variables to 0\|1 based on dual
  costs depending on input

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type==solveProblem: a list of length 1

  - first element: character vector of length 1 specifying the solver to
    use.

- type==fixVariables: a list of length 3

  - first element: numeric vector specifying lower bounds for the
    objective variables

  - second element: numeric vector specifying upper bounds for the
    objective variables

  - third element: numeric vector specifying indices of primary
    suppressed cells

## Value

manipulated data based on argument `type`

- list containing the solution and additional information if argument
  `type` matches 'solveProblem

- a numeric vector of indices if argument `type` matches 'fixVariables'

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
