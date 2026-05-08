# perform calculations on `sdcProblem`-objects depending on argument `type`

perform calculations on `sdcProblem`-objects depending on argument
`type`

## Usage

``` r
calc.sdcProblem(object, type, input)

# S4 method for class 'sdcProblem,character,list'
calc.sdcProblem(object, type, input)
```

## Arguments

- object:

  an object of class `sdcProblem`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- rule.freq: modify suppression status within `object` according to
  frequency suppression rule

- heuristicSolution: obtain a heuristic (greedy) solution to the problem
  defined by `object`

- cutAndBranch: solve a secondary cell suppression problem defined by
  `object` using cut and branch

- anonWorker: is used to solve the suppression problem depending on
  information provided with argument `input`

- ghmiter: solve a secondary cell suppression problem defined by
  `object` using hypercube algorithm

- preprocess: perform a preprocess procedure by trying to identify
  primary suppressed cells that are already protected due to other
  primary suppressed cells

- cellID: find index of cell defined by information provided with
  argument `input`

- finalize: create an object of class `safeObj`

- ghmiter.diagObj: calculate codes required to identify diagonal cells
  given a valid cell code - used for ghmiter-algorithm only

- ghmiter.calcInformation: calculate information for quaders identified
  by diagonal indices - used for ghmiter-algorithm only

- ghmiter.suppressQuader: suppress a quader based on indices

- ghmiter.selectQuader: select a quader for suppression depending on
  information provided with argument `input` - used for
  ghmiter-algorithm only

- ghmiter.suppressAdditionalQuader: select and suppress an additional
  quader (if required) based on information provided with argument
  `input` - used for ghmiter-algorithm only

- contributingIndices: calculate indices within the current problem that
  contribute to a given cell

- reduceProblem: reduce the problem given by `object` using a vector of
  indices

- genStructuralCuts: calculate cuts that are absolute necessary for a
  valid solution of the secondary cell suppression problem

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- a list (typically generated using genParaObj()) specifying parameters
  for primary cell suppression if argument `type` matches 'rule.freq'

- a list if argument `type` matches 'heuristicSolution' having the
  following elements:

  - element 'aProb': an object of class `linProb` defining the
    attacker's problem

  - element 'validCuts': an object of class `cutList` representing a
    list of constraints

  - element 'solver': a character vector of length 1 specifying a solver
    to use

  - element 'verbose': a logical vector of length 1 setting if verbose
    output is desired

- a list (typically generated using genParaObj()) specifying parameters
  for the secondary cell suppression problem if argument `type` matches
  'cutAndBranch', 'anonWorker', 'ghmiter', 'preprocess'

- a list of length 3 if argument `type` matches 'cellID' having
  following elements

  - first element: character vector specifying variable names that need
    to exist in slot 'dimInfo' of `object`

  - second element: character vector specifying codes for each variable
    that define a specific table cell

  - third element: logical vector of length 1 with TRUE setting
    verbosity and FALSE to turn verbose output off

- a list of length 3 if argument `type` matches 'ghmiter.diagObj' having
  following elements

  - first element: numeric vector of length 1

  - second element: a list with as many elements as dimensional
    variables have been specified and each element being a character
    vector of dimension-variable specific codes

  - third element: logical vector of length 1 defining if diagonal
    indices with frequency == 0 should be allowed or not

- a list of length 4 if argument `type` matches
  'ghmiter.calcInformation' having following elements

  - first element: a list object typically generated with method
    `calc.sdcProblem` and type=='ghmiter.diagObj'

  - second element: a list with as many elements as dimensional
    variables have been specified and each element being a character
    vector of dimension-variable specific codes

  - third element: numeric vector of length 1 specifying a desired
    protection level

  - fourth element: logical vector of length 1 defining if quader
    containing empty cells should be allowed or not

- a list of length 1 if argument `type` matches 'ghmiter.suppressQuader'
  having following element

  - first element: numeric vector of indices that should be suppressed

- a list of length 2 if argument `type` matches 'ghmiter.selectQuader'
  having following elements

  - first element: a list object typically generated with method
    `calc.sdcProblem` and type=='ghmiter.calcInformation'

  - second element: a list (typically generated using genParaObj())

- a list of length 4 if argument `type` matches
  'ghmiter.suppressAdditionalQuader' having following elements

  - first element: a list object typically generated with method
    `calc.sdcProblem` and type=='ghmiter.diagObj'

  - second element: a list object typically generated with method
    `calc.sdcProblem` and type=='ghmiter.calcInformation'

  - third element: a list object typically generated with method
    `calc.sdcProblem` and type=='ghmiter.selectQuader'

  - fourth element: a list (typically generated using genParaObj())

- a list of length 1 if argument `type` matches 'contributingIndices'
  having following element

  - first element: character vector of length 1 being an ID for which
    contributing indices should be calculated

- a list of length 1 if argument `type` matches 'reduceProblem' having
  following element

  - first element: numeric vector defining indices of cells that should
    be kept in the reduced problem

- an empty list if argument `type` matches 'genStructuralCuts'

## Value

information from objects of class `sdcProblem` depending on argument
`type`

- an object of class `sdcProblem` if argument `type` matches
  'rule.freq', 'cutAndBranch', 'anonWorker', 'ghmiter',
  'ghmiter.supressQuader', 'ghmiter.suppressAdditionalQuader' or
  'reduceProblem'

- a numeric vector with elements being 0 or 1 if argument `type` matches
  'heuristicSolution'

- a list if argument `type` matches 'preprocess' having following
  elements:

  - element 'sdcProblem': an object of class `sdcProblem`

  - element 'aProb': an object of class `linProb`

  - element 'validCuts': an object of class `cutList`

- a numeric vector of length 1 specifying the index of the cell of
  interest if argument `type` matches 'cellID'

- an object of class `safeObj` if argument `type` matches 'finalize'

- a list if argument `type` matches 'ghmiter.diagObj' having following
  elements:

  - element 'cellToProtect': character vector of length 1 defining the
    ID of the cell to protect

  - element 'indToProtect': numeric vector of length 1 defining the
    index of the cell to protect

  - element 'diagIndices': numeric vector defining indices of possible
    cells defining cubes

- a list containing information about each quader that could possibly be
  suppressed if argument `type` matches 'ghmiter.calcInformation'

- a list containing information about a single quader that should be
  suppressed if argument `type` matches 'ghmiter.selectQuader'

- a numeric vector with indices that contribute to the desired table
  cell if argument `type` matches 'contributingIndices'

- an object of class `cutList` if argument `type` matches
  'genStructuralCuts'

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
