# S4 class describing a dataObj-object

This class models a data object containing the 'raw' data for a given
problem as well as information on the position of the dimensional
variables, the count variable, additional numerical variables, weights
or sampling weights within the raw data. Also slot 'isMicroData' shows
if slow 'rawData' consists of microdata (multiple observations for each
cell are possible, isMicroData==TRUE) or if data have already been
aggregated (isMicroData==FALSE)

## Details

- slot `rawData`::

  list with each element being a vector of either codes of dimensional
  variables, counts, weights that should be used for secondary cell
  suppression problem, numerical variables or sampling weights.

- slot `dimVarInd`::

  numeric vector (or NULL) defining the indices of the dimensional
  variables within slot 'rawData'

- slot `freqVarInd`::

  numeric vector (or NULL) defining the indices of the frequency
  variables within slot 'rawData'

- slot `numVarInd`::

  numeric vector (or NULL) defining the indices of the numerical
  variables within slot 'rawData'

- slot `weightVarInd`::

  numeric vector (or NULL) defining the indices of the variables holding
  weights within slot 'rawData'

- slot `sampWeightInd`::

  numeric vector (or NULL) defining the indices of the variables holding
  sampling weights within slot 'rawData'

- slot `isMicroData`::

  logical vector of length 1 (or NULL) that is TRUE if slot 'rawData'
  are microData and FALSE otherwise

## Note

objects of class `dataObj` are input for slot `dataObj` in class
`sdcProblem`

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
