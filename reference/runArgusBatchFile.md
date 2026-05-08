# runArgusBatchFile

allows to run batch-files for tau argus given the path to an executable
of argus. The provided batch input files can either be created using
function
[`createArgusInput`](https://sdctools.github.io/sdcTable/reference/createArgusInput.md)
or can be arbitrarily created. In the latter case, argument `obj` should
not be specified and not output is returned, the script is just executed
in tau-argus.

## Usage

``` r
runArgusBatchFile(
  obj = NULL,
  batchF,
  exe = "C:\\Tau\\TauArgus.exe",
  batchDataDir = NULL,
  verbose = FALSE
)
```

## Arguments

- obj:

  `NULL` or an object of class
  [`sdcProblem-class`](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
  that was used to generate the batchfile for argus. If not `NULL`, this
  object is used to create correct variable names. Else, only the output
  from tau-Argus is read and returned as a `data.table`. In this case it
  is possible to run tau-Argus on arbitrarily created batch-files.

- batchF:

  a filepath to an batch-input file created by e.g.
  [`createArgusInput`](https://sdctools.github.io/sdcTable/reference/createArgusInput.md).

- exe:

  (character) file-path to tau-argus executable

- batchDataDir:

  if different from `NULL`, this directory is used to look for
  input-file and writes output files to. This helps to use relative
  paths in batch input files.

- verbose:

  (logical) if `TRUE`, some additional information is printed to the
  prompt

## Value

a `data.table` containing the protected table or an error in case the
batch-file was not solved correctly if the batch-file was created using
sdcTable (argument `obj`) was specified. In case an arbitrarily
batch-file has been run, `NULL` is returned.

## Note

in case a custom batch-file is used as input (e.g `obj` is `NULL`), this
functions does currently not try to read in any tables to the system.
