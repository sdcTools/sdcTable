# argusVersion

returns the version and build number of a given tau-argus executable
specified in argument `exe`.

## Usage

``` r
argusVersion(exe, verbose = FALSE)
```

## Arguments

- exe:

  a path to a tau-argus executable

- verbose:

  (logical) if `TRUE`, the version info and build number of the given
  tau-argus executable will be printed.

## Value

a list with two elements being the tau-argus version and the
build-number.

## Examples

``` r
if (FALSE) { # \dontrun{
argusVersion(exe="C:\\Tau\\TauArgus.exe", verbose=TRUE)
} # }
```
