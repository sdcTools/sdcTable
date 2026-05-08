# S4 class describing a dimVar-object

An object of class `dimVar` holds all necessary information about a
single dimensional variable such as original and standardized codes, the
level-structure, the hierarchical structure, codes that may be
(temporarily) removed from building the complete hierarchy (dups) and
their corresponding codes that correspond to these duplicated codes.

## Details

- slot `codesOriginal`::

  a character vector (or NULL) holding original variable codes

- slot `codesDefault`::

  a character vector (or NULL) holding standardized codes

- slot `codesMinimal`::

  a logical vector (or NULL) defining if a code is required to build the
  complete hierarchy or not (then the code is a (sub)total)

- slot `vName`::

  character vector of length 1 (or NULL) defining the variable name of
  the dimensional variable

- slot `levels`::

  a numeric vector (or NULL) defining the level structure. For each code
  the corresponding level is listed with the grand-total always having
  level==1

- slot `structure`::

  a numeric vector (or NULL) with length of the total number of levels.
  Each element shows how many digits the i-th level allocates within the
  standardized codes (note: level 1 always allocates exactly 1 digit in
  the standardized codes)

- slot `dims`::

  a list (or NULL) defining the hierarchical structure of the
  dimensional variable. Each list-element is a character vector with
  elements available in slot `codesDefault` and the first element always
  being a (sub)total and the remaining elements being the codes that
  contribute to the (sub)total

- slot `dups`::

  character vector (or NULL) having showing original codes that are
  duplicates in the hierarchy and can temporarily removed when building
  a table with this dimensional variable

- slot `dupsUp`::

  character vector (or NULL) with original codes that are the
  corresponding upper-levels to the codes that may be removed because
  they are duplicates and that are listed in slot `dups`

## Note

objects of class `dimVar` form the base for elements in slot `dimInfo`
of class `dimInfo`.

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
