# S4 class describing a safeObj-object

Objects of class `safeObj` are the final result after protection a
tabular structure. After a successful run of
[`protectTable`](https://sdctools.github.io/sdcTable/reference/protectTable.md)
an object of this class is generated and returned. Objects of class
`safeObj` contain a final, complete data set (slot `finalData`) that has
a column showing the anonymization state of each cell and the complete
information on the dimensional variables that have defined the table
that has been protected (slot `dimInfo`). Also, the number of
non-duplicated table cells (slot `nrNonDuplicatedCells`) is returned
along with the number of primary (slot `nrPrimSupps`) and secondary
(slot `nrSecondSupps`) suppressions. Furthermore, the number of cells
that can be published (slot `nrPublishableCells`) and the algorithm that
has been used to protect the data (slot `suppMethod`) is returned.

## Details

- slot `finalData`::

  a data.frame (or NULL) featuring columns for each variable defining
  the table (with their original codes), the cell counts and values of
  any numerical variables and the anonymization status for each cell
  with

  - `s, z`: cell can be published

  - `u`: cell is a primary sensitive cell

  - `x`: cell was selected as a secondary suppression

- slot `dimInfo`::

  an object of class
  [`dimInfo-class`](https://sdctools.github.io/sdcTable/reference/dimInfo-class.md)
  holding all information on variables defining the table

- slot `nrNonDuplicatedCells`::

  numeric vector of length 1 (or NULL) showing the number of
  non-duplicated table cells. This value is different from 0 if any
  dimensional variable features duplicated codes. These codes have been
  re-added to the final dataset.

- slot `nrPrimSupps`::

  numeric vector of length 1 (or NULL) showing the number of primary
  suppressed cells

- slot `nrSecondSupps`::

  numeric vector of length 1 (or NULL) showing the number of secondary
  suppressions

- slot `nrPublishableCells`::

  numeric vector of length 1 (or NULL) showing the number of cells that
  may be published

- slot `suppMethod`::

  character vector of length 1 holding information on the protection
  method

## Note

objects of class `safeObj` are returned after the function
[`protectTable`](https://sdctools.github.io/sdcTable/reference/protectTable.md)
has finished.

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
