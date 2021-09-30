#' @param objectA maps to argument `x` in [protect_linked_tables()]
#' @param objectB maps to argument `y` in [protect_linked_tables()]
#' @param commonCells maps to argument `common_cells` in [protect_linked_tables()]
#' @param method scalar character vector defining the algorithm
#' that should be used to protect the primary sensitive table cells. In versions `>= 0.32`
#' only the `SIMPLEHEURISTIC` procedure is supported
#' @md
#' @export
#' @rdname protect_linked_tables
protectLinkedTables <- function(objectA, objectB, commonCells, method = "SIMPLEHEURISTIC", ...) {
  .Deprecated(
    new = "protect_linked_tables",
    package = "sdcTable",
    msg = "Please use `protect_linked_tables()` in the future"
  )
  protect_linked_tables(
    x = objectA,
    y = objectB,
    common_cells = commonCells,
    method = method,
    ...
  )
}

