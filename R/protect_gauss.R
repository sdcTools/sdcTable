.protect_gauss <- function(x, input) {
  verbose <- input$verbose
  removeDuplicated <- input$removeDuplicated
  whenEmptySuppressed	<- input$whenEmptySuppressed
  whenEmptyUnsuppressed	<- input$whenEmptyUnsuppressed
  singletonMethod <- input$singletonMethod

  dt <- sdcProb2df(obj = x, addDups = FALSE, addNumVars = FALSE)
  if (verbose) {
    message("computing constraint matrix")
  }

  sdcinput <- createRegSDCInput(x)
  sdcmat <- sdcinput$x
  innercells <- sdcinput$info$is_innercell
  if (verbose) {
    message("convert constraint matrix into sparse matrix")
  }
  m <-  Matrix::sparseMatrix(
    i = sdcmat$i,
    j = sdcmat$j,
    x = sdcmat$v,
    dimnames = sdcmat$dimnames,
    dims = c(sdcmat$nrow, sdcmat$ncol)
  )

  if (verbose) {
    message("apply SSBtools::GaussSuppression()")
  }
  indices <- SSBtools::GaussSuppression(
    x = m,
    candidates = 1:ncol(m),
    primary = dt$sdcStatus %in% c("u", "x"),
    forced = dt$sdcStatus == "z",
    hidden = dt$sdcStatus == "w",
    singleton = as.logical(sdcinput$y == 1),
    singletonMethod = singletonMethod,
    removeDuplicated = removeDuplicated,
    whenEmptySuppressed	= whenEmptySuppressed,
    whenEmptyUnsuppressed	= whenEmptyUnsuppressed,
    printInc = verbose
  )
  x@problemInstance@sdcStatus[indices] <- "x"
  x
}
