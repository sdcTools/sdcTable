.onAttach <- function(lib, pkg) {
  options(useFancyQuotes = FALSE)
  v <- utils::packageVersion("sdcTable")
  packageStartupMessage("Package sdcTable ", v, " has been loaded!\n")
}
