#' Create input for RegSDC/other Tools
#'
#' This function transforms a [sdcProblem-class] object into an object
#' that can be used as input for [RegSDC::SuppressDec] (among others).
#'
#' @param x a [sdcProblem-class] object
#' @param chk a logical value deciding if computed linear relations should
#' be additionally checked for validity
#' @return an `list` with the following elements:
#' - `mat`: linear combinations depending on inner-cells of the given
#' problem instance.
#' - `y`: a 1-column matrix containing the frequencies of inner cells
#' - `z`: a 1-column matrix containing the frequencies of all cells
#' - `z_supp`: a 1-column matrix containing the frequencies of all cells
#' but suppressed cells have a value of `NA`
#' - `info`: a `data.frame` with the following columns:
#'   * `cell_id`: internal cell-id used in sdcTable
#'   * `is_innercell`: a binary indicator if the cell is an internal cell
#'   (`TRUE`) or a (sub)total (`FALSE`)
#' @author Bernhard Meindl (bernhard.meindl@@gmail.com)
#' @export
#' @md
#' @examples
#' \dontrun{
#' data("microData1", package = "sdcTable")
#' head(microData1)
#'
#' # define the problem
#' dim_region <- hier_create(root = "total", nodes = sort(unique(microData1$region)))
#' dim_gender <- hier_create(root = "total", nodes = sort(unique(microData1$gender)))
#'
#' prob <- makeProblem(
#'   data = microData1,
#'   dimList = list(region = dim_region, gender = dim_gender),
#'   freqVarInd = NULL
#' )
#'
#' # suppress some cells
#' prob <- primarySuppression(prob, type = "freq", maxN = 15)
#'
#' # compute input for RegSDC-package
#' inp_regsdc <- createRegSDCInput(x = prob, chk = TRUE)
#'
#' # estimate innner cells based on linear dependencies
#' res_regsdc <- RegSDC::SuppressDec(
#'   x = as.matrix(inp_regsdc$x),
#'   z = inp_regsdc$z_supp,
#'   y = inp_regsdc$y)[, 1]
#'
#' check if inner cells are all protected
#' df <- data.frame(
#'   freqs_orig = inp_regsdc$z[inp_regsdc$info$is_innercell == TRUE, ],
#'   freqs_supp = inp_regsdc$z_supp[inp_regsdc$info$is_innercell == TRUE, ],
#'   regsdc = sdc
#' )
#'
# cells where regsdc estimates identical cell value as `freqs`
# and `freqs_supps` is `NA` (a suppressed cell) can be recomputed
# and are not protected;
#' subset(df, df$regsdc == df$freqs_orig & is.na(freqs_supp))
#'
# --> the primary-suppression pattern in this case in unsafe!
#' }
createRegSDCInput <- function(x, chk = FALSE) {
  stopifnot(inherits(x, "sdcProblem"))
  stopifnot(rlang::is_scalar_logical(chk))

  # extracts the "problemInstance"
  pi <- get.sdcProblem(x, type = "problemInstance")
  nr_cells <- get.problemInstance(pi, "nrVars")

  # number of linear dependencies
  # all deps as sparse matrix
  st <- c_gen_mat_m(
    input = list(
      objectA = pi,
      objectB = get.sdcProblem(x, type = "dimInfo")
    )
  )

  # 1: identify cells that are sub-total and inner-cells
  ids_subtots <- sort(unique(st@j[st@v == -1]))
  ids_innercells <- setdiff(1:nr_cells, ids_subtots)

  # 2: create matrix with correct format
  # we create the diagonal matrix and remove rows later
  mat <- slam::simple_triplet_diag_matrix(v = 1, nrow = nr_cells)
  rownames(mat) <- colnames(mat) <- paste0("cell_", 1:nr_cells)
  mat <- mat[rownames(mat) %in% paste0("cell_", ids_innercells), ]

  # 3: add linear deps for subtotals
  # st: the simple triplet containing all relations
  # id: the id for which the linear-deps should be returned
  .get_rel <- function(st, id) {
    ii <- slot(st, "j") == id & slot(st, "v") == -1
    row <- slot(st, "i")[ii]
    slot(st, "j")[slot(st, "i") == row & slot(st, "v") != -1]
  }

  xx <- lapply(ids_subtots, function(x) {
    if (x == 1) {
      deps <- ids_innercells
    } else {
      deps <- .get_rel(st = st, id = x)
    }
    rr <- paste0("cell_", deps)
    #mat[rownames(mat) %in% rr, x] <<- 1
    mat[which(rownames(mat) %in% rr), x] <<- 1
  }); rm(xx)

  freqs <- slot(pi, "Freq")

  if (chk) {
    stopifnot(all(apply(mat, 2, function(x) {
      sum(x * freqs[ids_innercells])
    }) == freqs))
  }

  # 4: use strids to identify cells
  strids <- slot(pi, "strID")

  colnames(mat) <- strids
  rownames(mat) <- strids[ids_innercells]

  z <- matrix(data = freqs, ncol = 1)
  colnames(z) <- "freq"
  rownames(z) <- strids

  y <- z_supp <- z
  y <- y[rownames(y) %in% rownames(mat), , drop = F]

  # with NA in suppressed cells
  z_supp[slot(pi, "sdcStatus") %in% c("u", "x"), 1] <- NA

  # info: what are inner/marginal cells
  info <- data.frame(
    cell_id = strids,
    is_innercell = FALSE,
    stringsAsFactors = FALSE
  )
  info$is_innercell[info$cell_id %in% rownames(mat)] <- TRUE
  list(x = mat, y = y, z = z, z_supp = z_supp, info = info)
}

