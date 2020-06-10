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
#' # check if inner cells are all protected
#' df <- data.frame(
#'   freqs_orig = inp_regsdc$z[inp_regsdc$info$is_innercell == TRUE, ],
#'   freqs_supp = inp_regsdc$z_supp[inp_regsdc$info$is_innercell == TRUE, ],
#'   regsdc = res_regsdc
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
  strID <- id <- NULL
  stopifnot(inherits(x, "sdcProblem"))
  stopifnot(rlang::is_scalar_logical(chk))

  # extracts the "problemInstance"
  pi <- get.sdcProblem(x, type = "problemInstance")
  nr_cells <- get.problemInstance(pi, "nrVars")

  df <- sdcProb2df(x, addDups = FALSE, dimCodes = "original")
  df$inner_cell <- TRUE

  # all_contributing codes
  contr_codes <- .get_all_contributing_codes(x)
  names(contr_codes)

  # minimal_codes
  tot_codes <- lapply(x@dimInfo@dimInfo, function(y) {
    y@codesOriginal[!y@codesMinimal]
  })
  for (i in 1:length(tot_codes)) {
    df$inner_cell[df[[names(tot_codes)[i]]] %in% tot_codes[[i]]] <- FALSE
  }

  totals <- df$strID[df$inner_cell == FALSE]
  inner_cells <- df$strID[df$inner_cell == TRUE]

  # matrix:
  # cols --> all cells
  # rows = inner cells
  mat <- slam::simple_triplet_diag_matrix(v = 1, nrow = nr_cells)
  rownames(mat) <- colnames(mat) <- df$strID
  mat <- mat[rownames(mat) %in% inner_cells, ]

  # 1: we compute for all subtotal-codes the contributing inner codes
  cn_dims <- names(contr_codes)
  nr_dims <- length(contr_codes)
  pool <- vector("list", nr_dims)
  names(pool) <- cn_dims

  innerdf <- df[df$inner_cell == TRUE, ]
  innerdf$id <- 1:nrow(innerdf)

  for (d in cn_dims) {
    nr_tots <- length(tot_codes[[d]])
    ll <- vector("list", length = nr_tots)
    names(ll) <- tot_codes[[d]]

    for (j in tot_codes[[d]]) {
      ll[[j]] <- which(innerdf[[d]] %in% contr_codes[[d]][[j]]$contr_codes)
    }
    pool[[d]] <- ll
  }

  # 2: we compute for all subtotals the linear dependencies based
  # strictly on inner cells
  for (code in totals) {
    j <- match(code, colnames(mat))
    ids <- 1:nrow(mat)
    dd <- df[strID == code, cn_dims, with = FALSE]
    for (k in cn_dims) {
      v <- dd[[k]]
      if (v %in% names(pool[[k]])) {
        ids <- intersect(ids, pool[[k]][[dd[[k]]]])
      } else {
        ids <- intersect(ids, which(innerdf[[k]] == v))
      }
    }
    idx <- innerdf[id %in% unique(ids), id]
    mat[idx, j] <- 1
  }

  freqs <- df$freq

  if (chk) {
    stopifnot(all(apply(mat, 2, function(x) {
      sum(x * innerdf$freq)
    }) == freqs))
  }

  # 3: use strids to identify cells
  z <- matrix(data = freqs, ncol = 1)
  colnames(z) <- "freq"
  rownames(z) <- df$strID

  y <- z_supp <- z
  y <- y[rownames(y) %in% rownames(mat), , drop = F]

  # with NA in suppressed cells
  z_supp[df$sdcStatus %in% c("u", "x"), 1] <- NA

  # info: what are inner/marginal cells
  info <- data.frame(
    cell_id = df$strID,
    is_innercell = FALSE,
    stringsAsFactors = FALSE
  )
  info$is_innercell[info$cell_id %in% rownames(mat)] <- TRUE
  list(x = mat, y = y, z = z, z_supp = z_supp, info = info)
}
