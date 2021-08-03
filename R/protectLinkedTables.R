#' Protect two tables with common cells
#'
#' [protectLinkedTables()] can be used to protect tables that have
#' common cells. It is of course required that after the anonymization process
#' has finished, all common cells have the same anonymization state in both
#' tables.
#'
#' @param objectA a [sdcProblem-class] object
#' @param objectB a [sdcProblem-class] object
#' @param commonCells a list object defining common cells in
#' `objectA` and `objectB`. For each variable that has one or more common
#' codes in both tables, a list element needs to be specified.
#' - List-elements of length 3: Variable has exact same levels and structure
#' in both input tables
#'    * `first element`: scalar character vector specifying the variable
#'    name in argument `objectA`
#'    * `second element`: scalar character vector specifying the variable
#'    name in argument `objectB`
#'    * `third element`: scalar character vector being with keyword `"ALL"`
#' - List-elements of length `4`: Variable has different codes and levels
#' in tables `objectA` and `objectB`
#'    * `first element`: scalar character vector specifying the variable
#'    name in argument `objectA`
#'    * `second element`: scalar character vector specifying the variable
#'    name in argument `objectB`
#'    * `third element`: character vector defining codes within `objectA`
#'    * `fourth element`: character vector with length that equals the length
#'    of the third list-element. This vector defines codes of the dimensional
#'    variable in `objectB` that match the codes given in the third list-element
#'    for `objectA`.
#' @param method scalar character vector defining the algorithm
#' that should be used to protect the primary sensitive table cells. In versions `>= 0.32`
#' only the `SIMPLEHEURISTIC` procedure is supported; For details please see
#' [protectTable()].
#' @param ... additional arguments to control the secondary cell suppression
#' algorithm. For details, see [protectTable()].
#'
#' @return a list of length `2` with each list-element being an
#' [safeObj-class] object
#' @md
#' @examples
#' \dontrun{
#' # load micro data for further processing
#' data("microdata2", package = "sdcTable")
#'
#' # table1: defined by variables 'gender' and 'ecoOld'
#' microData1 <- microData[,c(2,3,5)]
#'
#' # table2: defined by variables 'region', 'gender' and 'ecoNew'
#' microData2 <- microData[,c(1,2,4,5)]
#'
#' # we need to create information on the hierarchies
#' # variable 'region': exists only in microDat2
#' d_region <- hier_create(root = "Tot", nodes = c("R1", "R2"))
#'
#' # variable 'gender': exists in both datasets
#' d_gender <- hier_create(root = "Tot", nodes = c("m", "f"))
#'
#' # variable 'eco1': exists only in microDat1
#' d_eco1 <- hier_create(root = "Tot", nodes = c("A", "B"))
#' d_eco1 <- hier_add(d_eco1, root = "A", nodes = c("Aa", "Ab"))
#' d_eco1 <- hier_add(d_eco1, root = "B", nodes = c("Ba", "Bb"))
#'
#' # variable 'ecoNew': exists only in microDat2
#' d_eco2 <- hier_create(root = "Tot", nodes = c("C", "D"))
#' d_eco2 <- hier_add(d_eco2, root = "C", nodes = c("Ca", "Cb", "Cc"))
#' d_eco2 <- hier_add(d_eco2, root = "D", nodes = c("Da", "Db", "Dc"))
#'
#' # creating objects holding information on dimensions
#' dl1 <- list(gender = d_gender, ecoOld = d_eco1)
#' dl2 <- list(region = d_region, gender = d_gender, ecoNew = d_eco2)
#'
#' # creating input objects for further processing.
#' # For details, see ?makeProblem.
#' p1 <- makeProblem(
#'   data = microData1,
#'   dimList = dl1,
#'   dimVarInd = 1:2,
#'   numVarInd = 3)
#'
#' p2 <- makeProblem(
#'   data = microData2,
#'   dimList = dl2,
#'   dimVarInd = 1:3,
#'   numVarInd = 4)
#'
#' # the cell specified by gender == "Tot" and ecoOld == "A"
#' # is one of the common cells! -> we mark it as primary suppression
#' p1 <- changeCellStatus(
#'   object = p1,
#'   characteristics = c("Tot", "A"),
#'   varNames = c("gender", "ecoOld"),
#'   rule = "u",
#'   verbose = FALSE)
#'
#' # the cell specified by region == "Tot" and gender == "f" and ecoNew == "C"
#' # is one of the common cells! -> we mark it as primary suppression
#' p2 <- changeCellStatus(
#'   object = p2,
#'   characteristics = c("Tot", "f", "C"),
#'   varNames = c("region", "gender", "ecoNew"),
#'   rule = "u",
#'   verbose = FALSE)
#'
#' # specifying input to define common cells
#' common_cells <- list()
#'
#' # variable "gender"
#' common_cells$v.gender <- list()
#' common_cells$v.gender[[1]] <- "gender" # variable name in "p1"
#' common_cells$v.gender[[2]] <- "gender" # variable name in "p2"
#'
#' # "gender" has equal characteristics on both datasets -> keyword "ALL"
#' common_cells$v.gender[[3]] <- "ALL"
#'
#' # variables: "ecoOld" and "ecoNew"
#' common_cells$v.eco <- list()
#' common_cells$v.eco[[1]] <- "ecoOld" # variable name in "p1"
#' common_cells$v.eco[[2]] <- "ecoNew" # variable name in "p2"
#'
#' # vector of common characteristics:
#' # "A" and "B" in variable "ecoOld" in "p1"
#' common_cells$v.eco[[3]] <- c("A", "B")
#'
#' # correspond to codes "C" and "D" in variable "ecoNew" in "p2"
#' common_cells$v.eco[[4]] <- c("C", "D")
#'
#' # protect the linked data
#' result <- protectLinkedTables(
#'   objectA = p1,
#'   objectB = p2,
#'   commonCells = common_cells,
#'   verbose = TRUE)
#'
#' # having a look at the results
#' result_tab1 <- result[[1]]
#' result_tab2 <- result[[2]]
#' summary(result_tab1)
#' summary(result_tab2)
#' }
#' @rdname protectLinkedTables
#' @export protectLinkedTables
#' @seealso [protectTable()]
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
protectLinkedTables <- function(objectA, objectB, commonCells, method = "SIMPLEHEURISTIC", ...) {
  . <- sdcStatus <- freq <- striD <- strID_x <- strID_y <- NULL

  x <- objectA
  y <- objectB
  common_cells <- commonCells
  stopifnot(inherits(x, "sdcProblem"))
  stopifnot(inherits(y, "sdcProblem"))
  method <- "SIMPLEHEURISTIC" # overwritten since 0.32 only SIMPLEHEURISTIC is supported
  params <- genParaObj(selection = "control.secondary", method = method, ...)

  df_x <- sdcProb2df(x, addDups = FALSE, dimCodes = "original")
  df_x[[.tmpweightname()]] <- g_weight(slot(x, "problemInstance"))
  df_y <- sdcProb2df(y, addDups = FALSE, dimCodes = "original")
  df_y[[.tmpweightname()]] <- g_weight(slot(y, "problemInstance"))

  .nr_supps <- function(x) {
    sum(g_sdcStatus(slot(x, "problemInstance")) %in% c("u", "x"))
  }

  if (.nr_supps(x) + .nr_supps(y) == 0) {
    return(list(
      outObj1 = c_finalize(x, input = params),
      outObj2 = c_finalize(y, input = params),
      full_m = NULL,
      full_df = NULL)
    )
  }

  .indices_common_cells <- function(df_x, df_y, common_cells) {
    strID <- NULL
    # restrict to totals in non-overlapping variables in dataset2
    .overall_total <- function(x, vname) {
      x@dimInfo@dimInfo[[vname]]@codesOriginal[1]
    }

    .tmpvname <- function(x) {
      paste0(x, ".tmpvname.xxxxxx")
    }

    df_x$id_x <- seq_len(nrow(df_x))
    df_y$id_y <- seq_len(nrow(df_y))

    all_vnames_x <- dv_x <- x@dimInfo@vNames
    all_vnames_y <- dv_y <- y@dimInfo@vNames

    for (i in seq_len(length(common_cells))) {
      cc <- common_cells[[i]]
      if (!length(cc) %in% 3:4) {
        stop("invalid input detected", call. = FALSE)
      }

      # we compute variables, we have already dealt with
      cn_x <- cc[[1]]
      cn_y <- cc[[2]]

      df_x[[.tmpvname(cn_x)]] <- df_x[[cn_x]]
      df_y[[.tmpvname(cn_y)]] <- df_y[[cn_y]]

      dv_x <- setdiff(dv_x, cn_x)
      dv_y <- setdiff(dv_y, cn_y)
      if  (length(cc) == 4) {
        df_x <- subset(df_x, df_x[[cn_x]] %in% cc[[3]])
        df_y <- subset(df_y, df_y[[cn_y]] %in% cc[[4]])

        # replace codes in second dataset with those from the first one
        df_y[[cn_y]] <- cc[[3]][match(df_y[[cn_y]], cc[[4]])]
      }
    }

    for (i in seq_len(length(dv_x))) {
      vname <- dv_x[i]
      df_x <- subset(df_x, df_x[[vname]] == .overall_total(x = x, vname = vname))
    }
    for (i in seq_len(length(dv_y))) {
      vname <- dv_y[i]
      df_y <- subset(df_y, df_y[[vname]] == .overall_total(x = y, vname = vname))
    }

    # merge and make sure order matches
    data.table::setorder(df_x, strID)
    data.table::setorder(df_y, strID)

    matchvars_x <- sapply(common_cells, function(x) x[[1]])
    matchvars_y <- sapply(common_cells, function(x) x[[2]])

    tmp_x <- df_x[, c("strID", "freq", "id_x", matchvars_x), with = FALSE]
    data.table::setnames(tmp_x, old = c("strID", "freq"), new = c("strID_x", "freq_x"))
    tmp_y <- df_y[, c("strID", "freq", "id_y", matchvars_y), with = FALSE]
    data.table::setnames(tmp_y, old = c("strID", "freq"), new = c("strID_y", "freq_y"))

    mm <- merge(tmp_x, tmp_y, by.x = matchvars_x, by.y = matchvars_y, all = TRUE)
    data.table::setkey(mm, strID_x)
    stopifnot(all(mm$freq_x == mm$freq_y))

    df_common <- data.frame(
      strid_x = mm$strID_x,
      strid_y = mm$strID_y,
      id_x = mm$id_x,
      id_y = mm$id_y
    )
    rownames(df_common) <- NULL
    df_common
  }
  message("common cell indices are computed ... ", appendLF = FALSE)
  df_common <- .indices_common_cells(
    df_x = df_x,
    df_y = df_y,
    common_cells = common_cells
  )
  message("[done]")

  # create a full "common" constraint-matrix
  .full_common_matrix <- function(x, y, df_common) {
    # returns a simple-triplet-matrices (from slam-pkg)
    mx <- .gen_contraint_matrix(x)
    info_x <- attributes(mx)$infodf

    my <- .gen_contraint_matrix(y)
    info_y <- attributes(my)$infodf

    colnames(mx) <- paste0("px_", info_x$str_id)
    colnames(my) <- paste0("py_", info_y$str_id)

    colnames(mx)[df_common$id_x] <- paste0("c_", df_common$strid_x, "_", df_common$strid_y)
    colnames(my)[df_common$id_y] <- paste0("c_", df_common$strid_x, "_", df_common$strid_y)

    # fill matrices with missing variables (only 0)
    v1_miss <- setdiff(colnames(my), colnames(mx))
    if (length(v1_miss) > 0) {
      tmpmat <- slam::simple_triplet_zero_matrix(nrow = nrow(mx), ncol = length(v1_miss))
      colnames(tmpmat) <- v1_miss
      mx <- cbind(mx, tmpmat)
    }
    v2_miss <- setdiff(colnames(mx), colnames(my))
    if (length(v2_miss) > 0) {
      tmpmat <- slam::simple_triplet_zero_matrix(nrow = nrow(my), ncol = length(v2_miss))
      colnames(tmpmat) <- v2_miss
      my <- cbind(my, tmpmat)
    }

    # we need to make sure, variable-order is the same in both matrices so that rbind()-works
    my <- my[, match(colnames(mx), colnames(my))]
    full_m <- rbind(mx, my)
    unique(full_m)
  }

  message("the full constraint-matrix is computed", appendLF = FALSE)
  full_m <- .full_common_matrix(x = x, y = y, df_common = df_common)
  message(" (", ncol(full_m), " variables and ", nrow(full_m), " constraints) [done]")

  # cn: names of full_m (combined variable names)
  .create_full_df <- function(df_x, df_y, df_common, cn) {
    strID <- NULL
    df_x <- df_x[, c("strID", "freq", "sdcStatus", .tmpweightname())]
    df_y <- df_y[, c("strID", "freq", "sdcStatus", .tmpweightname())]

    df_x$strID <- paste0("px_", df_x$strID)
    df_y$strID <- paste0("py_", df_y$strID)

    df_x$strID[df_common$id_x] <- paste0("c_", df_common$strid_x, "_", df_common$strid_y)
    df_y$strID[df_common$id_y] <- paste0("c_", df_common$strid_x, "_", df_common$strid_y)

    df_y <- df_y[!grepl("c_", df_y$strID)]

    df_full <- rbind(df_x, df_y)
    stopifnot(nrow(df_full) == length(cn)) # we need a column for each (unique) cell

    # make sure the order of column names in full_m matches the rows in df_full
    df_full <- df_full[match(cn, df_full$strID)]

    df_full$is_common_cell <- substr(df_full$strID, 1, 2) == "c_"
    df_full
  }

  message("a (temp) full-dataset with cells from both problems is computed ... ", appendLF = FALSE)
  full_df <- .create_full_df(
    df_x = df_x,
    df_y = df_y,
    df_common = df_common,
    cn = colnames(full_m)
  )
  message("[done]")

  # just for debugging
  print_constraints <- function(df, mat) {
    is_innercell <- sapply(df$strID, function(x) {
      sum(as.matrix(mat[, x]) == -1) == 0
    })
    df[, innercell := "st"]
    df[is_innercell, innercell := "i"]
    for (i in 1:nrow(mat)) {
      idx_tot <- which(as.matrix(mat[i, ]) == -1)
      idx_contr <- which(as.matrix(mat[i, ]) == 1)
      message("constraint ", i, ": ", paste0(shQuote(df$strID[idx_tot]), "(", df$innercell[idx_tot], ")"), " = ", paste0(shQuote(df$strID[idx_contr]), " (", df$innercell[idx_contr] ,")", collapse = " + "))
    }
  }
  #print_constraints(full_df, full_m)

  # anonymize the problem
  message("the linked problem is anonymized ... ")

  res <- suppConstraints(
    dat = full_df,
    m = full_m,
    params = list(
      check_constraints = TRUE, # possibly generate new constraints!
      verbose = params$verbose,
      do_singletons = params$detectSingletons,
      threshold = ifelse(is.na(params$threshold), 0, params$threshold)
    )
  )
  message("[done]")

  # split again
  full_df$sdcStatus <- res$sdc_status

  # split into two problems again
  strID <- NULL
  full_df[, strID_x := NA_character_]
  full_df[, strID_x := NA_character_]
  full_df[grepl("px_", strID), strID_x := sub("px_", "", strID)]
  full_df[grepl("py_", strID), strID_y := sub("py_", "", strID)]

  idx_common <- grepl("c_", full_df$strID)

  common_strids <- do.call("rbind", lapply(full_df$strID[idx_common], function(x) {
    ll <- strsplit(x, "_")[[1]]
    data.frame(x = ll[2], y = ll[3], stringsAsFactors = FALSE)
  }))

  full_df[idx_common, strID_x := common_strids$x]
  full_df[idx_common, strID_y := common_strids$y]

  res_x <- full_df[!is.na(strID_x), .(strID_x, sdcStatus, freq)]
  data.table::setnames(res_x, old = c("strID_x", "sdcStatus"), new = c("strID", "sdcStatus_new"))

  res_y <- full_df[!is.na(strID_y), .(strID_y, sdcStatus, freq)]
  data.table::setnames(res_y, old = c("strID_y", "sdcStatus"), new = c("strID", "sdcStatus_new"))

  data.table::setkey(res_x, strID)
  data.table::setkey(res_y, strID)

  # update sdcProblems
  x@problemInstance@sdcStatus <- res_x$sdcStatus_new
  y@problemInstance@sdcStatus <- res_y$sdcStatus_new

  res_x <- c_finalize(object = x, input = params)
  res_y <- c_finalize(object = y, input = params)
  return(list(
      outObj1 = res_x,
      outObj2 = res_y,
      full_df = full_df,
      full_m = res$constraints
    )
  )
}
