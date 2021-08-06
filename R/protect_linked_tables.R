#' Protect two tables with common cells
#'
#' [protect_linked_tables()] can be used to protect tables that have
#' common cells. It is of course required that after the anonymization process
#' has finished, all common cells have the same anonymization state in both
#' tables.
#'
#' @param x a [sdcProblem-class] object
#' @param y a [sdcProblem-class] object
#' @param common_cells a list object defining common cells in
#' `x` and `y`. For each variable that has one or more common
#' codes in both tables, a list element needs to be specified.
#' - List-elements of length `3`: Variable has exact same levels and structure
#' in both input tables
#'    * `first element`: scalar character vector specifying the variable
#'    name in argument `x`
#'    * `second element`: scalar character vector specifying the variable
#'    name in argument `y`
#'    * `third element`: scalar character vector being with keyword `"ALL"`
#' - List-elements of length `4`: Variable has different codes and levels
#' in inputs `x` and `y`
#'    * `first element`: scalar character vector specifying the variable
#'    name in argument `x`
#'    * `second element`: scalar character vector specifying the variable
#'    name in argument `y`
#'    * `third element`: character vector defining codes within `x`
#'    * `fourth element`: character vector with length that equals the length
#'    of the third list-element. This vector defines codes of the dimensional
#'    variable in `y` that match the codes given in the third list-element
#'    for `x`.
#' @param ... additional arguments to control the secondary cell suppression
#' algorithm. For details, see [protectTable()].
#'
#' @return a list elements `x` and `y` containing protected `sdcProblem` objects
#' @md
#' @examples
#' \dontrun{
#' # load micro data for further processing
#' utils::data("microdata2", package = "sdcTable")
#'
#' # table1: defined by variables 'gender' and 'ecoOld'
#' md1 <- microdata2[,c(2,3,5)]
#'
#' # table2: defined by variables 'region', 'gender' and 'ecoNew'
#' md2 <- microdata2[,c(1,2,4,5)]
#'
#' # we need to create information on the hierarchies
#' # variable 'region': exists only in md2
#' d_region <- hier_create(root = "Tot", nodes = c("R1", "R2"))
#'
#' # variable 'gender': exists in both datasets
#' d_gender <- hier_create(root = "Tot", nodes = c("m", "f"))
#'
#' # variable 'eco1': exists only in md1
#' d_eco1 <- hier_create(root = "Tot", nodes = c("A", "B"))
#' d_eco1 <- hier_add(d_eco1, root = "A", nodes = c("Aa", "Ab"))
#' d_eco1 <- hier_add(d_eco1, root = "B", nodes = c("Ba", "Bb"))
#'
#' # variable 'ecoNew': exists only in md2
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
#'   data = md1,
#'   dimList = dl1,
#'   dimVarInd = 1:2,
#'   numVarInd = 3)
#'
#' p2 <- makeProblem(
#'   data = md2,
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
#' result <- protect_linked_tables(
#'   x = p1,
#'   y = p2,
#'   common_cells = common_cells,
#'   verbose = TRUE)
#'
#' # having a look at the results
#' result_tab1 <- result$x
#' result_tab2 <- result$y
#' summary(result_tab1)
#' summary(result_tab2)
#' }
#' @export
#' @seealso [protectTable()]
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
protect_linked_tables <- function(x, y, common_cells, ...) {
  . <- sdcStatus <- freq <- striD <- strID_x <- strID_y <- innercell <- chkdf <- NULL

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
  if (params$verbose) {
    message("computing common cell-indices")
  }

  df_common <- .indices_common_cells(
    df_x = df_x,
    df_y = df_y,
    common_cells = common_cells
  )

  # create a full "common" constraint-matrix
  .full_common_matrix <- function(x, y, df_common) {
    # returns a simple-triplet-matrices (from slam-pkg)

    # get/compute constraint matrix
    mx <- attributes(x@problemInstance)$constraint_matrix
    if (is.null(mx)) {
      mx <- .gen_contraint_matrix(x)
    }
    info_x <- attributes(mx)$infodf

    my <- attributes(y@problemInstance)$constraint_matrix
    if (is.null(my)) {
      my <- .gen_contraint_matrix(y)
    }
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

  if (params$verbose) {
    message("computing the full constraint-matrix: ", appendLF = FALSE)
  }

  full_m <- .full_common_matrix(x = x, y = y, df_common = df_common)
  if (params$verbose) {
    message(ncol(full_m), " variables; ", nrow(full_m), " constraints)")
  }

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
    df_full$id <- 1:nrow(df_full)
    df_full
  }

  if (params$verbose) {
    message("compute a (temporary) full dataset with cells from both problems")
  }
  full_df <- .create_full_df(
    df_x = df_x,
    df_y = df_y,
    df_common = df_common,
    cn = colnames(full_m)
  )

  if (params$verbose) {
    if (params$solve_attackerprobs == TRUE) {
      message("note: attacker-problems are iteratively solved in this procedure")
    } else {
      message("note: attacker-problems are not solved; this might be unsafe.")
    }
  }

  # we need to solve the problem in any case
  # looping or not
  finished <- FALSE
  run <- 0
  max_run <- 10
  while (!finished) {
    run <- run + 1
    if (run > max_run) {
      stop("no solution possible after ", max_run, " runs", call. = FALSE)
    }
    if (run > 1) {
      do_singletons <- FALSE
      threshold <- 0
      cppverbose <- FALSE
    } else {
      do_singletons <- params$detectSingletons
      threshold <- ifelse(is.na(params$threshold), 0, params$threshold)
      cppverbose <- params$verbose
    }

    # anonymize and apply singleton-detection all in cpp
    res <- suppConstraints(
      dat = full_df,
      m = full_m,
      params = list(
        check_constraints = FALSE, # just check the generated constraints
        verbose = cppverbose,
        do_singletons = do_singletons,
        threshold = threshold
      )
    )

    # update pattern
    full_df$sdcStatus <- res$sdc_status

    if (params$solve_attackerprobs == FALSE) {
      finished <- TRUE
    } else {
      if (run == 1) {
        primsupps <- which(res$sdc_status == c("u"))
      } else {
        primsupps <- chkdf$prim_supps
      }

      # checking attacker's problems for primary unsafe cells
      if (params$verbose) {
        message("solving attacker problems in run ", run)
      }

      attackdf <- data.frame(
        to_attack = FALSE,
        sdc = full_df$sdcStatus,
        freq = full_df$freq
      )
      attackdf$to_attack[primsupps] <- TRUE
      chkdf <- .attack_worker(
        m = full_m,
        df = attackdf,
        verbose = FALSE
      )
      chkdf <- chkdf[chkdf$protected == FALSE, ]
      if (nrow(chkdf) > 0) {
        if (params$verbose) {
          message("--> invalid solution found; additional suppressions are added")
        }
        full_df$added_supps <- FALSE # we keep track if additional cells have already been suppressed

        for (cell in chkdf$prim_supps) {
          all_deps_supped <- TRUE # we compute if all cells in all constraints are suppressed

          # constraints to which the row contributes
          rr <- full_m$i[full_m$j == cell]
          nr_deps <- length(rr)
          added_supp <- FALSE
          cnt <- 0
          while (!added_supp) {
            cnt <- cnt + 1
            message("cell: ", cell, " | cnt: ", cnt)
            if (cnt > nr_deps) {
              stop("no additional suppression could be found!", call. = FALSE)
            }
            ids <- full_m$j[full_m$i == rr[cnt]]
            strids <- colnames(full_m)[ids]
            st <- full_df[ids, ]
            all_deps_supped <- all_deps_supped && all(st$sdcStatus %in% c("u", "x", "w"))
            st <- st[sdcStatus == "s"]
            if (nrow(st) > 0) {
              data.table::setorderv(st, .tmpweightname())
              add_supp <- st$id[1]
              full_df$added_supps[add_supp] <- TRUE
              full_df$sdcStatus[add_supp] <- "x"
              added_supp <- TRUE
            } else {
              if (cnt == nr_deps && all_deps_supped) {
                # edge case: if all cells are suppressed, we cannot supp more!
                added_supp <- TRUE
              }
            }
          }
        }
      } else {
        finished <- TRUE
      }
    }
  }

  # split the complete data.frame again into two problem-instances
  .split_up <- function(x, y, full_df) {
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
    list(res_x = res_x, res_y = res_y)
  }

  # split into two problems again
  final_results <- .split_up(
    x = x,
    y = y,
    full_df = full_df
  )

  return(list(
    x = final_results$res_x,
    y = final_results$res_y
  ))
}

