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
#' that should be used to protect the primary sensitive table cells. The possible
#' values are `"HITAS"`, `"SIMPLEHEURISTIC"` and `"OPT"`; For details please see
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
#' sp <- searchpaths()
#' fn <- paste(sp[grep("sdcTable", sp)], "/data/microData2.RData", sep="")
#' microData <- get(load(fn))
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
#'   method = "HITAS",
#'   verbose = TRUE)
#'
#' # having a look at the results
#' result_tab1 <- result[[1]]
#' result_tab2 <- result[[2]]
#' summary(result_tab1)
#' summary(result_tab2)
#' }
#'
#' @rdname protectLinkedTables
#' @export protectLinkedTables
#' @seealso [protectTable()]
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
protectLinkedTables <- function(objectA, objectB, commonCells, method, ...) {
  .indices_common_cells <- function(input1, input2, commonCells) {
    id1 <- id2 <- NULL
    dat1 <- g_df(input1); dat1$id1 <- 1:nrow(dat1)
    dat2 <- g_df(input2); dat2$id2 <- 1:nrow(dat2)
    dat1$strID <- dat1$sdcStatus <- NULL
    dat2$strID <- dat2$sdcStatus <- NULL
    dI1 <- g_dimInfo(input1)
    dI2 <- g_dimInfo(input2)

    # restrict to totals in non-overlapping variables in dataset1
    totvars <- setdiff(dI1@vNames, sapply(commonCells, function(x) x[[1]]))
    if (length(totvars) > 0) {
      for (i in 1:length(totvars)) {
        cmd <- paste0("dat1 <- dat1[", totvars[i], "=='", dI1@dimInfo[[totvars[i]]]@codesDefault[1], "']")
        eval(parse(text = cmd))
      }
    }
    # restrict to totals in non-overlapping variables in dataset2
    totvars <- setdiff(dI2@vNames, sapply(commonCells, function(x) x[[2]]))
    if (length(totvars) > 0) {
      for (i in 1:length(totvars)) {
        cmd <- paste0("dat2 <- dat2[", totvars[i], "=='", dI2@dimInfo[[totvars[i]]]@codesDefault[1], "']")
        eval(parse(text = cmd))
      }
    }

    for (i in 1:length(commonCells)) {
      if (length(commonCells[[i]]) == 4) {
        cmd1 <- paste0("dat1 <- dat1[,tmpxxx_V", i, ":=", commonCells[[i]][[1]], "_o]")
        cmd2 <- paste0("dat2 <- dat2[,tmpxxx_V", i, ":=", commonCells[[i]][[2]], "_o]")
        eval(parse(text = cmd1))
        eval(parse(text = cmd2))

        # recode different codes to those of dataset1
        codes <- commonCells[[i]][[4]]
        codesX <- commonCells[[i]][[3]]
        for  (z in 1:length(codes)) {
          if (codes[z] != codesX[z]) {
            cmd <- paste0("dat2 <- dat2[tmpxxx_V", i, "=='", codes[z], "',tmpxxx_V", i, ":='", codesX[z], "']")
            eval(parse(text = cmd))
          }
        }
      } else {
        # nothing to do, codes are the same!
        cmd1 <- paste0("dat1[,tmpxxx_V", i, ":=", commonCells[[i]][[1]], "_o]")
        cmd2 <- paste0("dat2[,tmpxxx_V", i, ":=", commonCells[[i]][[2]], "_o]")
        eval(parse(text = cmd1))
        eval(parse(text = cmd2))
      }
    }

    kV1 <- names(dat1)[grep("tmpxxx", names(dat1))]; setkeyv(dat1, kV1)
    kV2 <- names(dat2)[grep("tmpxxx", names(dat2))]; setkeyv(dat2, kV2)
    mm <- merge(dat1, dat2); setkey(mm, id1)
    if (any(mm$freq.x != mm$freq.y)) {
      stop("Error: common cells must have same values!\n")
    }
    return(list(commonInd1 = mm$id1, commonInd2 = mm$id2))
  }

  .check_common_cells <- function(suppPattern1, suppPattern2, commonCellIndices) {
    indOK <- TRUE
    if (any(suppPattern1[commonCellIndices[[1]]] != suppPattern2[commonCellIndices[[2]]]))
      indOK <- FALSE
    return(indOK)
  }

  # returns for both sdcProblems listed in `probs` and a list of common cell indices a list:
  # each list has two slots holding information about all subtables in which common cells occur
  # along with its hashes; unique tables (according to sha1) hashes are returned in the second
  # list element
  .subtabs_with_commoncells <- function(probs, common_cell_indices) {
    # computes for a sdcProblem instance (`prob`) and vector of common cell indices
    # (`common_cells`) a list with the following slots:
    # - `res`: a list for each cell index containing a list with computed sha1-hashes
    # of the relevant table(s)
    # - `subtabs`: for each (unique) hashed table; the subtable with the indices in variable
    # `index`
    .subtabs_with_commoncells <- function(prob, common_cells) {
      .subtabs_with_single_cell <- function(df, partition, cell_id) {
        out <- list()
        for (i in seq_len(length(partition))) {
          ll <- partition[[i]]
          for (j in seq_len(length(ll))) {
            indices <- ll[[j]]
            if (any(cell_id %in% indices)) {
              tmp <- list(df = df[indices, , drop = FALSE])
              tmp$df$indices <- indices
              tmp$digest <- digest::sha1(tmp$df)
              tmp <- list(tmp)
              names(tmp) <- df$strID[cell_id]
              out <- append(out, tmp)
            }
          }
        }
        if (length(out) > 0) {
          return(out)
        }
        return(invisible(NULL))
      }

      hashes <- c()
      subtabs <- list()

      df <- sdcProb2df(prob, addDups = FALSE, dimCodes = "original")
      partition <- slot(prob, "partition")$indices

      res <- vector("list", length(common_cells))
      names(res) <- common_cells
      for (cell_id in as.character(common_cells)) {
        out <- .subtabs_with_single_cell(
          df = df,
          partition = partition,
          cell_id = cell_id
        )

        nr_subtabs <- length(out)
        if (nr_subtabs > 0) {
          res[[cell_id]] <- vector("list", length(seq_len(length(out))))
          cn <- paste0("subtab_", 1:nr_subtabs)
          names(res[[cell_id]]) <- cn

          for (i in 1:nr_subtabs) {
            hash <- out[[i]]$digest
            res[[cell_id]][[cn[i]]] <- list(
              hash  = hash
            )

            if (!hash %in% hashes) {
              hashes <- c(hashes, hash)
              subtabs[[hash]] <- out[[i]]$df
            }
          }
        }
      }
      list(res = res, subtabs = subtabs)
    }

    stopifnot(is.list(probs))
    stopifnot(length(probs) == 2)
    stopifnot(all(sapply(probs, class) == "sdcProblem"))
    stopifnot(is.list(common_cell_indices))
    stopifnot(length(common_cell_indices) == 2)
    stopifnot(length(common_cell_indices[[1]]) == length(common_cell_indices[[2]]))

    res_a <- .subtabs_with_commoncells(
      prob = probs[[1]],
      common_cells = common_cell_indices[[1]]
    )
    res_b <- .subtabs_with_commoncells(
      prob = probs[[2]],
      common_cells = common_cell_indices[[2]]
    )
    ll <- list(tab_a = res_a, tab_b = res_b, common_cell_indices = common_cell_indices)
    class(ll) <- "st_commoncells"
    ll
  }

  # todo: loops through all subtables with common cells checking for single suppressed cells
  # when removing all common cells; this is required to protect against differencing
  # this function returns a list `data.table` with additional suppressions or `NULL`
  .check_subtabs_with_commoncells <- function(x) {
    .get_tab_cell_id <- function(x, cell_id) {
      cell_id <- as.character(cell_id)
      res <- x$res[[cell_id]]
      if (is.null(res)) {
        return(NULL)
      }
      hashes <- unique(sapply(res, function(x) x$hash))
      x$subtabs[hashes]
    }

    # adding supps (lowest frequency)
    .find_supp <- function(tab) {
      res <- NULL
      if (nrow(tab) <= 1) {
        return(res)
      }
      if (sum(tab$sdcStatus %in% c("u", "x")) == 1) {
        tab <- tab[sdcStatus %in% c("s", "z")]
        rg <- range(tab$freq)
        if (rg[1] == 0 & rg[2] > 0) {
          tab <- tab[freq > 0]
        }
        setorderv(tab, "freq")

        # add a suppression
        res <- data.table(
          tab = tab$tab[1],
          index = tab$indices[1]
        )
      }
      res
    }

    stopifnot(inherits(x, "st_commoncells"))
    checked_ids <- c()
    supps <- data.table(tab = numeric(), index = numeric())

    common_cells <- x$common_cell_indices
    nr_common_cells <- length(common_cells[[1]])
    for (i in seq_len(nr_common_cells)) {
      res_a <- .get_tab_cell_id(x$tab_a, cell_id = common_cells[[1]][i])
      res_b <- .get_tab_cell_id(x$tab_b, cell_id = common_cells[[2]][i])

      gr <- expand.grid(names(res_a), names(res_b))
      gr$id <- apply(gr, 1, paste, collapse = "_")

      # we need to check all combinations
      for (z in 1:nrow(gr)) {
        id <- gr$id[z]
        if (!id %in% checked_ids) {
          checked_ids <- c(checked_ids, id)
          tab_a <- res_a[[gr$Var1[z]]]
          tab_a$tab <- 1
          tab_b <- res_b[[gr$Var2[z]]]
          tab_b$tab <- 2

          # remove common cells that occur in both tables AND that are not suppressed
          idx <- sapply(1:nr_common_cells, function(x) {
            common_cells[[1]][x] %in% tab_a$indices & common_cells[[2]][x] %in% tab_b$indices
          })

          tab <- freq <- sdcStatus <- indices <- NULL
          tab_a <- tab_a[!indices %in% common_cells[[1]][idx]]
          tab_b <- tab_b[!indices %in% common_cells[[2]][idx]]

          supps <- rbind(supps, .find_supp(tab = tab_a))
          supps <- rbind(supps, .find_supp(tab = tab_b))
        }
      }
    }
    supps
  }

  if (!method %in% c("HITAS", "OPT", "SIMPLEHEURISTIC")) {
    stop("valid methods are 'HITAS', 'OPT' or 'SIMPLEHEURISTIC'!", call. = FALSE)
  }
  index <- tab <- NULL
  params <- genParaObj(selection = "control.secondary", method = method, ...)

  ### first run
  if (method == "SIMPLEHEURISTIC") {
    outA <- c_quick_suppression(objectA, input = params)$object
    outB <- c_quick_suppression(objectB, input = params)$object
  } else {
    if (params$useC) {
      if (method == "OPT") {
        outA <- c_opt_cpp(object = objectA, input = params)
        outB <- c_opt_cpp(object = objectB, input = params)
      }
      if (method == "HITAS") {
        outA <- c_hitas_cpp(object = objectA, input = params)
        outB <- c_hitas_cpp(object = objectB, input = params)
      }
    } else {
      outA <- c_anon_worker(object = objectA, input = params)
      outB <- c_anon_worker(object = objectB, input = params)
    }
  }

  pI.A <- g_problemInstance(outA)
  pI.B <- g_problemInstance(outB)

  # calc original primary suppressions
  origPrimSupp1Index <- g_primSupps(pI.A)
  origPrimSupp2Index <- g_primSupps(pI.B)

  # no primary suppressions
  if (length(origPrimSupp1Index) + length(origPrimSupp2Index) == 0) {
    if (params$verbose) {
      message("===> no primary suppressions. all common cells have the same anonymity-status! [finished]")
    }
    outA <- c_finalize(object = outA, input = params)
    outB <- c_finalize(object = outB, input = params)
    return(list(outObj1 = outA, outObj2 = outB))
  }

  # calculate commonCells:
  commonCellIndices <- .indices_common_cells(
    input1 = outA,
    input2 = outB,
    commonCells = commonCells
  )

  # suppression patterns after the first run
  suppPatternA <- g_suppPattern(pI.A)
  suppPatternB <- g_suppPattern(pI.B)

  # checking problems with differencing due to identical cells
  res_subtabs <- .subtabs_with_commoncells(
    probs = list(outA, outB),
    common_cell_indices = commonCellIndices
  )
  additional_supps <- .check_subtabs_with_commoncells(x = res_subtabs)
  if (nrow(additional_supps) > 0) {
    chk1 <- FALSE
    ii1 <- additional_supps[tab == 1, index]
    if (length(ii1) > 0) {
      suppPatternA[ii1] <- 1
    }
    ii2 <- additional_supps[tab == 2, index]
    if (length(ii2) > 0) {
      suppPatternB[ii2] <- 1
    }
  } else {
    chk1 <- TRUE
  }

  chk2 <- .check_common_cells(suppPatternA, suppPatternB, commonCellIndices)
  finished <- chk1 & chk2
  counter <- 1

  while (!finished) {
    if (counter == 1 & params$verbose) {
      message("we have to start the iterative procedure!")
    }
    supp_pattern <- data.frame(
      p1 = suppPatternA[commonCellIndices[[1]]],
      p2 = suppPatternB[commonCellIndices[[2]]]
    )

    if (counter == 1) {
      i1 <- which(supp_pattern$p1 == 1)
      i2 <- which(supp_pattern$p2 == 1)
    } else {
      i1 <- which(supp_pattern$p1 == 0 & supp_pattern$p2 == 1)
      i2 <- which(supp_pattern$p1 == 1 & supp_pattern$p2 == 0)
    }
    index <- list()
    index[[1]] <- commonCellIndices[[1]][i1]
    index[[2]] <- commonCellIndices[[2]][i2]

    .update_and_resolve_prob <- function(prob, pattern, params) {
      pi <- g_problemInstance(prob)
      s_sdcStatus(pi) <- list(
        index = pattern,
        vals = rep("u", length(pattern))
      )

      zs <- which(g_sdcStatus(pi) == "z")
      if (length(zs) > 0) {
        s_sdcStatus(pi) <- list(
          index = zs,
          vals = rep("s", length(zs)))
      }
      s_problemInstance(prob) <- pi
      s_indicesDealtWith(prob) <- NULL
      s_startJ(prob) <- 1
      s_startI(prob) <- 1
      c_quick_suppression(prob, input = params)$object
    }

    # update and resolve problems
    outA <- .update_and_resolve_prob(prob = outA, pattern = index[[1]], params = params)
    outB <- .update_and_resolve_prob(prob = outB, pattern = index[[2]], params = params)

    suppPatternA <- g_suppPattern(g_problemInstance(outA))
    suppPatternB <- g_suppPattern(g_problemInstance(outB))

    # checking problems with differencing due to identical cells
    res_subtabs <- .subtabs_with_commoncells(
      probs = list(outA, outB),
      common_cell_indices = commonCellIndices
    )
    additional_supps <- .check_subtabs_with_commoncells(x = res_subtabs)
    if (nrow(additional_supps) > 0) {
      chk1 <- FALSE
      ii1 <- additional_supps[tab == 1, index]
      if (length(ii1) > 0) {
        suppPatternA[ii1] <- 1
      }
      ii2 <- additional_supps[tab == 2, index]
      if (length(ii2) > 0) {
        suppPatternB[ii2] <- 1
      }
    } else {
      chk1 <- TRUE
    }

    chk2 <- .check_common_cells(suppPatternA, suppPatternB, commonCellIndices)
    if (chk1 & chk2) {
      finished <- TRUE
    }
    if (counter > params$maxIter) {
      finished <- TRUE
      warning("iterative procedure did not converge! --> returning NULL")
      return(NULL)
    }
    counter <- counter + 1
  }

  if (params$verbose) {
    message(
      "===> all common cells have the same anonymity-state in both tables after ",
      counter, " iterations! [finished]")
  }
  outA <- c_finalize(object = outA, input = params)
  outB <- c_finalize(object = outB, input = params)
  return(list(outObj1 = outA, outObj2 = outB))
}
