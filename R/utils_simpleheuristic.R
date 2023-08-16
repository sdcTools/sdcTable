# simpleheuristic based on constraints and iteratively solving attacker problems
# until all primary suppressed cells are secure
.protect_simpleheuristic <- function(object, input) {
  if (input$verbose) {
    message("note: attacker-problems are iteratively solved in this procedure")
  }

  sdcStatus <- chkdf <- NULL
  full_m <- create_m_matrix(obj = object, convert = FALSE, add_info_df = TRUE)

  # store the constraint matrix as attribute; in this case it can be
  # reused by attack()
  attr(object@problemInstance, "constraint_matrix") <- full_m
  info_df <- attributes(full_m)$infodf

  pi <- slot(object, "problemInstance")

  df <- sdcProb2df(object, addDups = FALSE, dimCodes = "original")
  df[[.tmpweightname()]] <- g_weight(pi)
  df$is_common_cell <- FALSE # required for suppConstraints()
  df$id <- 1:nrow(df)

  # remember original z's
  orig_z <- df$sdcStatus == "z"

  # we need to solve the problem in any case
  # looping or not
  finished <- FALSE
  run <- 0
  max_run <- 20
  cell_info <- list()

  while (!finished) {
    run <- run + 1
    if (input$verbose) {
      message("run ", run, " | ", max_run)
    }
    if (run > max_run) {
      stop("no solution possible after ", max_run, " runs", call. = FALSE)
    }

    if (run > 1) {
      do_singletons <- FALSE
      threshold <- 0
      cppverbose <- FALSE
    } else {
      do_singletons <- input$detectSingletons
      threshold <- ifelse(is.na(input$threshold), 0, input$threshold)
      cppverbose <- input$verbose
    }

    res <- suppConstraints(
      dat = df,
      m = full_m,
      params = list(
        check_constraints = FALSE, # just check the generated constraints
        verbose = cppverbose,
        do_singletons = do_singletons,
        threshold = threshold
      )
    )

    # reset z-cells
    idx <- res$sdc_status == "z" & !orig_z
    res$sdc_status[idx] <- "s"

    if (run == 1) {
      primsupps <- which(res$sdc_status == c("u"))
    } else {
      primsupps <- chkdf$prim_supps
    }

    # checking attacker's problems for primary unsafe cells
    object@problemInstance@sdcStatus <- res$sdc_status
    # cells_to_check are all remaining primary suppressions that were
    # previously not safe
    chkdf <- attack(object, to_attack = primsupps)
    chkdf <- chkdf[chkdf$protected == FALSE, ]

    added_supps <- c()
    if (nrow(chkdf) > 0) {
      if (input$verbose) {
        message("--> invalid solution for ", nrow(chkdf), " cells found in run ", run, appendLF = FALSE)
        message(" --> additional suppressions need to be added")
      }
      df$added_supps <- FALSE # we keep track if additional cells have already been suppressed

      for (cell in chkdf$id) {
        added_supp <- FALSE
        cnt <- 0
        while (!added_supp) {
          cnt <- cnt + 1
          # in which partition is the cell in?
          if (is.null(cell_info[[as.character(cell)]])) {
            # compute cell information
            tmp_i <- tmp_j <- c()
            pp <- object@partition$indices
            for (idx_i in 1:length(pp)) {
              for (idx_j in 1:length(pp[[idx_i]])) {
                if (cell %in% pp[[idx_i]][[idx_j]]) {
                  tmp_i <- c(tmp_i, idx_i)
                  tmp_j <- c(tmp_j, idx_j)
                }
              }
            }
            cell_info[[as.character(cell)]] <- list(tmp_i = tmp_i, tmp_j = tmp_j)
          } else {
            # receive cell information
            tmp_i <- cell_info[[as.character(cell)]]$tmp_i
            tmp_j <- cell_info[[as.character(cell)]]$tmp_j
          }

          # Idea: use cell with lowest value in the contributing cells
          finished2 <- FALSE
          added_supp <- FALSE
          i <- length(tmp_i) + 1
          while (!finished2) {
            i <- i - 1
            if (i <= 0) {
              stop("Unfortunately, no suitable suppression-pattern could be found", call. = FALSE)
            }
            ids <- pp[[tmp_i[i]]][[tmp_j[i]]]
            st <- df[ids][sdcStatus == "s"]
            if (nrow(st) > 0) {
              data.table::setorderv(st, .tmpweightname())
              add_supp <- st$id[1]
              df$added_supps[add_supp] <- TRUE
              df$sdcStatus[add_supp] <- "x"
              added_supp <- TRUE
              finished2 <- TRUE
            } else {
              if (i == 1) {
                # all tables with this cells are fully supped
                # -> nothing more to suppress left
                added_supp <- TRUE
                finished2 <- TRUE
              }
            }
          }
        }
      }
    } else {
      finished <- TRUE
    }
  }
  invisible(list(object = object, zstatus = NA))
}

# old implementation < 0.32 (fast but possibly unsafe)
# in this case; no attacker problems are solved
.protect_simpleheuristic_old <- function(object, input) {
  freq <- id <- sdcStatus <- weights <- NULL
  verbose <- input$verbose
  detectSingletons <- input$detectSingletons
  pI <- g_problemInstance(object)
  indices <- g_partition(object)$indices
  dimInfo <- g_dimInfo(object)
  strInfo <- g_str_info(dimInfo)
  vNames <- g_varname(dimInfo)

  if (verbose) {
    message("calculating subIndices (this may take a while) ...", appendLF = FALSE)
  }

  dat <- as.data.table(cpp_splitByIndices(g_strID(pI), strInfo))
  setnames(dat, vNames)
  dat[, id := 1:nrow(dat)]
  dat[, freq := g_freq(pI)]
  dat[, weights := g_weight(pI)]
  dat[, sdcStatus := g_sdcStatus(pI)]
  dimVars <- match(vNames, names(dat))
  nDims <- length(dimVars)
  freqInd <- match("freq", colnames(dat))
  if (length(vNames) == 1) {
    combs <- combn(vNames, 1)
  } else {
    combs <- combn(vNames, length(vNames) - 1)
  }

  tmpIndices <- rep(NA, length(vNames))

  nrGroups <- length(indices)
  subIndices <- list()
  length(subIndices) <- nrGroups

  for (group in 1:nrGroups) {
    nrTabs <- length(indices[[group]])
    subIndices[[group]] <- list()
    length(subIndices[[group]]) <- nrTabs
    for (tab in 1:nrTabs) {
      subDat <- dat[indices[[group]][[tab]], ]
      # only one dimension!
      if (ncol(combs) == 1) {
        subDat$ind_1_tmp <- 1
        tmpIndices[1] <- ncol(subDat)
      } else {
        for (i in 1:ncol(combs)) {
          setkeyv(subDat, combs[, i])
          cn <- paste0("ind_", i, "_tmp")
          expr <- parse(text = paste0(cn, ":=.GRP"))
          subDat[, eval(expr), by = key(subDat)]
          tmpIndices[i] <- ncol(subDat)
        }
      }
      setkeyv(subDat, vNames)
      subIndices[[group]][[tab]] <- as.list(subDat[, tmpIndices, with = FALSE])
    }
  }
  if (verbose) {
    message("[done]")
  }

  if (detectSingletons | !is.na(input$threshold)) {
    if (verbose) {
      message("start singleton/threshold detection procedure")
    }

    res <- detect_singletons(
      dat = dat,
      indices = indices,
      sub_indices = subIndices,
      do_singletons = input$detectSingletons,
      threshold = input$threshold
    )

    if (verbose) {
      message(
        "singleton/threshold detection procedure finished with ",
        res$nr_added_supps, " additional suppressions."
      )
    }
    dat <- res$dat; rm(res)
  }

  res <- greedyMultDimSuppression(
    dat = dat,
    indices = indices,
    subIndices = subIndices,
    dimVars = dimVars,
    verbose = verbose
  )

  if (verbose) {
    message("finishing output...", appendLF = FALSE)
  }

  # set orig primsupps as "u"
  status_new <- res$sdcStatus
  status_new[status_new %in% c("u", "x")] <- "x"
  status_new[pI@sdcStatus == "u"] <- "u"
  s_sdcStatus(pI) <- list(index = res$id, vals = status_new)
  s_problemInstance(object) <- pI
  if (verbose) {
    message("[done]")
  }
  invisible(list(object = object, zstatus = res$status_z))
}
