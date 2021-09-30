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
      do_singletons <- input$detectSingletons
      threshold <- ifelse(is.na(input$threshold), 0, input$threshold)
      cppverbose <- input$verbose
    }

    print(table(df$sdcStatus))
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
        message("--> invalid solution found in run ", run, ": additional suppressions are added")
      }
      df$added_supps <- FALSE # we keep track if additional cells have already been suppressed

      for (cell in chkdf$id) {
        all_deps_supped <- TRUE # we compute if all cells in all constraints are suppressed

        # constraints to which the row contributes
        rr <- full_m$i[full_m$j == cell]
        nr_deps <- length(rr)
        added_supp <- FALSE

        cnt <- 0
        while (!added_supp) {
          cnt <- cnt + 1
          if (cnt > nr_deps) {
            message("we try one last time")
            # we only try for the first constraint
            ids <- full_m$j[full_m$i == rr[1]]
            idx_z <- ids[which(df[ids, sdcStatus == "z"])]
            if (length(idx_z) > 0) {
              df$sdcStatus[idx_z] <- "s"
              df[[.tmpweightname()]][idx_z] <- max(df$freq[ids]) - 1
              added_supp <- TRUE
              break
            } else {
              stop("no additional suppression could be found (cell: ", cell, ")", call. = FALSE)
            }
          }
          ids <- full_m$j[full_m$i == rr[cnt]]
          strids <- colnames(full_m)[ids]
          st <- df[ids, ]

          all_deps_supped <- all_deps_supped && all(st$sdcStatus %in% c("u", "x", "w"))
          if (any(st$added_supps)) {
            added_supp <- TRUE
          } else {
            st <- st[sdcStatus == "s"]
            if (nrow(st) > 0) {
              data.table::setorderv(st, .tmpweightname())
              add_supp <- st$id[1]
              df$added_supps[add_supp] <- TRUE
              df$sdcStatus[add_supp] <- "x"
              added_supp <- TRUE
            } else {
              if (cnt == nr_deps && all_deps_supped) {
                # edge case: if all cells are suppressed, we cannot supp more!
                added_supp <- TRUE
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
  message("old")
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
