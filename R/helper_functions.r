## Wrapper function for pasting key-Variables
pasteStrVec <- function(strVec, nrVars, coll=NULL) {
  if(length(strVec) %% nrVars != 0)
    stop("Wrong Dimensions!\n")
  if ( is.null(coll) ) {
    return(cpp_myPaste(as.character(strVec), as.integer(nrVars)[1], NA))
  } else {
    return(cpp_myPaste(as.character(strVec), as.integer(nrVars)[1], as.character(coll[1])))
  }
}

# alternative to expand.grid (used for pasteStrVec!)
expand <- function(inputList, vector=TRUE) {
  uniques <- sapply(inputList, length)
  nrPoss <- prod(uniques)
  if ( vector == TRUE ) {
    out <- NULL
    for ( i in 1:length(inputList) ) {
      if ( i == 1 )
        out <- rep(inputList[[i]], nrPoss/length(inputList[[i]]))
      else
        out <- c(out, rep(inputList[[i]], each=prod(uniques[1:(i-1)]), nrPoss/length(rep(inputList[[i]], each=prod(uniques[1:(i-1)])))))
    }
  }
  else {
    out <- list()
    for ( i in 1:length(inputList) ) {
      if ( i == 1 )
        out[[i]] <- rep(inputList[[i]], nrPoss/length(inputList[[i]]))
      else
        out[[i]] <- rep(inputList[[i]], each=prod(uniques[1:(i-1)]), nrPoss/length(rep(inputList[[i]], each=prod(uniques[1:(i-1)]))))
    }
  }
  out
}

# returns a vector original size or str
mySplit <- function(strVec, keepIndices) {
  if ( min(keepIndices) < 1 | max(keepIndices) > nchar(strVec[1]) ) {
    stop("indices must be in 1:",nchar(strVec[1]),"!\n")
  }
  keepIndices <- unique(keepIndices)
  return(cpp_mySplit(as.character(strVec), as.integer(keepIndices)))
}

mySplitIndicesList <- function(strVec, keepList, coll="-") {
  u <- unlist(keepList)
  if ( min(u) < 1 | max(u) > nchar(strVec[1]) ) {
    stop("indices must be in 1:",nchar(strVec[1]),"!\n")
  }
  out <- list()
  for ( i in 1:length(keepList) ) {
    out[[i]] <- mySplit(strVec, keepList[[i]])
  }
  out <- cpp_myPaste(as.character(unlist(out)), as.integer(length(out)), coll)
}
# mySplitIndicesList("112233444", list(1:3, 5:6, 7:8))

# check ob 'x' ganzzahlig ist
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

is.zero <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - 0) < tol
}

is.one <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - 1) < tol
}

# welche Variable soll als Branching_Variable verwendet werden?
getBranchingVariable <- function(sol, alreadyBranched, primSupps) {
  ind <- setdiff(1:length(sol), c(alreadyBranched, primSupps))
  branchVar <- ind[which.min(0.5 - sol[ind])]
  branchVar
}

my.Rglpk_solve_LP <- function(obj, mat, dir, rhs, types=NULL, max=FALSE,
  bounds=NULL, verbose=FALSE, presolve=TRUE) {
  if (!identical(max, TRUE) && !identical(max, FALSE)) {
    stop("'Argument 'max' must be either TRUE or FALSE!", call. = FALSE)
  }
  if (!identical(verbose, TRUE) && !identical(verbose, FALSE)) {
    stop("'Argument 'verbose' must be either TRUE or FALSE.", call. = FALSE)
  }
  if (!class(mat) == "simpleTriplet") {
    stop("argument 'mat' must be of class 'simpleTriplet'", call. = FALSE)
  }

  if (!all(dir %in% c("<", "<=", ">", ">=", "=="))) {
    stop('directions must be one of "<", "<=", ">", ">= or "==".', call. = FALSE)
  }

  if (is.null(types)) {
    types <- rep("C", length(obj))
  }
  if (any(is.na(match(types, c("I", "B", "C"), nomatch = NA)))) {
    stop("'types' must be either 'B', 'C' or 'I'.", call. = FALSE)
  }

  slammat <- simple_triplet_matrix(
    i = mat@i,
    j = mat@j,
    v = mat@v,
    nrow = mat@nrRows,
    ncol = mat@nrCols
  )

  x <- Rglpk_solve_LP(
    obj = obj,
    mat = slammat,
    dir = dir,
    rhs = rhs,
    bounds = bounds,
    types = types,
    max = max,
    control = list(
      verbose = verbose,
      presolve = presolve,
      tm_limit = 0
    )
  )
  list(
    optimum = sum(x$solution * obj),
    solution = x$solution,
    status = x$status,
    dual = x$solution_dual
  )
}

# create default parameter objects suitable for primary|secondary suppression
# if selection == 'control.primary': set arguments suitable for primary suppression
# if selection == 'control.secondary': set arguments suitable for secondary suppression
genParaObj <- function(selection, ...) {
  controlPrimary <- function(...) {
    ### setDefaults ###
    paraObj <- list()

    # freq.rule
    paraObj$maxN <- 3
    paraObj$allowZeros <- FALSE

    # p-percent rule
    paraObj$p <- 80

    # n,k rule
    paraObj$n <- 2
    paraObj$k <- 85

    # pq-rule
    paraObj$pq <- c(25, 50)

    paraObj$numVarInd <- NA

    newPara <- list(...)

    indexNumVarIndices <- which(names(newPara) == "numVarIndices")
    if (length(indexNumVarIndices) == 0) {
      stop("genPara (type=='control.primary'): parameter 'numVarIndices' must be specified", call. = FALSE)
    } else {
      numVarIndices <- newPara[[indexNumVarIndices]]
    }

    for (i in seq_along(newPara)) {
      m <- match(names(newPara)[i], names(paraObj))
      if (!is.na(m)) {
        paraObj[[m]] <- newPara[[i]]
      }
    }

    if (!is.logical(paraObj$allowZeros)) {
      stop("genPara (type=='control.primary'): argument 'allowZeros' must be logical!\n")
    }
    if (!all(c(
      is_scalar_integerish(paraObj$maxN),
      is_scalar_integerish(paraObj$p),
      is_scalar_integerish(paraObj$n),
      is_scalar_integerish(paraObj$k)
    ))) {
      stop("arguments `maxN`, `p`, `n` and `k` must be integerish numbers.", call. = FALSE)
    }
    if (length(paraObj$pq) != 2) {
      stop("length of argument 'pq' must equal 2!", call. = FALSE)
    }

    if (paraObj$k < 1 | paraObj$k >= 100) {
      stop("argument `k` must be >= 1 and < 100!", call. = FALSE)
    }

    if (paraObj$p < 1 | paraObj$p >= 100) {
      stop("argument `p` must be >= 1 and < 100!", call. = FALSE)
    }
    if (paraObj$pq[1] < 1 | paraObj$pq[1] >= 100) {
      stop("argument `p` of `pq` must be >= 1 and < 100!", call. = FALSE)
    }
    if (paraObj$pq[2] < 1 | paraObj$pq[2] >= 100) {
      stop("argument `q` of `pq` must be >= 1 and < 100!", call. = FALSE)
    }
    if (paraObj$pq[1] >= paraObj$pq[2]) {
      stop("argument `p` of `pq` must be < argument `q` of `pq`", call. = FALSE)
    }
    if (!is.na(paraObj$numVarInd)) {
      if (!paraObj$numVarInd %in% 1:length(numVarIndices)) {
        l <- length(numVarIndices)
        stop("argument `numVarInd` must be >= 1 and <= ", l, call. = FALSE)
      }
    }
    return(paraObj)
  }

  ### create a parameter list with (...) changing the default-values -> used in protectTable()
  controlSecondary <- function(...) {
    ### setDefaults ###
    paraObj <- list()

    # general parameter
    paraObj$method <- NA
    paraObj$verbose <- FALSE
    paraObj$save <- FALSE
    paraObj$solver <- "glpk"

    # HITAS|OPT - parameter
    paraObj$maxIter <- 10
    paraObj$timeLimit <- NULL
    paraObj$maxVars <- NULL
    paraObj$fastSolution <- FALSE
    paraObj$fixVariables <- TRUE
    paraObj$approxPerc <- 10
    paraObj$useC <- FALSE

    # HYPERCUBE - parameter
    paraObj$protectionLevel <- 80
    paraObj$suppMethod <- "minSupps"
    paraObj$suppAdditionalQuader <- FALSE

    # SIMPLEHEURISTIC - parameter
    paraObj$detectSingletons <- FALSE
    paraObj$threshold <- NA

    # protectLinkedTables
    paraObj$maxIter <- 5

    newPara <- list(...)
    for (i in seq_along(newPara)) {
      m <- match(names(newPara)[i], names(paraObj))
      if (!is.na(m)) {
        paraObj[[m]] <- newPara[[i]]
      }
    }

    ### checks
    if (any(sapply(paraObj, length) != 1)) {
      stop("arguments controlObj for sdc-procedure are not valid!", call. = FALSE)
    }
    if (!all(c(
      is.numeric(paraObj$maxIter),
      is.numeric(paraObj$approxPerc),
      is.numeric(paraObj$protectionLevel),
      is.numeric(paraObj$maxIter)
    ))) {
      e <- "arguments 'maxIter', 'maxIter', 'protectionLevel' and 'maxIter' must be numeric!"
      stop(e, call. = FALSE)
    }
    if (!all(c(
      is.logical(paraObj$verbose),
      is.logical(paraObj$save),
      is.logical(paraObj$fastSolution),
      is.logical(paraObj$fixVariables),
      is.logical(paraObj$suppAdditionalQuader),
      is.logical(paraObj$detectSingletons)
    ))) {
      e <- c(
        "arguments `verbose`, `save`, `fastSolution` `fixVariables`",
        "`suppAdditionalQuader` and `detectSingletons` must be logical!"
      )
      stop(paste(e, collapse = " "), call. = FALSE)
    }
    if (!is.null(paraObj$timeLimit) && !paraObj$timeLimit %in% 1:3000) {
      stop("argument `timeLimit` must be >= 1 and <= 3000 minutes!", call. = FALSE)
    }
    if (!length(paraObj$approxPerc) &
        !paraObj$approxPerc %in% 1:100) {
      stop("argument `approxPerc` must be >= 1 and <= 100!\n", call. = FALSE)
    }
    if (!paraObj$method %in% c("SIMPLEHEURISTIC", "HITAS", "HYPERCUBE", "OPT")) {
      stop("`method` must be either `SIMPLEHEURISTIC`, `HITAS`, `HYPERCUBE` or `OPT`!", call. = FALSE)
    }
    if (!paraObj$suppMethod %in% c('minSupps', 'minSum', 'minSumLogs')) {
      stop("`suppMethod` must be either `minSupps`, `minSum` or `minSumLogs`", call. = FALSE)
    }

    if (!is.na(paraObj$threshold)) {
      if (!rlang::is_scalar_integerish(paraObj$threshold)) {
        stop("argument `threshold` is not an integerish number.", call. = FALSE)
      }
      if (paraObj$threshold < 1) {
        stop("argument `threshold` must be >= 1.", call. = FALSE)
      }
    }
    return(paraObj)
  }

  if (!selection %in% c("control.primary", "control.secondary")) {
    stop("wrong input in argument `selection`.", call. = FALSE)
  }

  if (selection == "control.primary") {
    paraObj <- controlPrimary(...)
  }
  if (selection == "control.secondary") {
    paraObj <- controlSecondary(...)
  }
  return(paraObj)
}

# convert simple triplet to matrix
st_to_mat <- function(x) {
  n.rows <- g_nr_rows(x)
  n.cols <- g_nr_cols(x)
  M <- matrix(0, nrow=n.rows, ncol=n.cols)

  i.x <- g_row_ind(x)
  j.x <- g_col_ind(x)
  v.x <- g_values(x)
  for ( i in 1:g_nr_cells(x) ) {
    M[i.x[i], j.x[i]] <- v.x[i]
  }
  # matrizen from attackers problem are transposed -> switch!
  return(t(M))
}

csp_cpp <- function(sdcProblem, attackonly=FALSE, verbose) {
  pI <- g_problemInstance(sdcProblem)
  dimInfo <- g_dimInfo(sdcProblem)
  aProb <- c_make_att_prob(input=list(objectA=pI, objectB=dimInfo))$aProb

  # already suppressed cells
  ind_prim <- as.integer(sort(c(g_primSupps(pI), g_secondSupps(pI))))
  len_prim <- as.integer(length(ind_prim))
  bounds_min <- bounds_max <- rep(0, len_prim)

  ind_fixed <- as.integer(g_forcedCells(pI))
  len_fixed <- as.integer(length(ind_fixed))

  aProb <- c_make_att_prob(input=list(objectA=pI, objectB=dimInfo))$aProb
  attProbM <- init.simpleTriplet("simpleTriplet", input=list(mat=st_to_mat(aProb@constraints)))

  ia <- as.integer(c(0, g_row_ind(attProbM)))
  ja <- as.integer(c(0, g_col_ind(attProbM)))
  ar <- as.double(c(0, g_values(attProbM)))

  cells_mat <- as.integer(length(ia))
  nr_vars <- as.integer(g_nr_cols(attProbM))
  nr_rows <- as.integer(g_nr_rows(attProbM))

  vals <- as.integer(g_freq(pI))

  lb <- as.double(g_lb(pI))
  ub <- as.double(g_ub(pI))

  LPL <- as.integer(g_LPL(pI))
  UPL <- as.integer(g_UPL(pI))
  SPL <- as.integer(g_SPL(pI))

  if ( attackonly == TRUE ) {
    attackonly <- as.integer(1)
  } else {
    attackonly <- as.integer(0)
  }
  final_pattern <- as.integer(rep(0, length(vals)))
  res <- .C("csp",
    ind_prim=ind_prim,
    len_prim=len_prim,
    bounds_min=bounds_min,
    bounds_max=bounds_max,
    ind_fixed=ind_fixed,
    len_fixed=len_fixed,
    ia=ia,
    ja=ja,
    ar=ar,
    cells_mat=cells_mat,
    nr_vars=nr_vars,
    nr_rows=nr_rows,
    vals=vals,
    lb=lb, ub=ub,
    LPL=LPL,
    UPL=UPL,
    SPL=SPL,
    final_pattern=final_pattern,
    attackonly=attackonly,
    verbose=as.integer(verbose),
    is_ok=0L
  )

  if ( attackonly ) {
    df <- data.frame(prim_supps=res$ind_prim, val=res$vals[res$ind_prim], bounds_low=res$bounds_min, bounds_up=res$bounds_max)
    df$protected <- df$bounds_low <= df$val - LPL[df$prim_supps]  &
    df$bounds_up >=  df$val + UPL[df$prim_supps] &
      df$bounds_up - df$bounds_low >= SPL[df$prim_supps]

    if ( length(g_secondSupps(pI)) > 0 ) {
      index <- g_primSupps(pI)
      df <- df[which(df$prim_supps %in% index),]
    }
    return(df)
  } else {
    if  ( res$is_ok != 0 ) {
      return(NULL)
    } else {
      nr_vars <- g_nrVars(g_problemInstance(sdcProblem))
      status_new <- rep("s", nr_vars)
      status_new[res$final_pattern!=0] <- "x"
      status_new[ind_prim] <- "u"
      if ( length(g_secondSupps(pI)) > 0 ) {
        status_new[g_secondSupps(pI)] <- "x"
      }
      if ( length(ind_fixed) > 0 ) {
        status_new[ind_fixed] <- "z"
      }

      pI <- g_problemInstance(sdcProblem)
      s_sdcStatus(pI) <- list(index=1:nr_vars, vals=status_new)
      s_problemInstance(sdcProblem) <- pI
      s_indicesDealtWith(sdcProblem) <- 1:nr_vars
      return(sdcProblem)
    }
  }
}

# do_singletons: logical --> ordinary singleton detection procedure
# threshold: make sure that in all rows the total amount of contributors is >= threshold_th
detect_singletons <- function(dat, indices, sub_indices, do_singletons, threshold = NA) {
  #.supp_val <- function(dt, dat) {
  .supp_val <- function(dt) {
    tmp <- subset(dt, dt$sdcStatus == "s")
    if (nrow(tmp) == 0) {
      stop("error finding an additional primary suppression (1)", call. = FALSE)
    }
    setorder(tmp, freq, -id)
    supp_id <- tmp$id[1]
    supp_id
  }

  if (do_singletons == FALSE & is.na(threshold)) {
    # nothing to do
    return(invisible(list(
      dat = dat,
      nr_added_supps = 0,
      suppIds = c()
    )))
  }

  id <- freq <- sdcStatus <- NULL
  nr_added_supps <- 0
  supp_ids <- c()

  # temporarily recode primary suppressions and check, if they are really singletons
  id_changed <- dat[sdcStatus == "u" & freq > 1, id]
  if (length(id_changed) > 0) {
    dat[id_changed, sdcStatus := "x"]
  }

  is_singleton <- is_primsupp <- is_suppressed <- NULL
  dat$is_singleton <- FALSE
  dat[sdcStatus == "u" & freq == 1, is_singleton := TRUE]
  dat[, is_suppressed := sdcStatus %in% c("u", "x")]
  dat[, is_primsupp := sdcStatus == "u"]

  for (i in 1:length(indices)) {
    sI <- sub_indices[[i]]
    for (j in 1:length(sI)) {
      sJ <- unique(sI[[j]]) # only unique subtables need to be covered!
      for (z in 1:length(sJ)) {
        poss <- sJ[[z]]
        mm <- max(poss)
        for (k in 1:mm) {
          ii <- indices[[i]][[j]][which(poss == k)]
          # only if we have a real subtable (more than 1 cell)
          # that is populated (freqs > 0) and not fully suppressed
          ss <- dat[ii]
          fully_supped <- sum(ss$freq[!ss$sdcStatus %in% c("u", "x")]) == 0

          if (length(ii) > 1 & max(ss$freq) > 0 & !fully_supped) {
            if (do_singletons) {
              # tau-argus strategy
              nr_supps <- sum(ss$is_suppressed)
              nr_singletons <- sum(ss$is_singleton)
              if (nr_supps == 2 & nr_singletons > 0) {
                # either two singletons or one singleton and exactly one additional suppression
                # we need to add one additional suppression
                supp_id <- .supp_val(dt = ss)
                nr_added_supps <- nr_added_supps + 1
                supp_ids <- c(supp_ids, supp_id)
                dat$sdcStatus[supp_id] <- "u"
                dat$is_suppressed[supp_id] <- TRUE
              }
              # 3. If a frequency rule is used, it could happen that two cells on a row/column are
              # primary unsafe, but the sum of the two cells could still be unsafe. In that case
              # it should be prevented that these two cells protect each other.
              nr_primsupps <- sum(ss$is_primsupp)
              if (nr_primsupps == 3) {
                ss <- dat[ii]
                # the sum is primary suppressed, thus the other two
                # primary suppressions are within the row/col
                if (ss$sdcStatus[1] == "u" & sum(ss$freq > 0) > 3) {
                  # we need to find an additional suppression
                  supp_id <- .supp_val(dt = ss)
                  nr_added_supps <- nr_added_supps + 1
                  supp_ids <- c(supp_ids, supp_id)
                  dat$sdcStatus[supp_id] <- "u"
                  dat$is_suppressed[supp_id] <- TRUE
                }
              }
            }

            # respect threshold for rows with suppressions
            if (!is.na(threshold)) {
              ss <- dat[ii]
              nr_supps <- sum(ss$is_suppressed)
              obs_supp <- sum(ss$freq[ss$is_suppressed])
              # only if we have suppressions
              finished <- nr_supps == 0 | obs_supp >= threshold
              # suppress as many cells as required
              while (!finished) {
                ss <- dat[ii]
                ind_supps <- ss$sdcStatus %in% c("u", "x")
                # already fully suppressed?
                fully_supped <- sum(ss$freq[!ss$is_suppressed]) == 0
                if (!fully_supped & sum(ss$freq[ss$is_suppressed]) < threshold) {
                  supp_id <- .supp_val(dt = ss)
                  nr_added_supps <- nr_added_supps + 1
                  supp_ids <- c(supp_ids, supp_id)
                  dat$sdcStatus[supp_id] <- "u"
                  dat$is_suppressed[supp_id] <- TRUE
                } else {
                  finished <- TRUE
                }
              }
            }
          }
        }
      }
    }
  }

  # reset primary suppressions
  if (length(id_changed) > 0) {
    dat[id_changed, sdcStatus := "u"]
  }
  invisible(list(
    dat = dat,
    nr_added_supps = nr_added_supps,
    suppIds = supp_ids
  ))
}
