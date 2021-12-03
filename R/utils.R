domRule <- function(object, params, type) {
  p_rule <- function(params) {
    stopifnot(is_scalar_double(params$cell_tot))

    cont1 <- params$top_contr[1]
    cont2 <- params$top_contr[2]
    stopifnot(is_scalar_double(cont1))
    stopifnot(is_scalar_double(cont2))
    stopifnot(is_scalar_integerish(params$p))
    stopifnot(params$p > 0, params$p <= 100)

    # if TRUE, cell needs to be suppressed
    (params$cell_tot - sum(cont1, cont2)) < (params$p / 100 * cont1)
  }
  pq_rule <- function(params) {
    stopifnot(is_scalar_double(params$cell_tot))
    cont1 <- params$top_contr[1]
    cont2 <- params$top_contr[2]
    stopifnot(is_scalar_double(cont1))
    stopifnot(is_scalar_double(cont2))
    stopifnot(is_integerish(params$pq))
    p <- params$pq[1]
    q <- params$pq[2]

    # if TRUE, cell needs to be suppressed
    (params$cell_tot - sum(cont1, cont2)) < (p / q) * cont1
  }
  nk_rule <- function(params) {
    stopifnot(is_scalar_double(params$cell_tot))
    stopifnot(is_scalar_integerish(params$n))
    # if TRUE, cell needs to be suppressed
    sum(params$top_contr) > (params$k / 100 * params$cell_tot)
  }

  if (type == "p") {
   fun <- p_rule
  }
  if (type == "pq") {
    fun <- pq_rule
  }
  if (type == "nk") {
    fun <- nk_rule
  }

  # compute inputs (cell total and top two contributing) units
  # for a given vector of cell values and weights `w`
  # values are replicated times their weights which are randomly
  # rounded to integers in case they are floating numbers
  .comp_weighted_inputs <- function(vals, w, n) {
    if (length(vals) == 0) {
      return(NULL)
    }

    # replicate by weights: what to do with non-integerish weights?
    # randomly round upwards and downwards?
    if (!is_integerish(w)) {
      dir <- sample(c(-1, 1), length(vals), replace = TRUE)
      w[dir == -1] <- floor(w[dir == -1])
      w[dir == 1] <- ceiling(w[dir == 1])
    }
    # division is required here because in makeProblem()
    # all numVars-variables are aggregated on cell-level
    vals <- rep(vals / w, times = w)

    top_contr <- rep(0, n)
    v <- rev(tail(sort(vals), n))
    top_contr[1:length(v)] <- v
    list(
      cell_tot = sum(vals),
      top_contr = top_contr
    )
  }

  if (!is_scalar_character(type)) {
    stop("`type` needs to be a scalar character.", call. = FALSE)
  }
  if (!type %in% c("pq", "p", "nk")) {
    stop("`type` needs to be either `pq`, `p` or `nk`.", call. = FALSE)
  }

  if (!g_is_microdata(g_dataObj(object))) {
    e <- "dominance rules can only be applied if micro-data are available!"
    stop(e, call. = FALSE)
  }

  if (type %in% c("p", "pq")) {
    n <- 2
  } else {
    n <- params$n
    if (n < 1) {
      stop("Parameter `n` must be >= 1 for nk-dominance rule.", call. = FALSE)
    }
  }

  pI <- g_problemInstance(object)
  dataObj <- g_dataObj(object)
  strIDs <- g_strID(pI)
  raw_data <- g_raw_data(dataObj)
  numVal <- raw_data[[params$numVarName]]

  if (any(na.omit(numVal) < 0)) {
    e <- c(
      "dominance rules can only be applied to numeric variables",
      "with only positive values!"
    )
    stop(paste(e, collapse = " "), call. = FALSE)
  }

  # sampweights
  samp_weights <- g_sampweight_ind(dataObj)
  if (!is.null(samp_weights)) {
    samp_weights <- slot(dataObj, "rawData")[[samp_weights]]
  } else {
    samp_weights <- rep(1, length(numVal))
  }

  # calculate contributing indices
  nr_cells <- g_nrVars(pI)

  # we check if we need to compute contributing indices
  indices <- attributes(raw_data)[["computed_indices"]]
  if (is.null(indices)) {
    # indices of contributing units have not yet been computed
    indices <- contributing_indices(
      prob = object,
      ids = strIDs
    )
    attr(raw_data, "computed_indices") <- indices
    object@dataObj@rawData <- raw_data
    object <<- object
  }

  # values and totals of contributing units
  inp <- lapply(1:nr_cells, function(x) {
    ii <- indices[[x]]
    .comp_weighted_inputs(
      vals = numVal[ii],
      w = samp_weights[ii],
      n = n
    )
  })

  # suppStatus: TRUE: unsafe, FALSE: safe
  supp_state <- sapply(1:nr_cells, function(x) {
    inp <- inp[[x]]
    # cells with value 0!
    if (is.null(inp)) {
      FALSE
    } else {
      fun(params = append(params, inp))
    }
  })

  # in case of dominance rules; empty cells (frequency = 0)
  # are never marked as sensitive
  supp_index <- which(supp_state == TRUE & g_freq(pI) > 0)
  if (length(supp_index) > 0) {
    s_sdcStatus(pI) <- list(
      index = supp_index,
      vals = rep("u", length(supp_index))
    )
  }
  s_problemInstance(object) <- pI
  validObject(object)
  return(object)
}

# returns all contributing codes for each dimensions
# of a sdcProblem-object; dimensions are stored as attributes
# in sdcProblem-objects; thus we can use sdcHierarchies::hier_info()
# this is used in createRegSDCInput() and contributing_indices()
.get_all_contributing_codes <- function(x) {
  stopifnot(inherits(x, "sdcProblem"))
  dims_hier <- attributes(x)$hierinfo

  dims_info <- lapply(dims_hier, function(x) {
    sdcHierarchies::hier_info(x)
  })

  all_contr_codes <- lapply(dims_info, function(x) {
    lapply(x, function(y) {
      list(
        is_root = y$is_rootnode,
        contr_codes = y$contributing_codes
      )
    })
  })
  all_contr_codes
}

.tmpweightname <- function() {
  "weight.for.suppression"
}

# returns a relevant row from a protected dataset (slot "results") or
# the current status (from "problemInstance") depending on the inputs
# specs is a named vector and complete allows to return the entire row
# or only the id
cell_id <- function(x, specs, complete = TRUE, addDups = FALSE) {
  stopifnot(inherits(x, "sdcProblem"))
  df <- slot(x, "results")
  if (is.null(df)) {
    df <- sdcProb2df(
      obj = x,
      dimCodes = "original",
      addNumVars = TRUE,
      addDups = addDups
    )
  }

  stopifnot(rlang::is_scalar_logical(complete))

  # check inputs
  if (!rlang::is_named(specs)) {
    stop("input `specs` must be a named character vector", call. = FALSE)
  }
  if (!rlang::is_character(specs)) {
    stop("input `specs` must be a named character vector", call. = FALSE)
  }

  di <- slot(x, "dimInfo")
  vnames <- slot(di, "vNames")

  if (length(specs) != length(vnames)) {
    stop("length of input `specs` does not match number of dimensions.", call. = FALSE)
  }

  idx <- match(names(specs), vnames)
  if (any(is.na(idx))) {
    stop("names of input `specs` does not match names of dimensions.", call. = FALSE)
  }

  specs <- specs[idx]
  df$id <- 1:nrow(df)
  for (v in vnames) {
    df <- df[get(v) == specs[v]]
  }
  if (nrow(df) != 1) {
    stop("0 or > 1 cells identified, check inputs `specs`", call. = FALSE)
  }
  if (complete) {
    return(df)
  } else {
    return(df$id)
  }
}

# computes the constraint-matrix (m) for a given problem instance;
# this replaces the old method `c_gen_m` available in versions <= 0.32
# convert: if TRUE; a (internal) simpleTripet matrix is returned;
# else a simple-triplet matrix in the format from the slam pkg
# add_info_df: if true, an attribute "infodf" is added containing
# a data.frame with all cell ids and TRUE|FALSE depending on wheather
# the cell is an internal cell or not
create_m_matrix <- function(obj, convert = TRUE, add_info_df = FALSE) {
  stopifnot(inherits(obj, "sdcProblem"))
  stopifnot(rlang::is_scalar_logical(convert))
  stopifnot(rlang::is_scalar_logical(add_info_df))

  .constraints_for_single_dim <- function(full_dt, dn, current_dim) {
    dl <- slot(current_dim, "dims")
    dname <- slot(current_dim, "vName")

    kv <- c(setdiff(dn, dname), dname)
    setkeyv(full_dt, kv)

    m <- slam::simple_triplet_zero_matrix(nrow = 0, ncol = nrow(full_dt))
    for (i in seq_len(length(dl))) {
      tmp <- full_dt[get(dname) %in% dl[[i]]]

      nr_elements <- length(dl[[i]])
      nr_constraints <- nrow(tmp) / nr_elements

      # rows
      v1 <- rep(1, nr_elements)
      v2 <- c(-1, rep(1, (nr_elements - 1)))
      m <- rbind(m, slam::simple_triplet_matrix(
        i = rep(seq_len(nr_constraints), each = nr_elements),
        j = tmp$id,
        v = rep(v2, times = nr_constraints),
        nrow = nr_constraints,
        ncol = nrow(full_dt)
      ))
    }
    m
  }

  full_dt <- sdcProb2df(obj, addDups = FALSE)
  full_dt$id <- 1:nrow(full_dt)
  str_ids <- full_dt$strID

  dim_info <- slot(obj, "dimInfo")
  dim_names <- slot(dim_info, "vNames")
  dims <- slot(dim_info, "dimInfo")

  res <- lapply(seq_len(length(dims)), function(x) {
    .constraints_for_single_dim(
      full_dt = full_dt,
      dn = dim_names,
      current_dim = dims[[x]]
    )
  })
  mat <- do.call("rbind", res)


  if (add_info_df) {
    colnames(mat) <- str_ids
    index_subtots <- unique(mat$j[mat$v == -1])
    infodf <- data.frame(
      str_id = colnames(mat),
      is_inner = TRUE
    )
    infodf$is_inner[index_subtots] <- FALSE
    attr(mat, "infodf") <- infodf
  }

  if (!convert) {
    return(mat)
  }

  st <- new("simpleTriplet")
  st@i <- mat$i
  st@j <- mat$j
  st@v <- mat$v
  st@nrRows <- mat$nrow
  st@nrCols <- mat$ncol
  if (add_info_df) {
    attr(st, "infodf") <- infodf
  }
  return(st)
}
