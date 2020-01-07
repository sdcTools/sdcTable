#' @aliases get.linProb,linProb,character-method
#' @rdname get.linProb-method
setMethod(f="get.linProb", signature=c("linProb", "character"),
  definition=function(object, type) {
    if ( !type %in% c("constraints", "direction", "rhs", "objective", "types", "bounds") ) {
      stop("get.cutList:: argument 'type' is not valid!\n")
    }
    if ( type == "constraints" ) {
      return(g_constraints(object))
    }
    if ( type == "direction" ) {
      return(g_direction(object))
    }
    if ( type == "rhs" ) {
      return(g_rhs(object))
    }
    if ( type == "objective" ) {
      return(g_objective(object))
    }
    if ( type == "types" ) {
      return(g_types(object))
    }
    if ( type == "bounds" ) {
      return(g_bounds(object))
    }
  }
)

#' @aliases set.linProb,linProb,character,list-method
#' @rdname set.linProb-method
setMethod(f="set.linProb", signature=c("linProb", "character", "list"),
  definition=function(object, type, input) {
    if ( !type %in% c("objective", "direction", "rhs", "types",
        "removeCompleteConstraint", "addCompleteConstraint",
        "bounds", "constraints") ) {
      stop("set.linProb:: check argument 'type'!\n")
    }
    if ( type == "objective" ) {
      s_objective(object) <- input
    }
    if ( type == "direction" ) {
      s_direction(object) <- input
    }
    if ( type == "rhs" ) {
      s_rhs(object) <- input
    }
    if ( type == "types" ) {
      s_types(object) <- input
    }
    if  ( type == "removeCompleteConstraint" ) {
      s_remove_complete_constraint(object) <- input
    }
    if ( type == "addCompleteConstraint" ) {
      s_add_complete_constraint(object) <- input
    }
    if ( type == "bounds" ) {
      s_bounds(object) <- input
    }
    if ( type == "constraints" ) {
      s_constraints(object) <- input
    }
    validObject(object)
    return(object)
  }
)

#' @aliases calc.linProb,linProb,character,list-method
#' @rdname calc.linProb-method
setMethod(f="calc.linProb", signature=c("linProb", "character", "list"),
  definition=function(object, type, input) {
    if ( !type %in% c("solveProblem", "fixVariables") ) {
      stop("calc.linProb:: check argument 'type'!\n")
    }

    if ( type == "solveProblem" ) {
      return(c_solve_problem(object, input))
    }
    if ( type == "fixVariables" ) {
      return(c_fix_variables(object, input))
    }
  }
)

setMethod(f="g_constraints", signature=c("linProb"), definition=function(object) {
  return(object@constraints)
})

setMethod(f="g_direction", signature=c("linProb"), definition=function(object) {
  return(object@direction)
})

setMethod(f="g_rhs", signature=c("linProb"), definition=function(object) {
  return(object@rhs)
})

setMethod(f="g_objective", signature=c("linProb"), definition=function(object) {
  return(object@objective)
})

setMethod(f="g_types", signature=c("linProb"), definition=function(object) {
  return(object@types)
})

setMethod(f="g_bounds", signature=c("linProb"), definition=function(object) {
  return(list(upper=object@boundsUpper, lower=object@boundsLower))
})

setReplaceMethod(f="s_objective", signature=c("linProb", "list"), definition=function(object, value) {
  object@objective <- value[[1]]
  return(object)
})

setReplaceMethod(f="s_direction", signature=c("linProb", "list"), definition=function(object, value) {
  object@direction <- value[[1]]
  return(object)
})

setReplaceMethod(f="s_rhs", signature=c("linProb", "list"), definition=function(object, value) {
  object@rhs <- value[[1]]
  return(object)
})

setReplaceMethod(f="s_types", signature=c("linProb", "list"), definition=function(object, value) {
  object@types <- value[[1]]
  return(object)
})

setReplaceMethod(f="s_bounds", signature=c("linProb", "list"), definition=function(object, value) {
  # FIXME: check bounds input (lower|upper,...)
  object@boundsLower <- value$lower
  object@boundsUpper <- value$upper
  return(object)
})

setReplaceMethod(f="s_constraints", signature=c("linProb", "list"), definition=function(object, value) {
  object@constraints <- value[[1]]
  return(object)
})

setReplaceMethod(f="s_remove_complete_constraint", signature=c("linProb", "list"), definition=function(object, value) {
  input <- value[[1]]
  if ( !all(input %in% 1:length(g_rhs(object))) ) {
    stop("s_remove_complete_constraint:: elements of argument 'input' must be >=1 and <=",length(g_rhs(object)),"!\n")
  }
  object@constraints <- c_remove_row(g_constraints(object), input=list(input))
  object@direction <- g_direction(object)[-input]
  object@rhs <- g_rhs(object)[-input]
  return(object)
})

setReplaceMethod(f="s_add_complete_constraint", signature=c("linProb", "list"), definition=function(object, value) {
  input <- value[[1]]
  if ( g_nr_cols(g_constraints(object)) != g_nr_cols(g_constraints(input)) ) {
    stop("s_add_complete_constraint:: nrCols of 'object' and 'input' differ!\n")
  }
  if ( g_nr_constraints(input) > 0 ) {
    con <- g_constraints(input)
    for ( k in 1:g_nr_rows(con) ) {
      x <- g_row(con, input=list(k))
      object@constraints <- c_add_row(g_constraints(object), input=list(index=g_col_ind(x), values=g_values(x)))
    }
    object@direction <- c(g_direction(object), g_direction(input))
    object@rhs <- c(g_rhs(object), g_rhs(input))
  }
  return(object)
})

setMethod(f = "c_solve_problem", signature = c("linProb", "list"), definition = function(object, input) {
  solver <- input[[1]]
  if (!solver %in% c("glpk")) {
    stop("`solver` needs to be `glpk` for now", call. = FALSE)
  }
  if (solver == "glpk") {
    sol <- my.Rglpk_solve_LP(
      g_objective(object),
      g_constraints(object),
      g_direction(object),
      g_rhs(object),
      g_types(object),
      max = FALSE,
      bounds = g_bounds(object),
      verbose = FALSE
    )
  }
  return(sol)
})

setMethod(f = "c_fix_variables", signature = c("linProb", "list"), definition = function(object, input) {
  lb <- input[[1]]
  ub <- input[[2]]
  prim_supps <- input[[3]]

  if (length(lb) != 1 | length(ub) != 1) {
    stop("c_fix_variables:: length of arguments 'lb' and 'ub' must equal 1!", call. = FALSE)
  }
  if (!ub > lb) {
    stop("c_fix_variables:: arguments 'ub' must be >= argument 'lb'!", call. = FALSE)
  }

  mat <- g_constraints(object)
  nr_vars <- g_nr_cols(mat)
  res_glpk <- my.Rglpk_solve_LP(
    obj = g_objective(object),
    mat = mat,
    dir = g_direction(object),
    rhs = g_rhs(object),
    types = g_types(object),
    bounds = g_bounds(object),
    max = FALSE)

  dual <- res_glpk$dual
  if (length(dual) != nr_vars) {
    stop("c_fix_variables:: length of arguments does not match!", call. = FALSE)
  }
  freqs <- g_objective(object)
  sol <- res_glpk$solution
  sol[is.zero(sol)] <- 0
  sol[is.one(sol)] <- 1

  # calculate reduced costs from dual solution
  reduced_costs <- freqs
  dual_ind <- which(dual != 0)
  if (length(dual_ind) > 0) {
    reduced_costs[dual_ind] <- sapply(dual_ind, function(x) {
      min(freqs[x], dual[x])
    })
  }

  bas <- which(sol != 0)
  reduced_costs[bas] <- 0
  # end calculation of reduced costs

  # which variables could be set to zero?
  ind_zero <- which(lb + reduced_costs >= ub)

  # do not fix primary suppressions to zero!
  ind_zero <- ind_zero[-which(ind_zero %in% prim_supps)]
  return(ind_zero)
})

