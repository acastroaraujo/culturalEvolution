
#' Create Recursive Models
#'
#' @param expr a mathematical expression
#'
#' @return a function of class "recursion."
#' @export
#'
#' @examples
#' biased_transmission <- recursion(expr = q + q*(1 - q)*B)
#' biased_transmission
#'
recursion <- function(expr) {
  expr <- substitute(expr)
  fun <- function(params, q_init = 0, tn = 20, ...) {
    validate_recursion_env(environment(), expr)
    list2env(params, envir = environment())
    out <- purrr::accumulate(
      .x = 1:tn,        ## sequence of time periods
      .init = q_init,   ## initial value for q
      .f = function(q,...) eval(expr)
    )
    return(structure(data.frame(t = 0:tn, q = out), params = params))
  }
  structure(fun, class = c("recursion", "function"))
}

#' @export
#'
print.recursion <- function(x, ...) {
  eq <- environment(x)[["expr"]]
  cat("Equation:\n")
  cat("q' =", paste0(deparse(eq), sep = "\n"))
}


#' Run Recursive Model with Multiple Parameters
#'
#' @param grid a data frame of parameter values
#' @param model a function of class "recursion."
#' @param q_init the initial value for "q" (default is zero).
#' @param tn the number of time periods after q_0
#'
#' @return a data frame with parameters t, q, .id, and whatever parameters where used
#' @export
#'
#' @examples
#'
#' env_learn <- recursion(
#'   expr = P1 + L*q
#' )
#'
#' env_learn
#'
#' grid <- expand.grid(P1 = seq(0, 0.5, by  = 0.1), L = seq(0.5, 1, by = 0.1))
#'
#' multi_par_call(grid, env_learn)
#'
multi_par_call <- function(grid, model, q_init = 0, tn = 20) {

  if (!("data.frame" %in% class(grid))) stop("grid must be a data frame.")
  if (!("recursion" %in% class(model))) stop("model must be of class \"recursion.\"")

  parameter_list <- split(grid, row.names(grid))

  multi_out <- purrr::map(
    .x = parameter_list,
    .f = function(x) {
      model(x, q_init, tn)
    }
  )
  out <- purrr::map2(multi_out, parameter_list, dplyr::bind_cols)
  out <- dplyr::bind_rows(out, .id = ".id")
  out[[".id"]] <- as.factor(out[[".id"]])
  attr(out, "params") <- NULL
  return(out)
}

get_expression_tree <- function(expr) {
  purrr::map_if(as.list(expr), is.call, get_expression_tree)
}

get_leafs <- function(tree) {
  purrr::map_if(tree[-1], is.list, get_leafs)
}

extract_par_names <- function(expr) {
  tree <- get_expression_tree(expr)
  s <- get_leafs(tree)
  s <- unique(unlist(s))
  i <- purrr::map_lgl(s, is.symbol)
  return(as.character(s[i]))
}

validate_recursion_env <- function(e, expr) {
  par_names <- extract_par_names(expr)
  ok <- par_names %in% names(append(e[["params"]], list(q = NULL)))
  if ("q" %in% names(e[["params"]])) {
    stop("The \"params\" list cannot contain an object named \"q\"", call. = FALSE)
  }
  if (!all(ok)) {
    stop("Missing Parameters: ", paste(par_names[!ok], collapse = ", "), call. = FALSE)
  }
  if (e[["tn"]] %% 1 != 0 | e[["tn"]] < 0) {
    stop("\"tn\" must be a positive integer", call. = FALSE)
  }
  if (e[["q_init"]] < 0 | e[["q_init"]] > 1) {
    stop("\"q_init\" must not be outside the [0, 1] range", call. = FALSE)
  }
}
