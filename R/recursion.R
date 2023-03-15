
#' Create Recursive Models
#'
#' @param expr a mathematical expression.
#'
#' @return a function of class "rfun."
#' @export
#'
#' @examples
#' biased_transmission <- recursion(expr = q + q*(1 - q)*B)
#' biased_transmission
#'
recursion <- function(expr) {
  expr <- substitute(expr)
  par_names <- extract_par_names(expr)
  fun <- function(params, q_init = 0, tn = 20) {
    validate_recursion_env(environment(), par_names)
    list2env(params, envir = environment())
    out <- purrr::accumulate(
      .x = 1:tn,        ## sequence of time periods
      .init = q_init,   ## initial value for q
      .f = function(q, t, ...) eval(expr)
    )
    return(structure(data.frame(t = 0:tn, q = out), params = params))
  }
  structure(fun, class = c("rfun", "function"))
}

#' @export
#'
print.rfun <- function(x, ...) {
  eq <- environment(x)[["expr"]]
  params <- environment(x)[["par_names"]]
  i <- which(params %in% c("q", "t"))
  cat("Recursive equation:\n")
  cat("q' =", paste0(deparse(eq), sep = "\n\n"))
  cat("Required parameters:\n")
  cat(params[-i], sep = ", ")
}


#' Run Recursive Model with Multiple Parameters
#'
#' @param grid a data frame of parameter values
#' @param model a function of class "rfun."
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
  if (!("rfun" %in% class(model))) stop("model must be of class \"rfun.\"")

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

validate_recursion_env <- function(env, par_names) {

  ok <- par_names %in% names(append(env[["params"]], list(q = NULL, t = NULL)))
  if ("q" %in% names(env[["params"]])) {
    stop("The \"params\" list cannot contain an object named \"q\"", call. = FALSE)
  }
  if ("t" %in% names(env[["params"]])) {
    stop("The \"params\" list cannot contain an object named \"t\"", call. = FALSE)
  }
  if (!all(ok)) {
    stop("Missing Parameters: ", paste(par_names[!ok], collapse = ", "), call. = FALSE)
  }
  if (env[["tn"]] %% 1 != 0 | env[["tn"]] < 0) {
    stop("\"tn\" must be a positive integer", call. = FALSE)
  }
  if (env[["q_init"]] < 0 | env[["q_init"]] > 1) {
    stop("\"q_init\" must not be outside the [0, 1] range", call. = FALSE)
  }
  if (any(purrr::map_lgl(env[["params"]], \(x) length(x) != 1))) {
    stop("Each value in \"params\" must be of length 1", call. = FALSE)
  }

}
