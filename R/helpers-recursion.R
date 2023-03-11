
recursion <- function(expr) {
  expr <- substitute(expr)
  fun <- function(params, q_init = 0, tn = 20, ...) {
    safety_check(environment(), expr)
    list2env(params, env = environment())
    out <- purrr::accumulate(
      .x = 1:tn,        ## sequence of time periods
      .init = q_init,   ## initial value for q
      .f = function(q,...) eval(expr)
    )
    return(structure(data.frame(t = 0:tn, q = out), params = params))
  }
  structure(fun, class = c("recursion", "function"))
}


print.recursion <- function(x, ...) {
  eq <- environment(x)[["expr"]]
  cat("Equation:\n")
  cat("q' =", paste0(deparse(eq), sep = "\n"))
}


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

  return(dplyr::bind_rows(out, .id = ".g"))
}

get_expression_tree <- function(expr) {
  purrr::map_if(as.list(expr), is.call, get_expression_tree)
}

get_leafs <- function(tree) {
  purrr::map_if(tree[-1], is.list, get_leafs)
}

extract_names <- function(expr) {
  tree <- get_expression_tree(expr)
  s <- get_leafs(tree)
  s <- unique(unlist(s))
  i <- purrr::map_lgl(s, is.symbol)
  return(as.character(s[i]))
}

safety_check <- function(e, expr) {
  par_names <- extract_names(expr)
  ok <- par_names %in% names(append(e[["params"]], list(q = NULL)))
  if (!all(ok)) {
    stop("Missing Parameters: ", paste(par_names[!ok], collapse = ", "), call. = FALSE)
  }
  if ("q" %in% names(e[["params"]])) {
    stop("\"params\" can't contain an object named \"q\"", call. = FALSE)
  }
  if (e[["tn"]] %% 1 != 0 | e[["tn"]] < 0) {
    stop("\"tn\" must be a positive integer", call. = FALSE)
  }
  if (e[["q_init"]] < 0 | e[["q_init"]] > 1) {
    stop("\"q_init\" must not be outside the [0, 1] range", call. = FALSE)
  }
}
