
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

validate_recursion_env <- function(env0, env1) {

  expr_names <- c(names(env0[["params"]]), env1[["sv"]], "t")
  par_names <- extract_par_names(env1[["expr"]])
  ok <- par_names %in% expr_names

  if (env1[["sv"]] %in% names(env0[["params"]])) {
    stop("the parameter list cannot contain an object named ", env1[["sv"]], call. = FALSE)
  }
  if ("t" %in% names(env0[["params"]])) {
    stop("the parameter list cannot contain an object named t", call. = FALSE)
  }
  if (!all(ok)) {
    stop(paste(par_names[!ok], collapse = ", "), " missing from the parameter list", call. = FALSE)
  }
  if (env0[["tn"]] %% 1 != 0 || env0[["tn"]] < 1) {
    stop("\"tn\" must be a positive integer", call. = FALSE)
  }
  if (env0[["init"]] < 0 || env0[["init"]] > 1) {
    stop("\"init\" must not be outside the [0, 1] range", call. = FALSE)
  }
}
