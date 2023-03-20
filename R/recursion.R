
#' Create Recursive Models
#'
#' @param var the state variable (e.g., q)
#' @param expr a mathematical expression.
#'
#' @return a function of class "rfun."
#' @export
#'
#' @examples
#' biased_transmission <- recursion(var = q, expr = q + q*(1 - q)*B)
#' biased_transmission
#'
recursion <- function(var, expr) {
  expr <- substitute(expr)
  sv <- as.character(substitute(var))
  f <- make_rfun(sv, expr)
  rfun <- function(params, init = 0, tn = 20) {
    validate_recursion_env(environment(), environment(f))
    list2env(params, envir = environment(f))
    out <- purrr::accumulate(1:tn, f, .init = init)
    return(structure(tibble::tibble(t = 0:tn, !!sv := out), params = params))
  }
  return(structure(rfun, class = c("rfun", "function")))
}

make_rfun <- function(sv, expr) {
  args <- purrr::set_names(vector("list", 3), c(sv, "t", "..."))
  f <- function() eval(expr)
  formals(f) <- args
  return(f)
}

#' @export
#'
print.rfun <- function(x, ...) {
  env <- environment(x)
  eq <- env[["expr"]]
  params <- extract_par_names(eq)
  i <- which(params %in% c(env[["sv"]], "t"))
  cat("Recursive equation:\n")
  cat(paste0(env[["sv"]], "' = ", deparse(eq)), "\n\n")
  cat("Required parameters:\n")
  cat(params[-i], sep = ", ")
}

#' Run Recursive Model with Multiple Parameters
#'
#' @param grid a data frame of parameter values
#' @param model a function of class "rfun."
#' @param init the initial value for "q" (default is zero).
#' @param tn the number of time periods after q_0
#'
#' @return a data frame with parameters t, q, .id, and whatever parameters where used
#' @export
#'
#' @examples
#'
#' env_learn <- recursion(
#'   var = q,
#'   expr = P1 + L*q
#' )
#'
#' env_learn
#'
#' grid <- expand.grid(P1 = seq(0, 0.5, by  = 0.1), L = seq(0.5, 1, by = 0.1))
#'
#' multi_par_call(grid, env_learn)
#'
multi_par_call <- function(grid, model, init = 0, tn = 20) {

  if (!("data.frame" %in% class(grid))) stop("grid must be a data frame.")
  if (!("rfun" %in% class(model))) stop("model must be of class \"rfun.\"")

  parameter_list <- split(grid, row.names(grid))

  multi_out <- purrr::map(
    .x = parameter_list,
    .f = function(x) model(x, init, tn)
  )

  out <- purrr::map2(multi_out, parameter_list, dplyr::bind_cols)
  out <- dplyr::bind_rows(out, .id = ".id")
  out[[".id"]] <- as.factor(out[[".id"]])
  attr(out, "params") <- NULL
  return(out)
}



