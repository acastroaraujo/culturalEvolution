
#' Create Recursive Models
#'
#' @param var The state variable or "trait" proportion (e.g., q). You are not allowed to choose "t",
#'   which is reserved for keeping track of time.
#' @param expr A recursive mathematical expression.
#'
#' @return A function of class "rfun", with the following arguments:
#'
#' \describe{
#'   \item{params}{A list or data frame of parameter values.}
#'   \item{init}{The initial value for the state variable (defaults to zero).}
#'   \item{tn}{The number of time periods.}
#' }
#'
#' @export
#'
#' @examples
#' biased_transmission <- recursion(var = q, expr = q + q*(1 - q)*B)
#' biased_transmission
#'
recursion <- function(var, expr) {
  sv <- as.character(substitute(var))
  if (sv == "t") stop(call. = FALSE, "\'t\' isn't allowed as state variable.")
  expr <- substitute(expr)
  fexpr <- make_rfun(sv, expr)
  rfun <- function(params, init = 0, tn = 20) {
    list2env(params, envir = environment(fexpr))
    out <- purrr::accumulate(1:tn, fexpr, .init = init)
    return(structure(tibble::tibble(t = 0:tn, !!sv := out)))
  }

  out <- function(params, init = 0, tn = 20) {
    validate_recursion_env(environment(), environment(fexpr))
    params <- as.data.frame(params)
    multi_par_call(params, rfun, init, tn)
  }

  return(structure(out, class = c("rfun", "function"), expr = expr))
}

make_rfun <- function(sv, expr) {
  args <- purrr::set_names(vector("list", 3), c(sv, "t", "..."))
  f <- function() eval(expr)
  formals(f) <- args
  return(f)
}

multi_par_call <- function(df, rfun, init = 0, tn = 20) {
  parameter_list <- split(df, row.names(df))
  multi_out <- purrr::map(parameter_list, function(x) rfun(x, init, tn))
  out <- purrr::map2(multi_out, parameter_list, dplyr::bind_cols)
  out <- dplyr::bind_rows(out, .id = ".id")
  out[[".id"]] <- as.factor(out[[".id"]])
  return(out)
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



