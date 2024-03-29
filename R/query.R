#' Create a query
#'
#' @param query
#' an R call. The R call consists of symbols, integers and real numbers,
#' character strings, boolean values, expressions, lists, and other calls.
#' Vectors of booleans, integers, floating point numbers, and strings with
#' length _N_ > 1 are translated to prolog compounds !/N, %/N, #/N and $$/N,
#' respectively. The names can be modified with the options below.
#'
#' @param options
#' This is a list of options controlling translation from and to prolog.
#' * _boolvec_ (see option rolog.boolvec, default is !) is the name of the
#'   prolog compound for vectors of booleans.
#' * _intvec_, _realvec_, _charvec_ define the compound names for vectors of
#'   integers, doubles and strings, respectively (defaults are %, # and $$).
#' * If _scalar_ is `TRUE` (default), vectors of length 1 are translated to
#'   scalar prolog elements. If _scalar_ is `FALSE`, vectors of length 1 are
#'   also translated to compounds.
#'
#' @param env
#' The R environment in which the query is run (default: globalenv()). This is
#' mostly relevant for r_eval/2.
#' 
#' @return
#' If the creation of the query succeeds, `TRUE`.
#'
#' @details
#' SWI-Prolog does not allow multiple open queries. If another query is open, it
#' it is closed and a warning is shown.
#'
#' @md
#'
#' @seealso [once()] for a query that is submitted only a single time.
#'
#' @seealso [findall()] for a query that is submitted until it fails.
#'
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))))
#' submit() # X = a
#' submit() # X = "b"
#' clear()
#'
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y),
#'   NA, NaN, Inf, NULL, function(x) {y <- sin(x); y^2})))
#' submit() # X = a
#' submit() # X = "b"
#' submit() # X = 3L
#' submit() # X = 4.0
#' submit() # X = TRUE
#' submit() # X = expression(Y) or Y = expression(X)
#' submit() # X = NA
#' submit() # X = NaN
#' submit() # X = Inf
#' submit() # X = NULL
#' submit() # X = function(x) {y <- sin(x); y^2}))
#' submit() # FALSE (no more results)
#' submit() # warning that no query is open
#'
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
#' query(call("member", expression(X), list(TRUE, expression(Y)))) # warning that another query is open
#' clear()
query <- function(
  query=call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))),
  options=NULL,
  env=globalenv())
{
  options <- c(options, rolog_options())
  query <- .preprocess(query, options$preproc)

  # Decorate result with the prolog syntax of the query
  if(options$portray)
    q <- portray(query, options)

  # Create query
  r <- .query(query, options, env)

  # Decorate result with the prolog syntax of the query
  if(options$portray)
    attr(r, "query") <- q

  return(r)
}

#' Clear current query
#'
#' @return
#' TRUE (invisible)
#'
#' @md
#'
#' @seealso [query()]
#' for a opening a query.
#'
#' @seealso [submit()]
#' for a submitting a query.
#'
#' @seealso [once()]
#' for a opening a query, submitting it, and clearing it again.
#'
#' @seealso [findall()]
#' for a opening a query, collecting all solutions, and clearing it again.
#'
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
#' submit() # X = a
#' submit() # X = "b"
#' clear()
#'
clear <- function()
{
  invisible(.clear())
}

#' Submit a query that has been opened with [query()] before.
#'
#' @param options
#' This is a list of options controlling translation from and to Prolog. Here,
#' only _postproc_ is relevant.
#'
#' @return
#' If the query fails, `FALSE` is returned. If the query succeeds, a
#' (possibly empty) list is returned that includes the bindings required to
#' satisfy the query.
#'   
#' @md
#'
#' @seealso [query()]
#' for a opening a query.
#' 
#' @seealso [rolog_options()]
#' for fine-grained control on the translation from R to Prolog and back.
#' 
#' @seealso [clear()]
#' for a clearing a query.
#' 
#' @seealso [once()]
#' for a opening a query, submitting it, and clearing it again.
#'
#' @seealso [findall()]
#' for a opening a query, collecting all solutions, and clearing it again.
#' 
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4, expression(Y))))
#' submit() # X = 3L
#' submit() # X = 4.0
#' submit() # X = TRUE
#' submit() # X = expression(Y) or Y = expression(X)
#' submit() # FALSE
#' submit() # warning that no query is open
#'
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
#' submit() # X = a
#' submit() # X = "b"
#' clear()
#' 
submit <- function(options=NULL)
{
  options <- c(options, rolog_options())
  r <- .submit()
  r <- .postprocess(r, options$postproc)
  return(r)
}
