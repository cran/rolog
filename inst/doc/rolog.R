## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(rmarkdown.html_vignette.check_title=FALSE)
library(htmltools)
library(DiagrammeR)
library(DiagrammeRsvg)

## -----------------------------------------------------------------------------
library(rolog)

## -----------------------------------------------------------------------------
# member(1, [1, 2.0, a, "b", X])
query(call("member", 1L, list(1L, 2.0, quote(a), "b", expression(X), TRUE)))

# returns an empty list, stating that member(1, [1 | _]) is satisfied
submit()

# returns a list, stating that the query is also satisfied if X = 1
submit()

# close the query
clear()

## -----------------------------------------------------------------------------
once(call("r_eval", c(1, 2, NA, NaN, Inf), expression(X)))

## ---- echo=FALSE, fig.width=6, fig.height=2-----------------------------------
HTML(export_svg(grViz(
  'digraph G
   {
     rankdir=LR
     Query Result

     subgraph cluster_0 
     {
       style=filled
       color=lightgrey
       node [style=filled,color=white]
       r2rolog -> forth -> rolog_pl
     }

     subgraph cluster_1 
     {
       style=filled
       color=lightgrey
       node [style=filled,color=white]
       rolog2r -> back [dir=back]
       back -> pl_rolog [dir=back]
     }
  
     Query -> r2rolog
     rolog_pl:e -> Prolog
     pl_rolog:e -> Prolog [dir=back]
     Result -> rolog2r [dir=back]

     Query [shape=Mdiamond;width=0.7;height=0.7]
     r2rolog [shape=rect,label="preproc(...)"]
     forth [label="(rolog)"]
     rolog_pl [shape=rect,label="preproc/2"]
     Prolog [shape=Mcircle]
     pl_rolog [shape=rect,label="postproc/2"]
     rolog2r [shape=rect,label="postproc(...)"]
     back [label="(rolog)"]
     Result [shape=Msquare]
   }')))

## -----------------------------------------------------------------------------
stringify <- function(x)
{
  # replace Prolog variable by the value of an R variable with the same name
	if(is.name(x))
	  return(as.character(x))
	
	# Recurse into lists and calls
	if(is.call(x))
	  x[-1] <- lapply(x[-1], FUN=stringify)

	if(is.list(x))
	  x <- lapply(x, FUN=stringify)

  # Leave the rest unchanged
	return(x)
}

# Example illustration
q <- quote(member(.X, ""[a, b, c]))
r <- findall(as.rolog(q))
stringify(r)

## -----------------------------------------------------------------------------
library(rolog)

# [family].
consult(system.file(file.path("pl", "family.pl"), package="rolog"))

# ancestor(X, jim).
query(call("ancestor", expression(X), quote(jim)))

# solutions for X, one by one
submit()
submit()
submit()
submit()
submit() # no more results (closing the query)
submit() # warning that no query is open
# clear() # normally used to close a query

## -----------------------------------------------------------------------------
# [backdoor].
consult(system.file(file.path("pl", "backdoor.pl"), package="rolog"))

# Figure 12 in Greenland et al.
add_node = function(N)
	invisible(once(call("assert", call("node", N))))

add_arrow = function(X, Y)
	invisible(once(call("assert", call("arrow", X, Y))))

add_node("a")
add_node("b")
add_node("c")
add_node("d") # outcome
add_node("e") # exposure
add_node("f")
add_node("u")

add_arrow("a", "d")
add_arrow("a", "f")
add_arrow("b", "d")
add_arrow("b", "f")
add_arrow("c", "d")
add_arrow("c", "f")
add_arrow("e", "d")
add_arrow("f", "e")
add_arrow("u", "a")
add_arrow("u", "b")
add_arrow("u", "c")

findall(call("minimal", "e", "d", expression(S)))

## -----------------------------------------------------------------------------
# [telescope].
consult(system.file(file.path("pl", "telescope.pl"), package="rolog"))

# findall(sentence(Tree, "john saw a man with a telescope")).
findall(call("sentence", expression(Tree), "john saw a man with a telescope"))

## -----------------------------------------------------------------------------
library(rolog)
consult(system.file(file.path("pl", "buggy.pl"), package="rolog"))

q <- quote(search(tratio(x, mu, s, n), .S))
findall(as.rolog(q))

## -----------------------------------------------------------------------------
library(rolog)
consult(system.file(file.path("pl", "mathml.pl"), package="rolog"))

# R interface to Prolog predicate r2mathml/2
mathml = function(term)
{
  t = once(call("r2mathml", term, expression(X)))
  cat(paste(t$X, collapse=""))
}

## ---- results="asis"----------------------------------------------------------
term = quote(pbinom(k, N, p))

# Pretty print
mathml(term)

# Do some calculations with the same term
k = 10
N = 22
p = 0.4
eval(term)

## ---- results="asis"----------------------------------------------------------
term = quote(integrate(sin, 0L, 2L*pi))
mathml(term)
eval(term)

## ---- results='asis'----------------------------------------------------------
# Apply match.call to all components of a term
canonical <- function(term)
{
  if(is.call(term))
  {
    f <- match.fun(term[[1]])
    if(!is.primitive(f))
      term <- match.call(f, term)
    
    # Recurse into arguments
    term[-1] <- lapply(term[-1], canonical)
  }

  return(term)
}

# A custom function
g <- function(u)
{
  sin(u)
}

# Mixture of (partially) named and positional arguments in unusual order
term <- quote(2L * integrate(low=-Inf, up=Inf, g)$value)
mathml(canonical(term))

# It is a bit of a mystery that R knows the result of this integral.
eval(term)

## -----------------------------------------------------------------------------
# [r_eval].
consult(system.file(file.path("pl", "r_eval.pl"), package="rolog"))

# rnorm(3)
once(call("r_norm", 3L, expression(X)))

## -----------------------------------------------------------------------------
# [interval].
consult(system.file(file.path("pl", "interval.pl"), package="rolog"))

# findall(1 ... 2 / -3 ... 3, Res).
q <- quote(int(`...`(1, 2) / `...`(-3, 3), .Res))
findall(as.rolog(q))

# t-ratio
D  = quote(`...`(5.7, 5.8))
mu = 4
s  = quote(`...`(3.8, 3.9))
N  = 24L
tratio = call("/", call("-", D, mu), call("/", s, call("sqrt", N)))
findall(call("int", tratio, expression(Res)))

# Binomial density
prob = quote(`...`(0.2, 0.3))
once(call("int", call("dbinom", 4L, 10L, prob, FALSE), expression(Res)))

prob = quote(`...`(0.5, 0.6))
once(call("int", call("dbinom", 4L, 10L, prob, FALSE), expression(Res)))

prob = quote(`...`(0.2, 0.6))
once(call("int", call("dbinom", 4L, 10L, prob, FALSE), expression(Res)))

