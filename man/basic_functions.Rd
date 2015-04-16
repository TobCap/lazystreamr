% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/basic-functions.r
\name{basic_functions}
\alias{basic_functions}
\alias{lcar}
\alias{lcdr}
\alias{lhead}
\alias{linit}
\alias{llast}
\alias{ltail}
\title{Extract the element of a lazy stream}
\usage{
lhead(x)

ltail(x)

linit(x)

llast(x)

lcar(x)

lcdr(x)
}
\arguments{
\item{x}{a lazy stream objects, which must not be \code{lempty}}
}
\description{
function names come from Haskell. \code{lcar} and \code{lcdr}
are renames of \code{lhead} and \code{ltail} but not recommended to use
 to avoid mixture of Haskell and Scheme.
}
\examples{
x <- llist(1, 2, 3)
lhead(x) # => 1
ltail(x) # => llist(2, 3)
linit(x) # => llist(1, 2)
llast(x) # => 3

\dontrun{lhead(lempty)}
tryCatch(lhead(lempty), error = function(e) print(e))
# => <simpleError in lhead(lempty): empty list>
}
