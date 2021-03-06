% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lprint.r
\name{lprint}
\alias{lprint}
\alias{lprint}
\alias{print.lcons}
\alias{lforce}
\alias{lforce_llist}
\title{Print a lazy stream object}
\usage{
lprint(x, elem_max = 50, depth_max = 3, ...)

\method{print}{lcons}(x, elem_max = 50, depth_max = 3, ...)

lforce(x, elem_max = Inf, depth_max = Inf)

lforce_llist(x, elem_max = 100)
}
\arguments{
\item{x}{a lazy stream}

\item{elem_max}{maximum conversion size}

\item{depth_max}{maximum conversion depth: drill down}

\item{...}{for generic print()}
}
\description{
print a lazy stream object
}
\examples{
# convert lazy object to R's list
lforce(llist(1,2,3)) # => list(1,2,3))
lforce(lcons(1, 2)) # => list(car = 1, cdr = 2)
}
