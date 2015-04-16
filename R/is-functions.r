#' is functions
#'
#' Check if an object is lcons class.
#'
#' @details
#' \code{lcons()} attach "lcons" class for output. \code{is.lpair()} simply
#' checks this attribute.
#'  \code{is.llist(x)} returns \code{TRUE} if \code{x} is \code{lempty} or
#'   \code{ltail(x)} is llist, or \code{ltail(x)} and
#' \code{ltail(ltail(x))} is the same expression in cdr part (promise part,
#'  i.e., function body) for infinit-stream.
#'  is.lpair_not_list(x) retuns TRUE if  \code{x} is lcons but not llist.
#' \code{is.llist_atomic_maybe(x)} checks if lhead(x) is R's atomic object
#' for several loops. If the checking arrives its limit loop, this returns
#' \code{structure(TRUE, class = "maybe")}. The default limit is ten.
#'
#' @param x a lcons object
#' @param depth for is.llist_atomic_maybe's
#' @name is_lazystream
#' @examples
#' is.lpair(lempty) # => FALSE
#' is.lpair_not_llist(lempty) # => FALSE
#' is.llist(lempty) # => TRUE
#'
#' is.lpair(1 %:% 2) # => TRUE
#' is.lpair_not_llist(1 %:% 2) # => TRUE
#' is.llist(1 %:% 2) # => FALSE
#'
#' is.lpair(1 %..% 2) # => TRUE
#' is.lpair_not_llist(1 %..% 2) # => FALSE
#' is.llist(1 %..% 2) # => TRUE
#'
#' ones <- 1 %:% ones
#' is.llist(ones) # => TRUE # circular reference
#' is.llist(1 %..% Inf) #=> TRUE #
#'
#' is.llist_atomic_maybe(1 %..% 10) # => TRUE
#' is.llist_atomic_maybe(1 %..% 11) # => structure(TRUE, class = "maybe")
#' is.llist_atomic_maybe(1 %..% Inf) # => structure(TRUE, class = "maybe")
NULL

#' @rdname is_lazystream
#' @export
is.lpair <- function(x) inherits(x, "lcons")

#' @rdname is_lazystream
#' @export
is.lpair_not_llist <- function(x) is.lpair(x) && !is.llist(x)

#' @rdname is_lazystream
#' @export
is.llist <- function(x) {
  if (lnull(x)) return(TRUE)
  if (!is.lpair(x)) return(FALSE)

  tl <- ltail(x)
  if (lnull(tl)) return(TRUE)
  if (!is.lpair(tl)) return(FALSE)

  identical(
    substitute(rhs, environment(x$tail)),
    substitute(rhs, environment(tl$tail))) ||
  is.llist(tl)
}

#' @rdname is_lazystream
#' @export
is.llist_atomic_maybe <- function(x, depth = 10) {
  maybe <- NULL
  iter <- function(x, depth) {
    if (lnull(x)) TRUE
    else if (!is.lpair(x)) FALSE
    else if (depth == 0) {maybe <<- "maybe"; TRUE}
    else is.atomic(lhead(x)) && iter(ltail(x), depth - 1)
  }
  `class<-`(iter(x, depth), maybe)
}
