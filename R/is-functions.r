#' is functions
#' @details
#'  is.llist(x) is TRUE if \code{x} is lempty or ltail(x) is llist.
#'  is.ilist(x) is TRUE for infinit-stream if ltail(x) and ltail(ltail(x))
#'   is the same object (circular reference) or the same function except its environment.
#' @param x a lcons object
#' @name is_lazystream
#' @examples
#'  is.lpair(lempty) # => FALSE
#'  is.lpair_not_llist(lempty) # => FALSE
#'  is.llist(lempty) # => TRUE
#'
#'  is.lpair(1 %:% 2) # => TRUE
#'  is.lpair_not_llist(1 %:% 2) # => TRUE
#'  is.llist(1 %:% 2) # => FALSE
#'
#'  is.lpair(1 %..% 2) # => TRUE
#'  is.lpair_not_llist(1 %..% 2) # => FALSE
#'  is.llist(1 %..% 2) # => TRUE
#'
#'  ones <- 1 %:% ones
#'  is.llist(ones) # => TRUE # circular reference
#'  is.llist(1 %..% Inf) #=> TRUE # the same function
#'
#'
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
