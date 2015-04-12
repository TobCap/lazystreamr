#' is functions
#' @details
#'  is.llist(x) is TRUE if \code{x} is lempty or ltail(x) is llist.
#'  is.ilist(x) is TRUE for infinit-stream if ltail(x) and ltail(ltail(x))
#'   is the same object (circular reference) or the same function except its environment.
#' @param x a lcons object
#' @name is_lazystream
#' @examples
#'  is.lcons(lempty) # => FALSE
#'  is.lpair(lempty) # => FALSE
#'  is.llist(lempty) # => TRUE
#'
#'  is.lcons(1 %:% 2) # => TRUE
#'  is.lpair(1 %:% 2) # => TRUE
#'  is.llist(1 %:% 2) # => FALSE
#'
#'  is.lcons(1 %..% 2) # => TRUE
#'  is.lpair(1 %..% 2) # => FALSE
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
is.lcons <- function(x) inherits(x, "lcons")

#' @rdname is_lazystream
#' @export
is.lpair <- function(x) is.lcons(x) && !is.llist(x)

#' @rdname is_lazystream
#' @export
is.llist <- function(x) {
  iter <- function(hd, tl) {
    if (is.lcons(hd)) {
      hd2 <- ltail(hd)
      if (is.lpair(hd2)) {
        identical(hd$tail, hd2$tail, ignore.environment = TRUE) || iter(ltail(hd2), ltail(tl))
      } else {
        lnull(hd2)
      }
    } else {
      lnull(hd)
    }
  }
  iter(x, x)
}
