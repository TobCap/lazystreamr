#' Searching with predicate f
#'
#' @name lsearching
#' @param x a lazy stream
#' @param f a predicate function (a' -> boolean)
#'
#' @examples
#' lfilter(1%..%10, function(x) x%%3== 0) # => llist(3,6,9)
#'
NULL

#' @rdname lsearching
#' @export
lfind <- function(x, f) {
  # not Maybe Class
  if (lnull(x)) lempty
  else if (f(hd <- lhead(x))) hd
  else lfind(ltail(x), f)
}

#' @rdname lsearching
#' @export
lfilter <- function(x, f) {
  if (lnull(x)) lempty
  else if (f(hd <- lhead(x))) hd %:% lfilter(ltail(x), f)
  else lfilter(ltail(x), f)
}

#' @rdname lsearching
#' @export
lpartition <- function(x, f) llist(lfilter(x, f), lfilter(x, Negate(f)))
