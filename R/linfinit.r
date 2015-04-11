#' Infinit lists
#' @description create an infinit lazy stream form a finit lazy stream
#' @name linfinit
#' @param x: a lazy stream object
#' @param f: a generating function
#'
#' @examples
#' literate(1, function(x) x+1)
#' lrepeat(1) # => llist(1, 1, 1, ...
#' lreplicate(1, 3) # => llist(1,1,1)
#' lcycle(llist(1))
NULL

#' @rdname linfinit
#' @export
literate <- function(x, f) {
  x %:% literate(f(x), f)
}

#' @rdname linfinit
#' @export
lrepeat <- function(x) {
  x %:% lrepeat(x) # == literate(identity, x)
}

#' @rdname linfinit
#' @export
lreplicate <- function(x, n) {
  if (n <= 0) lempty
  else x %:% lreplicate(x, n - 1)
}

#' @rdname linfinit
#' @export
lcycle <- function(x) {
  if (!is.llist(x)) stop("an argument only take a llist object")
  x %++% lcycle(x)
}
