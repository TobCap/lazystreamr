#' Concat a lazy object
#' @param x a lazy stream object
#' @param f function that returns a lazy stream object
#' @return [[a]] -> [a]
#' @examples
#' lconcat(llist(llist(1,2), llist(3), llist(4,5,6))) # => llist(1,2,3,4,5,6)
#' lconcatMap(llist(1,2,3), function(x) llist(x, 10 * x)) # => llist(1,10,2,20,3,30)

#' @rdname lconcat
#' @export
lconcat <- function(x) {
  # lfoldr(x, `%++%`, lempty)
  if (lnull(x)) lempty
  else lhead(x) %++% lconcat(ltail(x))
}

#' @rdname lconcat
#' @export
lconcatMap <- function(x, f) {
  if (lnull(x)) lempty
  else f(lhead(x)) %++% lconcatMap(ltail(x), f)
}
