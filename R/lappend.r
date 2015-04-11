#' append lazy stream
#' @param lhs: a lazy stream
#' @param rhs: a lazy stream
#' @examples
#' x <- llist(1, 2)
#' y <- llist(3, 4, 5)
#' z <- x %++% y # => llist(1, 2, 3, 4, 5)
#' lappend(x, y) # => llist(1, 2, 3, 4, 5)
#' @export
`%++%` <- function(lhs, rhs){
  if (lnull(lhs)) rhs
  else lhead(lhs) %:% (ltail(lhs) %++% rhs)
}

#' @export
lappend <- `%++%`

#' @export
lconcat <- function(x) {
  if (lnull(x)) lempty
  else lhead(x) %++% lconcat(ltail(x))
}

#' @export
lconcatMap <- function(f, x) {
  if (lnull(x)) lempty
  else f(lhead(x)) %++% lconcatMap(f, ltail(x))
}
