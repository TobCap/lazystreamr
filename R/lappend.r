#' append lazy stream
#' @param lhs a lazy stream
#' @param rhs a lazy stream
#' @name lappend
#' @examples
#' x <- llist(1, 2)
#' y <- llist(3, 4, 5)
#' z <- x %++% y # => llist(1, 2, 3, 4, 5)
#' lappend(x, y) # => llist(1, 2, 3, 4, 5)
NULL

#' @rdname lappend
#' @export
`%++%` <- function(lhs, rhs){
  if (lnull(lhs)) rhs
  else lhead(lhs) %:% (ltail(lhs) %++% rhs)
}

#' @rdname lappend
#' @export
lappend <- `%++%`
