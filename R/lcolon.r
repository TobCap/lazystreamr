#' Colon Operator
#' @description generate lazy stream
#' @param rhs: numeric which is converted to integer
#' @param lhs: numeric which is converted to integer
#' @examples
#' x <- 1 %..% 3
#' y <- 4 %..% 5
#' z <- x %++% y # => llist(1, 2, 3, 4, 5)
#' @export
`%..%` <- function(lhs, rhs) {
  stopifnot(is.numeric(lhs), is.numeric(rhs))
  iter <- function(x, y) {
    if (x == y) x %:% lempty
    else if (x < y) x %:% iter(x + 1L, y)
    else if (x > y) x %:% iter(x - 1L, y)
    else stop("") }
  iter(as.integer(lhs), if (is.finite(rhs)) as.integer(rhs) else rhs)
}
