#' Extracting subset of lazy stream
#' @description take the first nth elements from a lazy stream
#' @param n: integer
#' @param x: a lazy stream
#' @examples
#' x <- 1 %..% 10
#' ltake(5, x) # => llist(1, 2, 3, 4, 5)
#' ldrop(7, x) # => llist(8, 9, 10)
#' @export
ltake <- function(n, x) {
  if (n <= 0 || lnull(x)) lempty
  else lhead(x) %:% ltake(n - 1, ltail(x))
}

#' @export
ldrop <- function(n, x) {
  if (n <= 0) x
  else ldrop(n - 1, ltail(x))
}

#' @export
lsplitAt <- function(n, x) list(ltake(n, x), ldrop(n, x)) # no taple, R's list

