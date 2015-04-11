#' Extract the nth elements (starting from 0)
#'
#' @param x: a lazy stream object
#' @param n: index starting from 0
#' @export
#' @examples
#' x <- llist(1, 2, 3, 4, 5)
#' x %!!% 3 # => 4
`%!!%` <- function(x, n) {
  if (n < 0) stop("negative index")
  if (n == 0) lhead(x) else ltail(x) %!!% (n - 1)
}

