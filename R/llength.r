#' Count length of finite lazy stream
#'
#' @param x: a lazy stream
#' @export
#' @examples
#' x <- llist(1, 2, 3)
#' llength(x) # => 3
llength <- function(x, acc = 0) {
  if (lnull(x)) acc
  else llength(ltail(x), acc + 1)
}
