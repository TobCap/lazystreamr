#' Count length of finite lazy stream
#'
#' @name llength
#' @param x a lazy stream
#' @examples
#' x <- llist(1, 2, 3)
#' llength(x) # => 3
NULL

#' @rdname llength
#' @export
llength <- function(x) {
  iter <- function(x, acc) {
    if (lnull(x)) acc
    else iter(ltail(x), acc + 1)
  }
  iter(x, acc = 0)
}
