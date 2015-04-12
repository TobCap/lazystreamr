#' Lazy stream generators
#' @description generate a lazy stream
#' @name generators
#' @param rhs numeric which is converted to integer
#' @param lhs numeric which is converted to integer
#' @param  n number of elements
#' @param start_ start value
#' @param step_ step value
#' @param next_ next value
#' @param end_ env value
#' @param f a function for \code{lseq_maker}
#' @param ... arguments for \code{f}
#' @examples
#' x <- 1 %..% 3
#' y <- 4 %..% 5
#' z <- x %++% y # => llist(1, 2, 3, 4, 5)
#'
#' liota(5) # => llist(0, 1, 2, 3, 4)
#' larith(0, 2, 10) # => llist(0, 2, 4, 6, 8, 10)
#' lseq_maker(0, 1, f = function(x, y) x + y) # => fibonacci sequence
NULL

#' @rdname generators
#' @export
`%..%` <- function(lhs, rhs) {
  stopifnot(is.numeric(lhs), is.numeric(rhs))
  iter <- function(x, y) {
    if (x == y) x %:% lempty
    else if (x < y) x %:% iter(x + 1L, y)
    else if (x > y) x %:% iter(x - 1L, y)
    else stop("")
  }
  iter(as.integer(lhs), if (is.finite(rhs)) as.integer(rhs) else rhs)
}

#' @rdname generators
#' @export
liota <- function(n = Inf, start_ = 0L, step_ = 1L) {
  if (n <= 0L) lempty
  else start_ %:% liota(n - 1, start_ + step_, step_)
}

#' @rdname generators
#' @export
larith <- function(start_, next_, end_ = Inf) {
  # arithmetic sequence generater
  if (start_ > end_) lempty
  else start_ %:% larith(next_, 2L * next_ - start_, end_)
}


# The generating function f(x0, x1, ..., xn-1) takes last n arguments and returns xn (next value).
lseq_maker1 <- function(x, f) x %:% lseq_maker1(f(x), f)
lseq_maker2 <- function(x0, x1, f) x0 %:% lseq_maker2(x1, f(x0, x1), f)
lseq_maker3 <- function(x0, x1, x2, f)
  x0 %:% lseq_maker3(x1, x2, f(x0, x1, x2), f)

## induce a rule from above patterns.
#' @rdname generators
#' @export
lseq_maker <- function(..., f) {
  ..1 %:%
    do.call(
      lseq_maker,
      c(list(...)[-1], do.call(f, list(...), quote = TRUE), f = f),
      quote = TRUE)
}
