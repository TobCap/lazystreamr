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
#' @param f a function for \code{lseq_gen}
#' @param x0,x1,x2 initial values for generating function \code{f}
#' @param ... arguments for \code{f}
#' @examples
#' x <- 1 %..% 3
#' y <- 4 %..% 5
#' z <- x %++% y # => llist(1, 2, 3, 4, 5)
#'
#' liota(5) # => llist(0, 1, 2, 3, 4)
#' larith(0, 2, 10) # => llist(0, 2, 4, 6, 8, 10)
#' lseq_gen(0, 1, f = function(x, y) x + y) # => fibonacci sequence
#' lseq_gen1(list(0, 1), f = function(xs) list(xs[[2]], xs[[1]]+ xs[[2]]))
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

#' @rdname generators
#' @export
lrange <- function(start_ = 0, step_ = 1, end_ = Inf) {
  if (start_ > end_) lempty
  else start_ %:% lrange(start_ + step_, step_, end_)
}


#' @rdname generators
#' @export
lseq_gen1 <- function(x0, f) x0 %:% lseq_gen1(f(x0), f)

#' @rdname generators
#' @export
lseq_gen2 <- function(x0, x1, f) x0 %:% lseq_gen2(x1, f(x0, x1), f)

#' @rdname generators
#' @export
lseq_gen3 <- function(x0, x1, x2, f)
  x0 %:% lseq_gen3(x1, x2, f(x0, x1, x2), f)

## induce a rule from above patterns.
#' @rdname generators
#' @export
lseq_gen <- function(..., f) {
  # The generating function f(x0, x1, ..., xn-1) takes last n arguments and returns xn (next value).
  ..1 %:% do.call(lseq_gen, c(list(...)[-1], f(...), f = f), quote = TRUE)
}
