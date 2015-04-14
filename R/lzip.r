#' Zipping and unzipping a lazy stream
#'
#' When zipping, if the size of arguments are different,
#'  the shortest one is used for output, i.e., excess elements are discarded.
#'
#' @name lzip
#' @param ... lazy stream objects
#' @param f zipping function
#' @param x,y llist
#' @param xs llist of llist
#' @examples
#' lint <- liota()
#' ltake(lzipWith(`*`, lint, lint), 10)
#' ltake(lzip(lint, lint, lint), 10)
#'
#' lunzip(lzip(0%..%2, 100%..%200, 10%..%Inf))
#' # => list(list(0, 1, 2), list(100, 101, 102), list(10, 11, 12))
NULL

#' @rdname lzip
#' @export
lzipWith2 <- function(f, x, y) {
  if (lnull(x) || lnull(y)) lempty
  else f(lhead(x), lhead(y)) %:% lzipWith2(f, ltail(x), ltail(y))
}

#' @rdname lzip
#' @export
lzipWith <- function(f, ...) {
  # This can treat any number of arguments
  dots <- list(...)
  if (any(vapply(dots, lnull, FALSE))) return(lempty)

  do.call(f, lapply(dots, lhead), quote = TRUE) %:%
    do.call(lzipWith, c(f, lapply(dots, ltail)), quote = TRUE)
}

#' @rdname lzip
#' @export
lzip <- function(...) {
  lzipWith(llist, ...)
}
#
# lunzip <- function(x) {
#   n <- llength(lhead(x))
#
# }

#' @rdname lzip
#' @export
lunzip2 <- function(xs) {
#   if (lnull(xs)) return(llist(lempty, lempty))
#   hd <- lhead(xs); tl <- ltail(xs)
#   llist(hd%!!%0 %:% (lunzip2(tl) %!!% 0), hd%!!%1 %:% (lunzip2(tl) %!!% 1))
  llist(lmap(xs, function(ys) ys %!!% 0), lmap(xs, function(ys) ys %!!% 1))
}

#' @rdname lzip
#' @export
lunzip <- function(xs) {
  n <- llength(lhead(xs))
  args <- lapply(seq_len(n) - 1L, function(k) {
    bquote(lmap(xs, function(ys) ys %!!% .(num)), list(num = k))
  })

  eval(as.call(c(quote(llist), args)), list(xs = xs), parent.frame())
}
