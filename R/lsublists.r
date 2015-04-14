#' Extracting subset of lazy stream
#' @description the order of parameters is different from Haskell.
#'  The first argument is a lazy steram which enable you to use magrittr's pipe smoothly.
#' @name sublists
#' @param x a lazy stream
#' @param n intege
#' @param f predicate function (a' -> Boolean)
#' @examples
#' x <- 1 %..% 10
#' ltake(x, 5) # => llist(1, 2, 3, 4, 5)
#' ldrop(x, 7) # => llist(8, 9, 10)
#' lsplitAt(x, 4) # => list(1%..%3, 4%..%10)
#' ltakeWhile(x, function(x) x < 4) # => 1%..%3
#' ldropWhile(x, function(x) x < 4) # => 4%..%10
#' lspan(x, function(x) x < 4) # => list(1%..%3, 4%..%10)
#' lbreak(x, function(x) x >= 4) # => list(1%..%3, 4%..%10)
NULL

#' @rdname sublists
#' @export
ltake <- function(x, n) {
  if (n <= 0 || lnull(x)) lempty
  else lhead(x) %:% ltake(ltail(x), n - 1)
}

#' @rdname sublists
#' @export
ldrop <- function(x, n) {
  if (n <= 0) x
  else ldrop(ltail(x), n - 1)
}

#' @rdname sublists
#' @export
lsplitAt <- function(x, n) list(ltake(x, n), ldrop(x, n)) # no taple, R's list

#' @rdname sublists
#' @export
ltakeWhile <- function(x, f) {
  if (lnull(x) || !f(hd <- lhead(x))) lempty
  else hd %:% ltakeWhile(ltail(x), f)
}

#' @rdname sublists
#' @export
ldropWhile <- function(x, f) {
  if (lnull(x) || !f(hd <- lhead(x))) x
  else ldropWhile(ltail(x), f)
}

#' @rdname sublists
#' @export
lspan <- function(x, f) list(ltakeWhile(x, f), ldropWhile(x, f))

#' @rdname sublists
#' @export
lbreak <- function(x, f) lspan(x, Negate(f))
