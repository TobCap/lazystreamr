#' Boolean functions
#' @description Return TRUE or FALSE for a lazy stream
#' @name lboolean
#' @param x a lazy stream object
#' @param f a predicate function
#'
#' @examples
#' x <- 1%..%10
#' lall(x, function(x) x <= 10)
#' lany(x, function(x) x%%7==0)
#'
NULL

#' @rdname lboolean
#' @export
lall <- function(x, f) {
  if (lnull(x)) TRUE
  else f(lhead(x)) && lall(ltail(x), f)
}

#' @rdname lboolean
#' @export
lany <- function(x, f) {
  if (lnull(x)) FALSE
  else f(lhead(x)) || lany(ltail(x), f)
}

## and :: [Bool] -> Bool
#' @rdname lboolean
#' @export
land <- function(x) {
  if (lnull(x)) TRUE
  else isTRUE(lhead(x)) && land(ltail(x))
}

## or :: [Bool] -> Bool
#' @rdname lboolean
#' @export
lor <- function(x) {
  if (lnull(x)) FALSE
  else isTRUE(lhead(x)) || lor(ltail(x))
}

lsum <- function(x) {
  lfoldl(x, `+`, 0)
}

lproduct <- function(x) {
  lfoldl(x, `*`, 0)
}

lmaximum <- function(x, acc) {
  if (lnull(x)) stop("empty llist")
  else lfoldl1(x, max)
}

lminmum <- function(x) {
  if (lnull(x)) stop("empty llist")
  else lfoldl1(x, min)
}
