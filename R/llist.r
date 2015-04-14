#' Make a finite lazy object from R's object
#' @description functions to construct and coerce lazy stream
#' @name llist
#' @param ... non lazy-stream object,
#' @param x any type of object
#'
#' @examples
#' x <- llist(1, 2, 3) # arguments are evaluated
#' # y <- llist(1, repeat() {}, 3) # cannot return
#' z <- llist_lazy(1, repeat{}, 3) # can be defined
#' lhead(z) # => 1
#'
#' # Convert R's vector (include R's list) to llist
#' # is.llist(as.llist(x)) is ALWAYS TRUE
#' as.llist(1:3) # => llist(1,2,3)
#'
#' llist(1, list(2, 3))
#' # => llist(1, list(2,3))
#' # => only the first layer is converted
NULL

#' @rdname llist
#' @export
llist <- function(...) {
  if (is.null(pairlist(...))) lempty
  else ..1 %:% do.call(llist, list(...)[-1], quote = TRUE)
}

#' @rdname llist
#' @export
llist_lazy <- function(...) {
  e <- parent.frame()
  expr <- as.list(substitute((...)))[-1]
  if (length(expr) == 0) lempty
  else eval(expr[[1]], envir = e) %:% do.call(llist_lazy, c(expr[-1], e = e), quote = TRUE)
}

#' @rdname llist
#' @export
as.llist <- function(x) {
  if (is.llist(x)) x
  else do.call(llist, as.list(x), quote = TRUE)
}
