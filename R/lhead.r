#' Extract the first element of lazy stream
#'
#' @param x: lazy stream objects, which must not be `lempty`
#' @export
#' @examples
#' x <- llist(1, 2, 3)
#' lhead(x) # => 1
lhead <- function(x) {
  if (lnull(x)) stop("empty list")
  else if (!is.lcons(x)) stop("x must be lcons")
  else x$head
}

#' @export
lcar <- lhead
