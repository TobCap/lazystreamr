#' Extract the last element of a lazy stream
#'
#' @param x: lazy stream objects, which must not be `lempty`
#' @export
#' @examples
#' x <- llist(1, 2, 3)
#' llast(x) # => 3
llast <- function(x) {
  if (lnull(x)) stop("empty list")
  else if (lnull(tl <- ltail(x))) lhead(x)
  else llast(tl)
}
