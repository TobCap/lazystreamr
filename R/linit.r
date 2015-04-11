#' Extract a lazy stream except the last element.
#'
#' @param x: lazy stream objects, which must not be `lempty`
#' @export
#' @examples
#' x <- llist(1, 2, 3)
#' linit(x) # => llist(1, 2)
linit <- function(x) {
  if (lnull(x)) stop("empty list")
  else if (lnull(tl <- ltail(x))) lempty
  else lhead(x) %:% linit(tl)
}
