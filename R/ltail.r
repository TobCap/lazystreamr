#' Extract the rest of the first element of lazy stream
#'
#' @param x: lazy stream objects, which must not be `lempty`
#' @export
#' @examples
#' x <- llist(1, 2, 3)
#' ltail(x) # => llist(2, 3)
ltail <- function(x) {
  if (lnull(x)) stop("empty list")
  else if (!is.lcons(x)) stop(x, " must be lcons")
  else x$tail()
}

#' @export
lcdr <- ltail
