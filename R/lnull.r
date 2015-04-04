#' Check weather a lazy stream is lempty
#'
#' @param x: a lazy stream object
#' @export
#' @examples
#' lnull(llist(1, 2, 3)) # => FALSE
#' lnull(lempty) # => TRUE
#' @export
lnull <- function(x) identical(x, lempty)
