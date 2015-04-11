#' Check weather a lazy stream is lempty
#'
#' @param x: a lazy stream object
#' @details 'lempty' is just a symbol representing empty list of a lazy stream object.
#'  lnull() tests whether a R object equals lempty.
#' @examples
#' lnull(llist(1, 2, 3)) # => FALSE
#' lnull(lempty) # => TRUE
#' @export
lnull <- function(x) identical(x, lempty)

#' @export
lempty <- quote(empty)
