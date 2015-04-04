#' Extract the first element of lazy stream
#'
#' @param x: lazy stream objects, which must not be `lempty`
#' @export
#' @examples
#' x <- llist(1, 2, 3)
#' lhead(x) # => 1
lhead <- lcar <- function(x) if (lnull(x)) stop("empty list") else x$head
