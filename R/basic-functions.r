#' Extract the element of a lazy stream
#'
#' function names come from Haskell. \code{lcar} and \code{lcdr}
#' are renames of \code{lhead} and \code{ltail} but not recommended to use
#'  to avoid mixture of Haskell and Scheme.
#' @param x a lazy stream objects, which must not be \code{lempty}
#' @name basic_functions
#' @examples
#' x <- llist(1, 2, 3)
#' lhead(x) # => 1
#' ltail(x) # => llist(2, 3)
#' linit(x) # => llist(1, 2)
#' llast(x) # => 3
#'
#' \dontrun{lhead(lempty)}
#' tryCatch(lhead(lempty), error = function(e) print(e))
#' # => <simpleError in lhead(lempty): empty list>
NULL

#' @rdname basic_functions
#' @export
lhead <- function(x) {
  if (lnull(x)) stop("empty list")
  else if (!is.lpair(x)) stop("x must be lcons")
  else x$head
}

#' @rdname basic_functions
#' @export
ltail <- function(x) {
  if (lnull(x)) stop("empty list")
  else if (!is.lpair(x)) stop(x, " must be lcons")
  else x$tail()
}

#' @rdname basic_functions
#' @export
linit <- function(x) {
  if (lnull(x)) stop("empty list")
  else if (lnull(tl <- ltail(x))) lempty
  else lhead(x) %:% linit(tl)
}

#' @rdname basic_functions
#' @export
llast <- function(x) {
  if (lnull(x)) stop("empty list")
  else if (lnull(tl <- ltail(x))) lhead(x)
  else llast(tl)
}

#' @rdname basic_functions
#' @export
lcar <- lhead

#' @rdname basic_functions
#' @export
lcdr <- ltail
