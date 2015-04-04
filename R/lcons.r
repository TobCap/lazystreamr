#' Creates a lazy stream
#'
#' The idea and funcition names mainly comes from Scheme (Gauche) and Haskell. see Details.
#'
#' \itemize{
#'   \item \url{http://srfi.schemers.org/srfi-41/srfi-41.html}
#'   \item \url{http://srfi.schemers.org/srfi-41/srfi-41.html}
#'   \item \url{http://srfi.schemers.org/srfi-45/srfi-45.html}
#'   \item \url{http://practical-scheme.net/gauche/man/gauche-refe_59.html}
#'   \item \url{http://practical-scheme.net/gauche/man/gauche-refe_88.html}
#'   \item \url{http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html}
#'   \item \url{http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-List.html}
#' }
#'
#' @param x car part
#' @param y cdr part
#' @return lazy cons of \code{x} and \code{y}.
#' @export
#' @examples
#'
#' # R's infix operator has left-associativity
#' # run
#' 1 %:% (2 %:% (3 %:% NULL))
#'
#' # can be parsed but not meaningful to interpret
#' 1 %:% 2 %:% 3 %:% NULL
#'
#' # repeat 1L as an infinit sequence
#' ones <- 1L %:% ones
`%:%` <- lcons <- function(x, y) {
  `class<-`(pairlist(head = x, tail = function() y), "llist")
}
