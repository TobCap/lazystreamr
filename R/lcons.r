#' Creates a pair, the right value is promised (by making lambda)
#'
#' The idea and funcition names mainly comes from Scheme (Gauche) and Haskell. see References
#' @references
#' \itemize{
#'   \item \url{http://srfi.schemers.org/srfi-1/srfi-1.html}
#'   \item \url{http://srfi.schemers.org/srfi-41/srfi-41.html}
#'   \item \url{http://srfi.schemers.org/srfi-45/srfi-45.html}
#'   \item \url{http://practical-scheme.net/gauche/man/gauche-refe_59.html}
#'   \item \url{http://practical-scheme.net/gauche/man/gauche-refe_88.html}
#'   \item \url{http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html}
#'   \item \url{http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-List.html}
#' }
#' @name lcons
#' @param lhs car part
#' @param rhs cdr part
#' @return lazy cons of \code{lhs} and \code{rhs}.
#' @examples
#'
#' # R's infix operator has left-associativity
#' # run
#' 1 %:% (2 %:% (3 %:% lempty))
#'
#' # can be parsed but not meaningful to interpret
#' 1 %:% 2 %:% 3 %:% lempty
#'
#' # repeat 1L as an infinit sequence
#' ones <- 1L %:% ones
#'
#' # this was privous version of lcons that evaluate a cdr part as eager-eval.
#' `%:%` <- lcons <- function(x, y) {
#'   `class<-`(pairlist(head = x, tail = function() y), "lcons")
#' }
NULL

#' @rdname lcons
#' @export
`%:%` <- function(lhs, rhs) {
  env_ <- parent.frame()
  `class<-`(
    eval(
      bquote(
        pairlist(head = .(x), tail = function() .(y)),
        list(x = substitute(lhs), y = substitute(rhs))), envir = env_), "lcons")
}

#' @rdname lcons
#' @export
lcons <- `%:%`
