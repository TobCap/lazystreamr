#' llist transformation
#' @param x a lazy stream
#' @param l llist of llist
#' @param f conver
#' @param ex a simbol `.` is treated as an formal parameter
#' @name llist_transformer
#' @examples
#' x1 <- llist(llist(1,2,3), llist(4,5,6))
#' x2 <- ltranspose(x1)
#' lprint(x1); lprint(x2)
#'
#' lmap(liota(), function(x) x^2)
#' lmap2(liota(), .^2)
#'
#' lreverse(liota(10))
NULL

#' @rdname llist_transformer
#' @export
lmap <- function(x, f) {
  if (lnull(x)) lempty
  else f(lhead(x)) %:% lmap(ltail(x), f)
}

#' @rdname llist_transformer
#' @export
lmap2 <- function(x, ex) {
  expr <- substitute(ex)

  f_mod <- tryCatch(
    is.function(ex),
    error = function(e) {
      eval(call("function", as.pairlist(alist(.=)), expr), parent.frame())
    })
#
#   f_mod <-
#     if (expr[[1]] == "function" || length(expr) == 1 && expr != ".")
#       ex
#     else if ("." %in% all.vars(expr))
#       eval(call("function", as.pairlist(alist(.=)), expr), parent.frame())
#     else
#       stop("bad exession for ex")

  iter <- function(x, f) {
    if (lnull(x)) lempty
    else f(lhead(x)) %:% iter(ltail(x), f)
  }
  iter(x, f_mod)
}

#' @rdname llist_transformer
#' @export
lreverse <- function(x) {
  iter <- function(x, acc) {
    if (lnull(x)) acc
    else iter(ltail(x), lhead(x) %:% acc)
  }
  iter(x, acc = lempty)
}

#' @rdname llist_transformer
#' @export
ltranspose <- function(l) {
  if (!is.llist(l)) stop("argument must be llist")

  if (lnull(l)) lempty
  else if (lnull(hd <- lhead(l))) ltranspose(ltail(l))
  else {
    x <- lhead(hd)
    xs <- ltail(hd)
    xss <- ltail(l)
    x %:% llist(lhead(lhead(xss))) %:% (ltranspose(xs %:% llist(ltail(lhead(xss)))))
  }
}
