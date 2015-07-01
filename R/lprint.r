#' Print a lazy stream object
#'
#' @description print a lazy stream object
#' @name lprint
#' @param x a lazy stream
#' @param elem_max maximum conversion size
#' @param depth_max maximum conversion depth: drill down
#' @param ... for generic print()
#' @examples
#' # convert lazy object to R's list
#' lforce(llist(1,2,3)) # => list(1,2,3))
#' lforce(lcons(1, 2)) # => list(car = 1, cdr = 2)
NULL

#' @rdname lprint
#' @export
lprint <- function(x, elem_max = 50, depth_max = 3, ...) {
  message("llist -> list, lcons -> pairlist(car = , cdr = )")
  dput(lforce(x, elem_max, depth_max), control = NULL)
}


#' @rdname lprint
#' @export
print.lcons <- lprint

#' @rdname lprint
#' @export
lforce <- function(x, elem_max = 50, depth_max = 3) {
  # save initial value
  elem_init <- elem_max
  depth_init <- depth_max

  iter_normal <- function(x, elem_max, depth_max) {
    if (length(x) <= 1 && !is.list(x)) x
    else lapply(x, iter, elem_max = elem_init, depth_max = depth_max - 1)
  }

  iter <- function(x, elem_max, depth_max) {
    if (elem_max == 0 && !lnull(x)) {
      message(
        "the number of elements are over ",
        elem_init,
        "; stop converting and the rest are omitted")
      return(quote(`... omitted rest elements`))
    } else if (depth_max == 0 && !lnull(x)) {
      message(
        "depth is over ",
        depth_init,
        "; stop converting and the rest are omitted")
      return(list(quote(`... omitted deeper elements`)))
    }

    if (lnull(x))
      return(NULL)
    else if (!is.lpair(x))
      return(iter_normal(x, elem_max, depth_max))
    else if (is.lpair_not_llist(x))
      return(pairlist(
        car = iter(lhead(x), elem_init, depth_max - 1),
        cdr = iter(ltail(x), elem_init, depth_max - 1)))

    hd <- lhead(x); tl <- ltail(x); n <- elem_max; d <- depth_max

    if (is.lpair_not_llist(hd))
      c(list(pairlist(
        car = iter(lhead(hd), elem_init, d - 1),
        cdr = iter(ltail(hd), elem_init, d - 1))), iter(tl, n - 1, d))
    else if (is.llist(hd))
      c(list(iter(hd, elem_init, d - 1)), iter(tl, n - 1, d))
    else
      c(list(hd), iter(tl, n - 1, d))
  }

  iter(x, elem_max, depth_max)
}

#' @rdname lprint
#' @export
lforce_llist <- function(x, elem_max = 100) {
  iter <- function (x, elem_max = 100, acc = NULL) {
    if (elem_max == 0) {
      message("too many elements; stop converting");
      return(c(acc, quote(....)))
    }

    if (lnull(x)) acc
    else iter(ltail(x), elem_max - 1, c(acc, list(lhead(x))))
  }

  if (exists("tco")) tco(iter)(x, elem_max, acc = NULL)
  else iter(x, elem_max, acc = NULL)
}
