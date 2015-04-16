#' fold a lazy stream
#'
#' The order of arguments position is different from that of Haskell's
#'  because it is easy for magrittr's pipe to pass a lazy stream as the first argument.
#' @param x a lazy stream
#' @param f function who must takes two areguments
#' @param init initial value if needed
#' @param g a function that takes one argument and returns lcons or lempty
#' @param f_pred a predicate function for unfold
#' @param f_map a mapping function for unfold
#' @param f_gen a generating function for unfold
#' @name lfold
#' @examples
#' lfoldl(1%..%10, `+`, 0) # => 55
#'
#' lfoldl(1%..%10, function(x,y) paste0("(",x,"+",y,")"), 0)
#' # => "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"
#' lfoldr(1%..%10, function(x,y) paste0("(",x,"+",y,")"), 0)
#' # => "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+0))))))))))"
#' lscanl(1%..%10,`+`, 0) # => llist(0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55)
#' ## library("magrittr")
#' ## liota() %>% ltake(10) %>% lfoldl1(`+`) # => 45
#'
#' lunfold_haskell(0, function(x) if (x >= 10) lempty else lcons(x, x + 1))
#' lunfoldl(1, function(x) x > 10, function(x) x ^ 2, function(x) x + 1)

NULL


## Reducing lists (folds)
#' @rdname lfold
#' @export
lfoldl <- function(x, f, init) {
  if (lnull(x)) init
  else lfoldl(ltail(x), f, f(init, lhead(x)))
}
# > lfoldl(`+`, 0, 1%..%10)
# [1] 55

#' @rdname lfold
#' @export
lfoldl1 <- function(x, f) {
  if (lnull(x)) stop("x must be non-empty LList")
  else lfoldl(ltail(x), f, lhead(x))
}

#' @rdname lfold
#' @export
lfoldr <- function(x, f, init) {
  if (lnull(x)) init
  else f(lhead(x), lfoldr(ltail(x), f, init))
}

#' @rdname lfold
#' @export
lfoldr1 <- function(x, f) {
  if (lnull(x)) stop("x must be non-empty LList")
  else if (llength(x) == 1) lhead(x)
  else f(lhead(x), lfoldr1(ltail(x), f))
}

## Building lists
#' @rdname lfold
#' @export
lscanl <- function(x, f, init) {
  if (lnull(x)) init %:% lempty
  else init %:% lscanl(ltail(x), f, f(init, lhead(x)))
}
# > lscanl(`+`, 0, 1%..%10)
# list(0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55)

#' @rdname lfold
#' @export
lscanl1 <- function(x, f) {
  if (lnull(x)) lempty
  else lscanl(ltail(x), f, lhead(x))
}

#' @rdname lfold
#' @export
lscanr <- function(x, f, init) {
  if (lnull(x)) llist(init)
  else {
    tmp <- lscanr(ltail(x), f, init)
    f(lhead(x), lhead(tmp)) %:% tmp
  }
}

#' @rdname lfold
#' @export
lscanr1 <- function(x, f) {
  if (lnull(x)) lempty
  else if (llength(x) == 1) x
  else {
    tmp <- lscanr1(ltail(x), f)
    f(lhead(x), lhead(tmp)) %:% tmp
  }
}

#' @rdname lfold
#' @export
lunfold_haskell <- function(x, g) {
  # g() must return pure lcons object, not llist object
  tmp <- g(x)
  if (lnull(tmp)) lempty
  else lhead(tmp) %:% lunfold_haskell(ltail(tmp), g)
}

#' @rdname lfold
#' @export
lunfoldl <- function(x, f_pred, f_map, f_gen) {
  if (f_pred(x)) lempty
  else f_map(x) %:% lunfoldl(f_gen(x), f_pred, f_map, f_gen)
}

#' @rdname lfold
#' @export
lunfoldr <- function(x, f_pred, f_map, f_gen) {
  iter <- function(x, acc) {
    if (f_pred(x)) acc
    else iter(f_gen(x), lcons(f_map(x), acc))
  }
  iter(x, lempty)
}
