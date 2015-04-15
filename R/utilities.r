#' Syntax sugar of making an anonymous function
#'
#'  Tilda is a very useful symbol because rest parts are not evaluated.
#'  I hijack this functionality to make an anounymous function.
#'  Double tilda and its posterior parts with a symbol of `..` make
#'  an anonymous function whose formal parameter is converted from `..` to `._`.
#'  If you want to make an anoymous function which has more than one argument,
#'  the placeholde should be `..1`, `..2`, and so on. See examples.
#'  Single tilda works as if it is normaly used but it takes a bit calulation process.

#' @name doubletilda
#' @param ... expression starts with `~~`
#' @param env_ environment where ... is evaluated
#' @usage ~~ "any valid syntax with a symbol .."
#' @examples
#' ~~ .. + 1 # => function(._) ._ + 1
#' ~~ ..1 + ..2 # => function(._1, ._2) ._1 + ._2
#'
#' Reduce(~~..1 + ..2, 1:10)
#' Filter(~~ ..%%3==0, 1:10)
#
#' # tilda's role remains.
#' ~ speed + dist
#' lm(speed ~ dist, data = cars)

`~` <- function(..., env_ = parent.frame()) {

  dots <- as.list(substitute((...)))[-1]
  if (length(dots) != 1 || length(dots[[1]]) == 1 || dots[[1]][[1]] != "~")
    return(as.formula(deparse(as.call(c(quote(`~`), dots))), env_))

  expr <- dots[[1]][[2]]

  all_vars <- all.names(expr, functions = FALSE, unique = TRUE)
  # a symbol of an argument must be "..".
  # If there are more than one argument, symbols must start with ".." and numerics counting from 1.
  args_ <- sort(all_vars[grep("^..$|^..[0-9]+$", all_vars)])

  args_len <- length(args_)
  if (args_len == 1 && args_ != "..")
    stop("only one placeholder must be input `..`")
  if (args_len > 1 && args_ != paste0("..", seq_len(args_len)))
    stop("the placeholder must start with `..` and end with numeric from 1.")

  args_new <- sub("..", "._", args_)

  substi_list <-
    if (args_len == 1) list(.. = quote(._))
    else `names<-`(lapply(seq_len(args_len), function(k) as.symbol(paste0("._", k))) , args_)

  eval(call(
    "function",
    as.pairlist(tools:::as.alist.call(args_new)),
    substituteDirect(expr, substi_list)), env_)
}
