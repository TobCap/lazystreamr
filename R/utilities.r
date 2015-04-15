make_closure <- function(ex, .depth = 0) {
  
  expr <- 
    if (.depth == 0) substitute(ex)
    else if (.depth == 1) eval(substitute(substitute(ex)), parent.frame(1))
    else stop("only accept .depth == 0 or 1")

  if (expr[[1]] != "{") {
    if (is.function(ex)) return(ex)
    else stop("input must be a function or expression with '{' and `..` placeholder")
  }
  vars <- all.names(expr, unique = TRUE)
  dot2 <- sort(vars[grep("^..$|^..[0-9]+$", vars)])

  dot2_len <- length(dot2)
  if (dot2_len == 1 && dot2 != "..") 
    stop("the placeholder's symbol must be `..` when creating one argument l")
  else if (dot2_len > 1 && !all(dot2 == paste0("..", seq_len(dot2_len))))
    stop("the placeholder's symbol must be `..1`, `..2` in order")
  
  args_ <- 
    if (dot2_len == 1) as.pairlist(alist(..=))
    else as.pairlist(tools:::as.alist.call(dot2))

  target_frame <- parent.frame(.depth + 1)  
  eval(call("function", args_ , expr), target_frame)
}
make_closure({..1 + ..2})
