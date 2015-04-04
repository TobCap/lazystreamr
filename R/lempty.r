#' an empty object of lazy stream
#'
#' @export
#' @examples
#' 1 %:% (2 %:% lempty)
#' lnull(lempty) # => TRUE
lempty <- `class<-`(quote(empty), "llist")
