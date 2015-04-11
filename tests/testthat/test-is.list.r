context("test for is.list()")

test_that("test", {
  expect_true(is.llist(lempty))
  expect_true(is.llist(1 %:% (2 %:% lempty)))
  expect_true(is.llist(llist(1, 2, 3)))
  expect_true(is.llist(1 %..% 3))

  ones <- 1 %:% ones
  lseq_maker. <- function(..., f) ..1 %:% do.call(lseq_maker., c(list(...)[-1], do.call(f, list(...)), f = f))
  lseq <- lseq_maker.(0, f = function(x) x + 1)

  expect_true(is.llist(ones))
  expect_true(is.llist(lseq))

  expect_false(is.llist(1 %:% 2))
  expect_false(is.llist(1))
  expect_false(is.llist("test"))
})