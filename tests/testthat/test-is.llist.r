context("test for is.list()")

test_that("test", {
  expect_false(is.lpair(lempty))
  expect_false(is.lpair_not_llist(lempty))
  expect_true(is.llist(lempty))

  expect_true(is.lpair(1 %:% 2))
  expect_true(is.lpair_not_llist(1 %:% 2))
  expect_false(is.llist(1 %:% 2))

  expect_true(is.lpair(1 %..% 2))
  expect_false(is.lpair_not_llist(1 %..% 2))
  expect_true(is.llist(1 %..% 2))

  expect_true(is.llist(1 %:% (2 %:% lempty)))
  expect_true(is.llist(llist(1, 2, 3)))
  expect_true(is.llist(1 %..% 3))

  expect_true(is.lpair(1 %:% (2 %:% 3)))
  expect_true(is.lpair_not_llist(1 %:% (2 %:% 3)))
  expect_false(is.llist(1 %:% (2 %:% 3)))

  ones <- 1 %:% ones
  lseq_maker <- function(..., f)
    ..1 %:% do.call(lseq_maker, c(list(...)[-1], do.call(f, list(...)), f = f))
  lseq <- lseq_maker(0, f = function(x) x + 1)

  expect_true(is.llist(ones))
  expect_true(is.llist(lseq))

  expect_false(is.llist(1 %:% 2))
  expect_false(is.llist(1))
  expect_false(is.llist("test"))

  expect_true(is.llist_atomic_maybe(1 %..% 10))
  expect_identical(
    is.llist_atomic_maybe(1 %..% 11),
    structure(TRUE, class = "maybe"))
  expect_identical(
    is.llist_atomic_maybe(liota()),
    structure(TRUE, class = "maybe"))
})
