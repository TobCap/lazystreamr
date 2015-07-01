context("test for lconcat")

test_that("test", {
  a <- lforce(lconcat(llist(llist(1L, 2L), llist(3L), llist(4L, 5L, 6L))))
  b <- lforce(1 %..% 6)
  expect_identical(a, b)

  expect_identical(
    lforce(lconcat(llist(llist(1L, 2L), llist(3L), llist(4L, 5L, 6L)))),
    lforce(1 %..% 6))

  expect_identical(
    lforce(lconcatMap(llist(1, 2, 3), function(x) llist(x, 10 * x))),
    lforce(llist(1, 10, 2, 20, 3, 30)))
})
