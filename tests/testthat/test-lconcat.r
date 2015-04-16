context("test for lconcat")

test_that("test", {
  expect_equal(
    lforce(lconcat(llist(llist(1, 2), llist(3), llist(4, 5, 6)))),
    lforce(1 %..% 6))

  expect_equal(
    lforce(lconcatMap(llist(1, 2, 3), function(x) llist(x, 10 * x))),
    lforce(llist(1, 10, 2, 20, 3, 30)))
})