context("test for lconcat")

test_that("test", {
  expect_equal(
    lforce(lconcat(llist(llist(1,2), llist(3), llist(4,5,6)))),
    lforce(llist(1,2,3,4,5,6)))

  expect_equal(
    lforce(lconcatMap(function(x) llist(x, 10 * x), llist(1,2,3))),
    lforce(llist(1,10,2,20,3,30)))
})