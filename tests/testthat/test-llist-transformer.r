context("test for list-transformer")

f <- lforce
l <- liota(5)

test_that("test", {
  expect_equal(f(lmap(l, function(x) x ^ 2)), f(lmap2(l, . ^ 2)))
  expect_equal(f(lreverse(l)), f(4 %..% 0))
  expect_equal(
    f(ltranspose(llist(llist(1, 2, 3),llist(4, 5, 6)))),
    f(llist(llist(1, 4), llist(2, 5), llist(3, 6))))

})
