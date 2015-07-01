context("test for list-transformer")

f <- lforce
l <- liota(5)

test_that("test", {
  expect_identical(f(lmap(l, function(x) x ^ 2L)), f(lmap2(l, . ^ 2L)))
  expect_identical(f(lreverse(l)), f(4 %..% 0))
  expect_identical(
    f(ltranspose(llist(llist(1, 2, 3),llist(4, 5, 6)))),
    f(llist(llist(1, 4), llist(2, 5), llist(3, 6))))

})
