context("test for sublists")

x <- 1%..%10
x1_3 <- 1%..%3
x4_10 <- 4%..%10
f <- lforce

test_that("test", {
  expect_equal(f(ltake(x, 3)), f(x1_3))
  expect_equal(f(ldrop(x, 3)), f(x4_10))
  expect_equal(f(lsplitAt(x, 3)), f(list(x1_3, x4_10)))
  expect_equal(f(ltakeWhile(x, function(x) x <= 3)), f(x1_3))
  expect_equal(f(ldropWhile(x, function(x) x <= 3)), f(x4_10))
  expect_equal(f(lspan(x, function(x) x <= 3)), f(list(x1_3, x4_10)))
  expect_equal(f(lbreak(x, function(x) x > 3)), f(list(x1_3, x4_10)))
})

