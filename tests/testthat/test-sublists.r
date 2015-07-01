context("test for sublists")

x <- 1 %..% 10
x1_3 <- 1 %..% 3
x4_10 <- 4 %..% 10
f <- lforce

test_that("test", {
  expect_identical(f(ltake(x, 3)), f(x1_3))
  expect_identical(f(ldrop(x, 3)), f(x4_10))
  expect_identical(f(lsplitAt(x, 3)), f(list(x1_3, x4_10)))
  expect_identical(f(ltakeWhile(x, function(x) x <= 3)), f(x1_3))
  expect_identical(f(ldropWhile(x, function(x) x <= 3)), f(x4_10))
  expect_identical(f(lspan(x, function(x) x <= 3)), f(list(x1_3, x4_10)))
  expect_identical(f(lbreak(x, function(x) x > 3)), f(list(x1_3, x4_10)))
})
