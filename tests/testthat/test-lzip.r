context("test for lzip")


test_that("test", {
  l <- liota()
  f <- function(x) lforce(ltake(x, 10))

  expect_identical(
    f(lzipWith(`*`, l, l)),
    f(lmap(l, function(x) x * x)))

  x1 <- 0 %..% 2
  x2 <- 10 %..% 100
  x3 <- 100 %..% Inf
  expect_identical(
    f(lunzip(lzip(x1, x2, x3))),
    f(llist(0 %..% 2, 10 %..% 12, 100 %..% 102)))

})
