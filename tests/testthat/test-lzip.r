context("test for lzip")

l <- liota()
f <- function(x) lforce(ltake(x, 10))

test_that("test", {
  expect_equal(
    f(lzipWith(`*`, l, l)),
    f(lmap(l, function(x) x^2)))

  x1 <- 0%..%2
  x2 <- 10%..%100
  x3 <- 100%..%Inf
  expect_equal(
    f(lunzip(lzip(x1, x2, x3))),
    f(llist(0%..%2, 10%..%12, 100%..%102)))

})