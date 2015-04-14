context("test for lgenerator")

f <- lforce
l <- liota(10)
lfib <- 0 %:% (1 %:% lzipWith(`+`, lfib, ltail(lfib)))
test_that("test", {
  expect_equal(f(0%..%9), f(l))
  expect_equal(f(liota(10, 2, 3)), f(larith(2, 5, 30)))
  expect_equal(f(liota(10, 2, 3)), f(lrange(2, 3, 30)))
  expect_equal(f(larith(2, 5, 30)), f(lrange(2, 3, 30)))
  expect_equal(
    lseq_gen(0, 1, f = function(x, y) x + y) %!!% 10,
    lfib %!!% 10)
})
