context("test for lforld")

x1_10 <- 1 %..% 10
f <- lforce
test_that("test", {
  expect_equal(lfoldl(x1_10, `+`, 0), 55)
  expect_equal(lfoldr(x1_10, `+`, 0), 55)
  expect_equal(lfoldl1(x1_10, `+`), 55)
  expect_equal(lfoldr1(x1_10, `+`), 55)
  expect_identical(
    lforce(ltail(lscanl(liota(10), `+`, 0L))),
    lforce(lscanl1(liota(10), `+`))
  )
  expect_identical(
    f(lunfold_haskell(0L, function(x) if (x >= 10L) lempty else lcons(x, x + 1L))),
    f(liota(10)))

})
