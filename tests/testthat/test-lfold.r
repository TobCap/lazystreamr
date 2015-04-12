context("test for lforld")

x1_10 <- 1%..%10
f <- lforce
test_that("test", {
  expect_equal(lfoldl(x1_10, `+`, 0), 55)
  expect_equal(lfoldr(x1_10, `+`, 0), 55)
  expect_equal(lfoldl1(x1_10, `+`), 55)
  expect_equal(lfoldr1(x1_10, `+`), 55)
  expect_equal(
    lforce(ltail(lscanl(liota(10), `+`, 0))),
    lforce(lscanl1(liota(10), `+`))
  )
  expect_equal(
    f(unfoldr(0, function(x) if (x >= 10) quote(Nothing) else lcons(x, x+1))),
    f(liota(10)))

})