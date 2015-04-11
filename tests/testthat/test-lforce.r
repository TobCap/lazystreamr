context("test for lforce")

x1 <- 1 %:% (2 %:% lempty)
x2 <- (1%..%2) %:% ((3%..%4) %:% lempty)
x3 <- llist(llist(1,2), llist(3,4))
x4 <- 1 %:% 2


test_that("test", {
  expect_equal(lforce(x1), list(1, 2))
  expect_equal(lforce(x2), list(list(1L,2L), list(3L,4L)))
  expect_equal(lforce(x2), lforce(x3))

  expect_equal(lforce(1 %:% (2 %:% lempty)), lforce(1%..%2))
  expect_equal(lforce(llist(1,2,3,4)), lforce(1 %..% 4))
  expect_equal(lforce(llist(1,2) %++% llist(3,4)), lforce(1 %..% 4))

})

