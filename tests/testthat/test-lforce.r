context("test for lforce")


test_that("test", {

  x1 <- 1L %:% (2L %:% lempty)
  x2 <- (1 %..% 2) %:% ((3 %..% 4) %:% lempty)
  x3 <- llist(llist(1L, 2L), llist(3L, 4L))
  x4 <- 1L %:% 2L

  expect_identical(lforce(x1), list(1L, 2L))
  expect_identical(lforce(x2), list(list(1L, 2L), list(3L, 4L)))
  expect_identical(lforce(x2), lforce(x3))

  expect_identical(lforce(1L %:% (2L %:% lempty)), lforce(1 %..% 2))
  expect_identical(lforce(llist(1L, 2L, 3L, 4L)), lforce(1 %..% 4))
  expect_identical(lforce(llist(1L, 2L) %++% llist(3L, 4L)), lforce(1 %..% 4))

})
