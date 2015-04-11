context("test for basic functions")
x0 <- llist(1, 2, 3)
x1 <- 1 %:% (2 %:% lempty)
x2 <- (1%..%2) %:% ((10%..%11) %:% lempty)
x3 <- llist(llist(1,2), llist(10,11))
x4 <- 1 %:% 2

test_that("test", {
  expect_true(lnull(lempty))
  # expect_equal(llength(lempty), 0L)

  expect_equal(lhead(x0), 1)
  expect_equal(ltail(x0), llist(2, 3))
  expect_equal(linit(x0), llist(1, 2))
  expect_equal(llast(x0), 3)

  expect_error(lhead(lempty))
  expect_error(ltail(lempty))

  expect_true(is.llist(x1))
  expect_true(is.lcons(x1))
  expect_false(is.lpair((x1)))

  expect_true(is.llist(x2))
  expect_true(is.lcons(x2))
  expect_false(is.lpair((x2)))

  expect_true(is.llist(x3))
  expect_true(is.lcons(x3))
  expect_false(is.lpair((x3)))

  expect_false(is.llist(x4))
  expect_true(is.lcons(x4))
  expect_true(is.lpair((x4)))

})
