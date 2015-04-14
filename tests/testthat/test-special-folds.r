context("test for special-folds")

x <- 1 %..% 10

test_that("test", {
  expect_true(lall(x, function(x) x <= 10))
  expect_true(lany(x, function(x) x %% 7 == 0))
  expect_true(land(lempty))
  expect_false(lor(lempty))
})
