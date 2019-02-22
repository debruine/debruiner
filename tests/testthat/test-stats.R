context("steiger")

test_that("error messages", {
  expect_error(steiger(2, .5, .5, 20), "r12 <= 1 is not TRUE")
  expect_error(steiger(-2, -.5, -.5, 20), "r12 >= -1 is not TRUE")
  expect_error(steiger(.2, 5, .5, 20), "r13 <= 1 is not TRUE")
  expect_error(steiger(-.2, -5, -.5, 20), "r13 >= -1 is not TRUE")
  expect_error(steiger(.2, .5, 5, 20), "r23 <= 1 is not TRUE")
  expect_error(steiger(-.2, -.5, -5, 20), "r23 >= -1 is not TRUE")
  expect_error(steiger(-.2, -.5, -.5, 1), "n > 1 is not TRUE")
})

test_that("correct answer", {
  stg <- steiger(.4, .3, .2, 20)
  expect_equal(stg$z, 0.3583792, tolerance = 1e-06)
  expect_equal(stg$p, 0.7200596, tolerance = 1e-06)
  
  stg <- steiger(.4, .3, .2, 20, 1)
  expect_equal(stg$z, 0.3583792, tolerance = 1e-06)
  expect_equal(stg$p, 0.7200596/2, tolerance = 1e-06)
})