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

test_that("steiger_dat", {
  stg <- steiger_dat(iris[1:50,])
  
  expect_equal(stg$Sepal.Length$z, 3.354723, tolerance = 1e-06)
  expect_equal(stg$Sepal.Length$p, 0.0007944461, tolerance = 1e-06)
  expect_equal(stg$Sepal.Length$text,
               "Sepal.Length is correlated to Sepal.Width (r = 0.743) and to Petal.Length (r = 0.267). This difference is significant (z = 3.355, p < .001) 2-tailed with an alpha of 0.05.")
  
  expect_equal(stg$Petal.Length$z, 0.8797166, tolerance = 1e-06)
  expect_equal(stg$Petal.Length$p, 0.3790129, tolerance = 1e-06)
  expect_equal(stg$Petal.Length$text,
               "Petal.Length is correlated to Sepal.Length (r = 0.267) and to Sepal.Width (r = 0.178). This difference is not significant (z = 0.88, p = .379) 2-tailed with an alpha of 0.05.")
  
  expect_equal(stg$Sepal.Width$z, 4.016482, tolerance = 1e-06)
  expect_equal(stg$Sepal.Width$p, 5.907349e-05, tolerance = 1e-06)
  expect_equal(stg$Sepal.Width$text,
               "Sepal.Width is correlated to Sepal.Length (r = 0.743) and to Petal.Length (r = 0.178). This difference is significant (z = 4.016, p < .001) 2-tailed with an alpha of 0.05.")
  
})