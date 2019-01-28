context("format_p")

test_that("error messages", {
  expect_error(format_p(-1), "p cannot be < 0")
  expect_error(format_p(3), "p cannot be > 1")
  expect_error(format_p("A"), "p must be a number")
  
  expect_error(format_p(0.05, minp = -1), "minp cannot be < 0")
  expect_error(format_p(0.05, minp = 3), "minp cannot be > 1")
  expect_error(format_p(0.05, minp = "A"), "minp must be a number")
  
  expect_error(format_p(0.05, digits = 3.4), 
               "digits must be an integer between 1 and 10")
  expect_error(format_p(0.05, digits = "A"), 
               "digits must be an integer between 1 and 10")
  expect_error(format_p(0.05, digits = 0), 
               "digits must be an integer between 1 and 10")
})

test_that("correct default parameters", {
  expect_equal(format_p(0.000001), "p < .001")
  expect_equal(format_p(0.01023), "p = .010")
  expect_equal(format_p(0.01099), "p = .011")
  expect_equal(format_p(0.03), "p = .030")
  expect_equal(format_p("0.03"), "p = .030")
})