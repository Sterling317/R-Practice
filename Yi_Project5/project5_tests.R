#Project 1 Tests
library(pacman)

p_load(testthat)


#Test Problem 6
test_that("Testing Problem 6", {
  expect_equal(prob6_result, .192,tolerance = 1e-3)
})

#Test Problem 7
test_that("Testing Problem 7", {
  expect_equal(prob7_result, .715,tolerance = 1e-3)
})

#Test Problem 8
test_that("Testing Problem 8", {
  expect_equal(prob8_result, .189,tolerance = 1e-3)
})

#Test Problem 9
test_that("Testing Problem 9", {
  expect_equal(prob9_result, .036 ,tolerance = 1e-3)
})

#Test Problem 10
test_that("Testing Problem 10", {
  expect_equal(prob10_result, .382,tolerance = 1e-3 )
})

#Test Problem 11
test_that("Testing Problem 11", {
  expect_equal(prob11_result, .1253814,tolerance = 1e-4 )
})
#Test Problem 12
test_that("Testing Problem 12", {
  expect_equal(prob12_result, .001324826 ,tolerance = 1e-5)
})
#Test Problem 13
test_that("Testing Problem 13", {
  expect_equal(prob13_result, .1244525, tolerance = 1e-3)
})

#Test Problem 14
test_that("Testing Problem 14", {
  expect_equal(factorial(-5), -1)
  expect_equal(factorial(3), 6)
  expect_equal(factorial(0), 1)
  expect_equal(factorial(6), 720)
})

#Test Problem 21
test_that("Testing Problem 21", {
  expect_equal(prob21_result,0.3456,tolerance = 1e-3 )
})

#Test Problem 22
test_that("Testing Problem 22", {
  expect_equal(prob22_result,0.4752 ,tolerance = 1e-3)
})

#Test Problem 23
test_that("Testing Problem 23", {
  expect_equal(prob23_result, 0.8704,tolerance = 1e-3)
})

#Test Problem 25
test_that("Optional - Testing Problem 25", {
  expect_equal(prob25_result, .007415771,tolerance = 1e-4)
})

#Test Problem 26
test_that("Optional Testing Problem 26", {
  expect_equal(prob26_result, .9998474,tolerance = 1e-3)
})

#Test Problem 27
test_that("Optional Testing Problem 27", {
  expect_equal(prob27_result, 100)
})

