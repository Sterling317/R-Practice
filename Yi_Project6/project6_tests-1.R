#Project 6 Tests
library(testthat)

#Test Problem 1
test_that("Testing Problem 1", {
  expect_equal(prob1_result, .2984848, tolerance = 1e-7)
})

#Test Problem 2
test_that("Testing Problem 2", {
  expect_equal(prob2_result$probability[1], .000643, tolerance = 1e-6)
  expect_equal(prob2_result$probability[4], .144, tolerance = 1e-3)
  expect_equal(prob2_result$probability[7], .185 , tolerance = 1e-3)
  
  })

#Test Problem 3
test_that("Testing Problem 3", {
  expect_equal(prob3_result,.4677167 , tolerance = 1e-7)
})

#Test Problem 4
test_that("Testing Problem 4", {
  expect_equal(prob4_result, .7105939, tolerance = 1e-7)
})

#Test Problem 5
test_that("Testing Problem 5", {
  expect_equal(prob5_result, .5322833, tolerance = 1e-7)
})

#Test Problem 6
test_that("Testing Problem 6", {
  expect_equal(prob6_result,4.55 , tolerance = 1e-7)
})

#Test Problem 7
test_that("Testing Problem 7", {
  expect_equal(prob7_result, 1.5925, tolerance = 1e-7)
})

#Test Problem 9
test_that("Testing Problem 9", {
  expect_equal(prob9_result, 4.521, tolerance = 5e-1)
})

#Test Problem 10
test_that("Testing Problem 10", {
  expect_equal(prob10_result, 1.689248, tolerance = 1e-1)
})

#Test Problem 11
test_that("Testing Problem 11", {
  expect_equal(prob11_result, .1490028, tolerance = 1e-7)
})

#Test Problem 12
test_that("Testing Problem 12", {
  expect_equal(prob12_result, .01552688, tolerance = 1e-7)
})

#Test Problem 13
test_that("Testing Problem 13", {
  expect_equal(prob13_result, 0.6254307, tolerance = 1e-7)
})

#Test Problem 14
test_that("Testing Problem 14", {
  expect_equal(prob14_result, 0.0005401031, tolerance = 1e-5)
})

#Test Problem 15
test_that("Testing Problem 15", {
  expect_equal(prob15_result, 66, tolerance = 1e-7)
})

#Test Problem 17
test_that("Testing Problem 17", {
  expect_equal(prob17_result,56.205 , tolerance = 2)
})

#Test Problem 18
test_that("Testing Problem 18", {
  expect_equal(prob18_result,58.08306 , tolerance = 2)
})
#Test Problem 19
test_that("Testing Problem 19", {
  expect_equal(prob19_result, .9544997, tolerance = 1e-7)
})

#Test Problem 20
test_that("Testing Problem 20", {
  expect_equal(prob20_result,2.866516e-07 , tolerance = 1e-5)
})

#Test Problem 21
test_that("Testing Problem 21", {
  expect_equal(prob21_result, 1872)
})

#Test Problem 23
test_that("Testing Problem 23", {
  expect_equal(prob23_result, 1999.71, tolerance = 1e-2)
})

#Test Problem 24
test_that("Testing Problem 24", {
  expect_equal(prob24_result,100.0536 , tolerance = 5e-1)
})

#Test Problem 27
test_that("Testing Problem 27", {
  expect_equal(prob27_result, 1999.644, tolerance = 1e-3)
})


