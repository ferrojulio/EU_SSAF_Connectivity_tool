library(testthat)
library(shiny)

source("/srv/shiny-server/utils.R")

context("Utility Functions")

test_that("Sanitize cleans text correctly", {
  expect_equal(Sanitize("Hello World"), "hello world")
  expect_equal(Sanitize("It's a test"), "its a test")
  expect_equal(Sanitize("Multi-word string"), "multi word string")
  expect_equal(Sanitize(NULL), "No answer")
  expect_equal(Sanitize(""), "No answer")
})
