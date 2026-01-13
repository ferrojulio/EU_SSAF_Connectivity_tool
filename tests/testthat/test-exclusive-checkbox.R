library(shinytest2)
library(stringr)

test_that("Exclusive checkbox JS call is present in farmers app", {
  app <- AppDriver$new(app_dir = "/srv/shiny-server/farmers", name = "farmers_exclusive_checkbox_test")
  
  # Navigate to page 5 where intSec_newconcepts is located
  app$set_inputs(currentPage = 5)
  app$wait_for_idle()
  
  # Check if the script tag is present in the rendered HTML
  # This is a basic check to ensure the R code is emitting the JS call
  script_present <- app$get_html() %>%
    stringr::str_detect("initExclusiveGroup('intSec_newconcepts'", fixed = TRUE)
  
  expect_true(script_present)
})
