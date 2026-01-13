library(shinytest2)

test_that("Farmers app starts and has initial elements", {
  app <- AppDriver$new(name = "farmers_app", app_dir = "farmers")
  
  # Check for the language selector
  app$expect_js("document.getElementById('lang') !== null")
  
  # Check for the 'Start' button
  app$expect_js("document.getElementById('btnStart') !== null")
  
  # Simulate clicking the start button
  app$click("btnStart")
  
  # Check if the page has advanced (e.g., currentPage is no longer 0)
  # This assumes that clicking btnStart changes the page.
  # You might need to inspect the app's behavior more closely for a robust check.
  # For now, we'll just check if the btnStart is no longer visible or a new element appears.
  app$expect_js("document.getElementById('btnStart') === null || document.getElementById('PRESENTATION_Next') !== null")
  
  app$stop()
})
