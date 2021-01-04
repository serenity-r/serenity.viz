library(shiny)
library(shinytest)

context("Test layer aesthetic value module")

appdir <- system.file("test_apps/moduleLayerAesValue", package = "serenity.viz")

app <- ShinyDriver$new(appdir)

test_that("initial values should output NULL if show_initial is FALSE", {
  values <- app$getAllValues()$export
  expect_null(values$size)
  expect_null(values$colour)
  expect_null(values$linetype)
})

test_that("initial values should NOT output NULL if show_initial is TRUE", {
  values <- app$getAllValues()$export
  expect_equal(values$shape, "19")
})

test_that("custom initial values should be NULL as well (except for shape)", {
  # Switch to custom inputs
  app$setInputs(`size-custom_toggle` = TRUE)
  app$setInputs(`colour-custom_toggle` = TRUE)
  app$setInputs(`linetype-custom_toggle` = TRUE)
  app$setInputs(`shape-custom_toggle` = TRUE)

  values <- app$getAllValues()$export
  expect_null(values$size)
  expect_null(values$colour)
  expect_null(values$linetype)
  expect_equal(values$shape, "19")
})

test_that("custom values, when changed, display correctly", {
  # Change custom inputs away from initial values
  app$setInputs(`size-value-custom_text` = "1.0")
  app$setInputs(`size-value-custom_ready` = "click")

  app$setInputs(`colour-value-custom_text` = "red")
  app$setInputs(`colour-value-custom_ready` = "click")

  app$setInputs(`linetype-value-custom_text` = "dashed")
  app$setInputs(`linetype-value-custom_ready` = "click")

  values <- app$getAllValues()$export
  expect_equal(values$size, "1.0")
  expect_equal(values$colour, "\"red\"")
  expect_equal(values$linetype, "\"dashed\"")
})

test_that("UI controls, when changed, display correctly (possibly changed)", {
  # Switch back to UI controls for inputs
  app$setInputs(`size-custom_toggle` = FALSE)
  app$setInputs(`colour-custom_toggle` = FALSE)
  app$setInputs(`linetype-custom_toggle` = FALSE)

  app$setInputs(`size-value` = 1.0)
  app$setInputs(`colour-value` = "red")
  app$setInputs(`linetype-value` = "dashed")

  values <- app$getAllValues()$export
  expect_equal(values$size, "1")
  expect_equal(values$colour, "\"#FF0000\"")
  expect_equal(values$linetype, "\"dashed\"")
})

test_that("resetting inputs back to initial values", {
  # Click the reset buttons
  app$setInputs(`size-reset_value` = "click")
  app$setInputs(`colour-reset_value` = "click")
  app$waitForValue("colour", iotype = "export", ignore = 'colour = "#FF0000"') # Color takes a while
  app$setInputs(`linetype-reset_value` = "click")

  values <- app$getAllValues()$export
  expect_null(values$size)
  expect_null(values$colour)
  expect_null(values$linetype)
})

app$stop()
