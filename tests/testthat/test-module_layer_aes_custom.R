library(shiny)
library(shinytest)

context("Test layer aesthetic custom module")

appdir <- system.file("test_apps/moduleLayerAesCustom/app.R", package = "serenity.viz")

app <- ShinyDriver$new(appdir)

test_that("custom value is initialized correctly", {
  expect_equal(app$getValue("custom_string"), "#73F0E8")
  expect_equal(app$getValue("custom_string_size"), "0.5")
  expect_equal(app$getValue("custom_string_with_control"), "#000000")
})

test_that("custom value inherits input value on change", {
  app$setInputs(colour = "#FFFFFF")
  expect_equal(app$getValue("custom_string"), "#FFFFFF")
  expect_equal(app$getValue("custom_string_with_control"), "#FFFFFF")
  app$setInputs(size = 0.25)
  expect_equal(app$getValue("custom_string_size"), "0.25")
})

test_that("module output changes only on click of ready button", {
  app$setInputs(`test-custom_text` = "#000000")
  expect_equal(app$getValue("custom_string"), "#FFFFFF")
  app$setInputs(`test-custom_ready` = "click")
  expect_equal(app$getValue("custom_string"), "#000000")
})

test_that("input value does not inherit custom value on change", {
  expect_equal(app$getAllValues()$input$colour, "#FFFFFF") # app$getValue("colour") throws error
  expect_equal(app$getValue("custom_string"), "#000000")
})

test_that("can control custom value from passed-in reactive", {
  app$setInputs(custom_value = "red")
  expect_equal(app$getValue("custom_string_with_control"), "red")
})

test_that("reset button sets value back to input", {
  app$setInputs(`test_with_control-reset_value` = "click")
  expect_equal(app$getValue("custom_string_with_control"), "#FFFFFF")
})

app$stop()

# Test render UI issue
appdir <- system.file("test_apps/moduleLayerAesCustom/renderUI.R", package = "serenity.viz")

app <- ShinyDriver$new(appdir)

test_that("custom value is initialized correctly when waitforit is 1", {
  expect_equal(app$getValue("custom_string"), "#000000")
})

test_that("custom value inherits input value on change when waitforit is 1", {
  app$setInputs(colour = "#FFFFFF")
  expect_equal(app$getValue("custom_string"), "#FFFFFF")
})

app$stop()
