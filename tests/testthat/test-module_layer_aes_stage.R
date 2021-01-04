library(shiny)
library(shinytest)

context("Test layer aesthetic stage module")

test_that("Outputs correct when inheriting, linking turned on and default is not NULL", {
  testServer(layerAesMappingStageServer, {
    session$setInputs(custom_toggle = FALSE)
    session$setInputs(mapping = "Species")
    expect_null(stage_to_code())

    session$setInputs(mapping = "Sepal.Length")
    expect_equal(stage_to_code(), "Sepal.Length")

    session$setInputs(mapping = NULL)
    expect_equal(stage_to_code(), "NULL")
  }, args = list(id = "start", stage = "start",
                 choices = reactive({
                   isolate({ serenity.viz:::dataInputChoices(iris, zone="aeszone") })
                 }),
                 default = list(mapping = reactive({ "Species" }),
                                custom_mapping = reactive({ "Species" }),
                                custom_toggle = reactive({ FALSE })),
                 linked = reactive({ TRUE })
  ))
})

test_that("Outputs correct when inheriting, linking turned on and default is NULL", {
  testServer(layerAesMappingStageServer, {
    session$setInputs(custom_toggle = FALSE)
    session$setInputs(mapping = "Species")
    expect_equal(stage_to_code(), "Species")

    session$setInputs(mapping = NULL)
    expect_null(stage_to_code())
  }, args = list(id = "start", stage = "start",
                 choices = reactive({
                   isolate({ serenity.viz:::dataInputChoices(iris, zone="aeszone") })
                 }),
                 default = list(mapping = reactive({ NULL }),
                                custom_mapping = reactive({ character(0) }),
                                custom_toggle = reactive({ FALSE })),
                 linked = reactive({ TRUE })
  ))
})

test_that("Outputs correct when inheriting, linking turned off and default is not NULL", {
  testServer(layerAesMappingStageServer, {
    session$setInputs(custom_toggle = FALSE)

    # When default is Species and mapping is Species...
    session$setInputs(mapping = "Species")
    # ...output is null
    expect_equal(stage_to_code(), "Species")

    # When default is Species and mapping is Sepal.Length...
    session$setInputs(mapping = "Sepal.Length")
    # ...output is Sepal.Length
    expect_equal(stage_to_code(), "Sepal.Length")

    # When default is Species and mapping is NULL...
    session$setInputs(mapping = NULL)
    # ...output is "NULL"
    expect_equal(stage_to_code(), "NULL")
  }, args = list(id = "start", stage = "start",
                 choices = reactive({
                   isolate({ serenity.viz:::dataInputChoices(iris, zone="aeszone") })
                 }),
                 default = list(mapping = reactive({ "Species" }),
                                custom_mapping = reactive({ "Species" }),
                                custom_toggle = reactive({ FALSE })),
                 linked = reactive({ FALSE })
  ))
})

test_that("Outputs correct when inheriting, linking turned off and default is NULL", {
  testServer(layerAesMappingStageServer, {
    session$setInputs(custom_toggle = FALSE)
    session$setInputs(mapping = "Species")
    expect_equal(stage_to_code(), "Species")

    session$setInputs(mapping = "Sepal.Length")
    expect_equal(stage_to_code(), "Sepal.Length")

    session$setInputs(mapping = NULL)
    expect_equal(stage_to_code(), "NULL")
  }, args = list(id = "start", stage = "start",
                 choices = reactive({
                   isolate({ serenity.viz:::dataInputChoices(iris, zone="aeszone") })
                 }),
                 default = list(mapping = reactive({ NULL }),
                                custom_mapping = reactive({ character(0) }),
                                custom_toggle = reactive({ FALSE })),
                 linked = reactive({ FALSE })
  ))
})
