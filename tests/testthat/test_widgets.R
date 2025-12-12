test_that("Widget_Simaerep creates a valid HTML widget", {
  # Create site list widget
  w <- Widget_Simaerep(
    dfInput = lAnalysis$Analysis_kri0001$Analysis_Input,
    dfFlagged = lAnalysis$Analysis_kri0001$Analysis_Flagged,
    dfGroups = lReport$Reporting_Groups,
    lMetric = lReport$Reporting_Metrics %>%
      filter(MetricID == "Analysis_kri0001")
  )

  expect_s3_class(w, "htmlwidget")

  expect_true(stringr::str_detect(attr(w, "output_label"), "Simaerep"))
})


test_that("Widget_BarChartSimaerep creates a valid HTML widget", {
  # Create site list widget
  w <- Widget_BarChartSimaerep(
    dfResults = lAnalysis$Analysis_kri0001$Analysis_Flagged,
    dfGroups = lReport$Reporting_Groups,
    lMetric = lReport$Reporting_Metrics %>%
      filter(MetricID == "Analysis_kri0001")
  )

  expect_s3_class(w, "htmlwidget")

  expect_true(stringr::str_detect(attr(w, "output_label"), "Bar Chart"))
})
