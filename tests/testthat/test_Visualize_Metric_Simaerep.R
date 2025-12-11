test_that("Visualize_Metric_Simaerep processes data correctly", {

  dfResultsA <- lReport$Reporting_Results
  dfResultsA$SnapshotDate <- as.Date(Sys.Date())

  dfResultsB <- lReport$Reporting_Results
  dfResultsB$SnapshotDate <- as.Date("1900-01-01")


  expect_message(
    {
      charts <- Visualize_Metric_Simaerep(
        dfResults = bind_rows(dfResultsA, dfResultsB),
        dfInput = lReport$Reporting_Input,
        dfMetric = lReport$Reporting_Metrics,
        dfGroups = lReport$Reporting_Groups,
        strMetricID = "Analysis_kri0001"
      )
    },
    "Parsed"
  )

  # Test if the function returns a list of charts
  expect_true(is.list(charts))

  # Test if the list contains expected chart names
  expect_true("scatterPlot" %in% names(charts))
  expect_true("barChart" %in% names(charts))
  expect_true("metricTable" %in% names(charts))
  expect_true("simaerepChart" %in% names(charts))
  expect_true("timeSeries" %in% names(charts))

})

test_that("Visualize_Metric_Simaerep handles missing MetricID", {
  expect_message(
    {
      charts <- Visualize_Metric_Simaerep(
        dfResults = lReport$Reporting_Results,
        dfInput = lReport$Reporting_Input,
        dfMetric = lReport$Reporting_Metrics,
        dfGroups = lReport$Reporting_Groups,
        strMetricID = "Analysis_kri1000"
      )
    },
    "MetricID not found in dfResults"
  )

  # Test if the function returns NULL when MetricID is not found
  expect_null(charts)

  expect_message(
    {
      charts <- Visualize_Metric_Simaerep(
        dfResults = lReport$Reporting_Results,
        dfInput = lReport$Reporting_Input %>%
          dplyr::filter(.data$MetricID != "Analysis_kri0001"),
        dfMetric = lReport$Reporting_Metrics,
        dfGroups = lReport$Reporting_Groups,
        strMetricID = "Analysis_kri0001"
      )
    },
    "MetricID not found in dfInput"
  )

  # Test if the function returns NULL when MetricID is not found
  expect_null(charts)

  expect_message(
    {
      charts <- Visualize_Metric_Simaerep(
        dfResults = lReport$Reporting_Results,
        dfInput = lReport$Reporting_Input,
        dfMetric = lReport$Reporting_Metrics %>%
          dplyr::filter(.data$MetricID != "Analysis_kri0001"),
        dfGroups = lReport$Reporting_Groups,
        strMetricID = "Analysis_kri0001"
      )
    },
    "MetricID not found in dfMetrics"
  )

  expect_true("scatterPlot" %in% names(charts))
  expect_true("barChart" %in% names(charts))
  expect_true("metricTable" %in% names(charts))
  expect_true("simaerepChart" %in% names(charts))

})

test_that("Visualize_Metric_Simaerep handles missing Snapshot", {
  expect_warning(
    {
      charts <- Visualize_Metric_Simaerep(
        dfResults = lReport$Reporting_Results,
        dfInput = lReport$Reporting_Input,
        dfMetric = lReport$Reporting_Metrics,
        dfGroups = lReport$Reporting_Groups,
        strMetricID = "Analysis_kri0001",
        strSnapshotDate = as.Date("1900-01-01")
      )
    },
    "No data found for specified snapshot date"
  )

  # Test if the function returns NULL when MetricID is not found
  expect_null(charts)

  dfResults <- lReport$Reporting_Results
  dfResults$SnapshotDate <- as.Date("1900-01-01")

  expect_warning(
    {
      charts <- Visualize_Metric_Simaerep(
        dfResults = dfResults,
        dfInput = lReport$Reporting_Input %>%
          dplyr::filter(.data$MetricID != "Analysis_kri0001"),
        dfMetric = lReport$Reporting_Metrics,
        dfGroups = lReport$Reporting_Groups,
        strSnapshotDate = as.Date("1900-01-01")
      )
    },
    "No data found for specified snapshot date"
  )

  # Test if the function returns NULL when MetricID is not found
  expect_null(charts)

})

test_that("Visualize_Metric_Simaerep can run on just results and input", {

  dfInput <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  dfAnalyzed <- Analyze_Simaerep(dfInput)
  dfFlagged <- Flag_Simaerep(dfAnalyzed, vThreshold = c(-0.99, -0.95, 0.95, 0.99))

  charts <- Visualize_Metric_Simaerep(
     dfResults = dfFlagged,
     dfInput = dfInput
   )

  # Test if the list contains expected chart names
  expect_true("scatterPlot" %in% names(charts))
  expect_true("barChart" %in% names(charts))
  expect_true("metricTable" %in% names(charts))
  expect_true("simaerepChart" %in% names(charts))

})

