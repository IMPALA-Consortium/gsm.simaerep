test_that("test Input_CumCount PD", {

  dfInput <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::ctms_protdev %>% rename(subjid = subjectenrollmentnumber),
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol  = "visit_dt"
  )

  dfAnalyze <- Analyze_Simaerep(dfInput)

  expected_cols <- c(
    "GroupID",
    "GroupLevel",
    "MetricExpected",
    "Metric",
    "OverReportingProbability",
    "UnderReportingProbability",
    "Score"
  )

  expect_equal(colnames(dfAnalyze), expected_cols)

})

test_that("test Input_CumCount AE", {

  dfInput <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol  = "visit_dt"
  )

  dfAnalyze <- Analyze_Simaerep(dfInput)

  expected_cols <- c(
    "GroupID",
    "GroupLevel",
    "MetricExpected",
    "Metric",
    "OverReportingProbability",
    "UnderReportingProbability",
    "Score"
  )

  expect_equal(colnames(dfAnalyze), expected_cols)

})
