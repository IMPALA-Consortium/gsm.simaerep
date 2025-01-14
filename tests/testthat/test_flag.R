
expected_cols <- c(
  "GroupID",
  "GroupLevel",
  "Numerator",
  "Denominator",
  "MetricExpected",
  "Metric",
  "OverReportingProbability",
  "UnderReportingProbability",
  "Score",
  "Flag"
)

test_that("test Flag_Simaerep PD", {

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
  dfFlagged <- Flag_Simaerep(dfAnalyze, vThreshold = c(0.1, 0.5, 0.95, 0.99))

  expect_equal(colnames(dfFlagged), expected_cols)

  expect_equal(sort(unique(dfFlagged$Flag)), c(-2, -1, 0, 1, 2))

})

test_that("test Flag_Simaerep AE", {

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
  dfFlagged <- Flag_Simaerep(dfAnalyze, vThreshold = c(0.1, 0.5, 0.95, 0.99))

  expect_equal(colnames(dfFlagged), expected_cols)

  expect_equal(sort(unique(dfFlagged$Flag)), c(-2, -1, 0, 1, 2))

})


