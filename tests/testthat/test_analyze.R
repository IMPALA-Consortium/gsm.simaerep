expected_cols <- c(
  "GroupID",
  "GroupLevel",
  "Numerator",
  "Denominator",
  "Metric",
  "Score",
  "ScoreMult",
  "ExpectedNumerator"
)


test_that("test Analyze_Simaerep PD", {
  dfInput <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::ctms_protdev %>% rename(subjid = subjectenrollmentnumber),
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol = "visit_dt"
  )

  dfAnalyze <- Analyze_Simaerep(dfInput)

  expect_equal(colnames(dfAnalyze), expected_cols)
})

test_that("test Analyze_Simaerep AE", {
  dfInput <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  dfAnalyze <- Analyze_Simaerep(dfInput)

  expect_equal(colnames(dfAnalyze), expected_cols)
})
