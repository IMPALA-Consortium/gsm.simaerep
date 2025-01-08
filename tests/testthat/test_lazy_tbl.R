test_that("lazy_tbl input returns same results as with data.frame", {

  dfInputAE <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol  = "visit_dt"
  )

  dfInputPD <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::ctms_protdev %>% rename(subjid = subjectenrollmentnumber),
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol  = "visit_dt"
  )

  dfAnalyzedAE <- Analyze_Simaerep(dfInputAE)
  dfAnalyzedPD <- Analyze_Simaerep(dfInputPD)

  db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")

  duckdb::dbWriteTable(db, "dm", clindata::rawplus_dm)
  duckdb::dbWriteTable(db, "ae", clindata::rawplus_ae)
  duckdb::dbWriteTable(db, "visit", clindata::rawplus_visdt)
  duckdb::dbWriteTable(db, "pd", clindata::ctms_protdev)

  dfInputAE_duckdb <- Input_CumCount(
    dfSubjects = dplyr::tbl(db, "dm"),
    dfNumerator = dplyr::tbl(db, "ae"),
    dfDenominator = dplyr::tbl(db, "visit") %>% mutate(visit_dt = sql("TRY_CAST(visit_dt AS DATE)")),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol  = "visit_dt"
  ) %>%
    dplyr::collect() %>%
    arrange(GroupID, SubjectID, Denominator)

  expect_equal(dfInputAE, dfInputAE_duckdb)

  dfInputPD_duckdb <- Input_CumCount(
    dfSubjects = dplyr::tbl(db, "dm"),
    dfNumerator = dplyr::tbl(db, "pd") %>% rename(subjid = subjectenrollmentnumber),
    dfDenominator = dplyr::tbl(db, "visit") %>% mutate(visit_dt = sql("TRY_CAST(visit_dt AS DATE)")),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol  = "visit_dt"
  ) %>%
    dplyr::collect() %>%
    arrange(GroupID, SubjectID, Denominator)

  expect_equal(dfInputPD, dfInputPD_duckdb)

  dfAnalyzedAE_duckdb <- Analyze_Simaerep(dfInputAE_duckdb) %>%
    dplyr::collect() %>%
    arrange(GroupID)

  dfAnalyzedPD_duckdb <- Analyze_Simaerep(dfInputPD_duckdb) %>%
    dplyr::collect() %>%
    arrange(GroupID)

  # we can only check the non-random elements for equality
  expect_equal(
    select(dfAnalyzedAE, - MetricExpected, - OverReportingProbability, - UnderReportingProbability),
    select(dfAnalyzedAE_duckdb, - MetricExpected, - OverReportingProbability, - UnderReportingProbability)
  )

  expect_equal(
    select(dfAnalyzedPD, - MetricExpected, - OverReportingProbability, - UnderReportingProbability),
    select(dfAnalyzedPD_duckdb, - MetricExpected, - OverReportingProbability, - UnderReportingProbability)
  )

  DBI::dbDisconnect(db)

})
