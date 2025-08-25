test_that("lazy_tbl input returns same results as with data.frame", {
  # regular data.frame =================================================
  dfInputAE <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  dfInputPD <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::ctms_protdev %>% rename(subjid = subjectenrollmentnumber),
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol = "visit_dt"
  )

  dfAnalyzedAE <- Analyze_Simaerep(dfInputAE)
  dfAnalyzedPD <- Analyze_Simaerep(dfInputPD)

  dfFlaggedAE <- Flag_Simaerep(dfAnalyzedAE, vThreshold = c(0.01, 0.05, 0.95, 0.99))
  dfFlaggedPD <- Flag_Simaerep(dfAnalyzedPD, vThreshold = c(0.01, 0.05, 0.95, 0.99))

  # duckdb ==============================================================
  db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")

  duckdb::dbWriteTable(db, "dm", clindata::rawplus_dm)
  duckdb::dbWriteTable(db, "ae", clindata::rawplus_ae)
  duckdb::dbWriteTable(db, "visit", clindata::rawplus_visdt)
  duckdb::dbWriteTable(db, "pd", clindata::ctms_protdev)
  duckdb::dbWriteTable(db, "r", tibble(r = seq(1, 10000)))

  tblInputAE_duckdb <- Input_CumCount(
    dfSubjects = dplyr::tbl(db, "dm"),
    dfNumerator = dplyr::tbl(db, "ae"),
    dfDenominator = dplyr::tbl(db, "visit") %>% mutate(visit_dt = sql("TRY_CAST(visit_dt AS DATE)")),
    strSubjectCol = "subjid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  tblInputPD_duckdb <- Input_CumCount(
    dfSubjects = dplyr::tbl(db, "dm"),
    dfNumerator = dplyr::tbl(db, "pd") %>% rename(subjid = subjectenrollmentnumber),
    dfDenominator = dplyr::tbl(db, "visit") %>% mutate(visit_dt = sql("TRY_CAST(visit_dt AS DATE)")),
    strSubjectCol = "subjid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol = "visit_dt"
  )

  dfAnalyzedAE_duckdb <- Analyze_Simaerep(tblInputAE_duckdb, r = dplyr::tbl(db, "r"))
  dfAnalyzedPD_duckdb <- Analyze_Simaerep(tblInputPD_duckdb, r = dplyr::tbl(db, "r"))

  dfFlaggedAE_duckdb <- Flag_Simaerep(dfAnalyzedAE_duckdb, vThreshold = c(-0.99, -0.95, 0.95, 0.99))
  dfFlaggedPD_duckdb <- Flag_Simaerep(dfAnalyzedPD_duckdb, vThreshold = c(-0.99, -0.95, 0.95, 0.99))

  # compare results =======================================================

  # input ---------------------------------------------------
  dfInputPD_duckdb <- tblInputPD_duckdb %>%
    dplyr::collect() %>%
    arrange(GroupID, SubjectID, Denominator)

  dfInputAE_duckdb <- tblInputAE_duckdb %>%
    dplyr::collect() %>%
    arrange(GroupID, SubjectID, Denominator)

  expect_equal(dfInputAE, dfInputAE_duckdb)
  expect_equal(dfInputPD, dfInputPD_duckdb)

  expect_true(inherits(tblInputPD_duckdb, "tbl_lazy"))
  expect_true(inherits(tblInputAE_duckdb, "tbl_lazy"))

  # analyze ---------------------------------------------------
  expect_true(inherits(dfAnalyzedAE_duckdb, "data.frame"))
  expect_true(inherits(dfAnalyzedPD_duckdb, "data.frame"))

  # we can only check the non-random elements for equality
  expect_equal(
    select(dfAnalyzedAE, -ExpectedNumerator, -Score, -ScoreMult),
    select(dfAnalyzedAE_duckdb, -ExpectedNumerator, -Score, -ScoreMult)
  )

  expect_equal(
    select(dfAnalyzedPD, -ExpectedNumerator, -Score, -ScoreMult),
    select(dfAnalyzedPD_duckdb, -ExpectedNumerator, -Score, -ScoreMult)
  )

  # Flag----------------------------------------------------------
  # although Analyze_Simaerep collects the lazy tbl to memory we still compare the number of
  # flagged sites between in memory and duckdb version to show that we get comparable results

  n_sites_flaggedAE <- sum(dfFlaggedAE$Flag > 0)
  n_sites_flaggedPD <- sum(dfFlaggedPD$Flag > 0)

  n_sites_flaggedAE_duckdb <- sum(dfFlaggedAE_duckdb$Flag > 0)
  n_sites_flaggedPD_duckdb <- sum(dfFlaggedPD_duckdb$Flag > 0)

  tolerance <- dplyr::n_distinct(dfFlaggedAE$GroupID) * 0.02

  expect_true(between(n_sites_flaggedAE_duckdb, n_sites_flaggedAE - tolerance, n_sites_flaggedAE + tolerance))
  expect_true(between(n_sites_flaggedPD_duckdb, n_sites_flaggedPD - tolerance, n_sites_flaggedPD + tolerance))

  sites_flaggedAE2 <- dfFlaggedAE %>%
    filter(Flag == 2) %>%
    pull(GroupID)

  sites_flaggedAE <- dfFlaggedAE %>%
    filter(Flag > 0) %>%
    pull(GroupID)

  sites_flaggedPD2 <- dfFlaggedPD %>%
    filter(Flag == 2) %>%
    pull(GroupID)

  sites_flaggedPD <- dfFlaggedPD %>%
    filter(Flag > 0) %>%
    pull(GroupID)

  sites_flaggedAE_duckdb <- dfFlaggedAE_duckdb %>%
    filter(Flag > 0) %>%
    pull(GroupID)

  sites_flaggedAE2_duckdb <- dfFlaggedAE_duckdb %>%
    filter(Flag == 2) %>%
    pull(GroupID)

  sites_flaggedPD_duckdb <- dfFlaggedPD_duckdb %>%
    filter(Flag > 0) %>%
    pull(GroupID)

  sites_flaggedPD2_duckdb <- dfFlaggedPD_duckdb %>%
    filter(Flag > 2) %>%
    pull(GroupID)

  expect_true(all(sites_flaggedAE2 %in% sites_flaggedAE_duckdb))
  expect_true(all(sites_flaggedAE2_duckdb %in% sites_flaggedAE))

  expect_true(all(sites_flaggedPD2 %in% sites_flaggedPD_duckdb))
  expect_true(all(sites_flaggedPD2_duckdb %in% sites_flaggedPD))

  DBI::dbDisconnect(db)
})
