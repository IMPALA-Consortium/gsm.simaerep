test_that("test Input_CumCount PD", {
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
  expect_equal(colnames(dfInput), c("SubjectID", "GroupID", "GroupLevel", "Numerator", "Denominator"))
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
    strDenominatorDateCol = "visit_dt"
  )
  expect_equal(colnames(dfInput), c("SubjectID", "GroupID", "GroupLevel", "Numerator", "Denominator"))
})

test_that("Input_CumCount same final count as Input_Rate for AEs and Visits with valid dates", {
  dfCumCount <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  ) %>%
    filter(
      Denominator == max(Denominator),
      .by = c(SubjectID)
    ) %>%
    arrange(SubjectID)

  dfRate <- gsm.core::Input_Rate(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae %>% filter(!is.na(aest_dt)),
    dfDenominator = clindata::rawplus_visdt %>% filter(!is.na(lubridate::ymd(visit_dt))),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site"
  ) %>%
    select(-Metric) %>%
    arrange(SubjectID)

  expect_equal(dfCumCount, dfRate)
})


test_that("Input_CumCount same final count as Input_Rate for PDs and Visits with valid dates", {
  dfCumCount <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::ctms_protdev %>% rename(subjid = subjectenrollmentnumber),
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol = "visit_dt"
  ) %>%
    filter(
      Denominator == max(Denominator),
      .by = c(SubjectID)
    ) %>%
    arrange(SubjectID)

  dfRate <- gsm.core::Input_Rate(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::ctms_protdev %>% rename(subjid = subjectenrollmentnumber) %>% filter(!is.na(deviationdate)),
    dfDenominator = clindata::rawplus_visdt %>% filter(!is.na(lubridate::ymd(visit_dt))),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site"
  ) %>%
    select(-Metric) %>%
    arrange(SubjectID)

  expect_equal(dfCumCount, dfRate)
})

test_that("As Denominator count increases Numerator count must never decrease", {
  dfCumCount <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  never_decrease <- dfCumCount %>%
    arrange(SubjectID, Denominator) %>%
    group_by(SubjectID) %>%
    mutate(
      DenominatorIncrease = Denominator - tidyr::replace_na(lag(Denominator), 0),
      never_decrease = DenominatorIncrease >= 0
    ) %>%
    ungroup() %>%
    dplyr::pull(never_decrease) %>%
    all()

  expect_true(never_decrease)
})

test_that("Check Numerator Events befor/after first/last Denominator and same day as Denominator", {
  # dfSubjects tibble with one subject and one site
  dfSubjects <- tibble(
    SubjectID = 1,
    GroupID = 1
  )

  # dfNumerator tibble with one subject 10 AEs, two of which on same day
  dfNumerator <- tibble(
    SubjectID = rep(1, 10),
    aest_dt = as.Date("2000-01-01") + c(months(0:4), rep(months(7), 2), months(9:11)),
  ) %>%
    mutate(
      aest_dt = aest_dt + lubridate::hours(12)
    )

  # dfDenominator tibble with one subject 4 visits, one on same day as two Numerator events
  # Denominator time indicates that they occurr before Numerator events, which should be ignored
  dfDenominator <- tibble(
    SubjectID = rep(1, 4),
    visit_dt = c(as.Date(c("2000-01-03", "2000-04-12", "2000-08-01", "2000-11-12")))
  ) %>%
    mutate(
      visit_dt = visit_dt + lubridate::hours(1)
    )

  dfCumCount <- Input_CumCount(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strSubjectCol = "SubjectID",
    strGroupCol = "GroupID",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  dfExpected <- tibble(
    SubjectID = rep(1, 4),
    GroupID = rep(1, 4),
    GroupLevel = "Site",
    Numerator = c(1, 4, 7, 10),
    Denominator = c(1, 2, 3, 4)
  )

  expect_equal(dfCumCount, dfExpected)
})

test_that("AssignOrphans - Orphaned Numerator events will not be dropped if GroupID available", {
  dfNumerator <- clindata::ctms_protdev %>%
    rename(subjid = subjectenrollmentnumber) %>%
    left_join(clindata::rawplus_dm %>% select(subjid, siteid), by = "subjid") %>%
    # set 30% of subjectid per subject to NA
    arrange(runif(n())) %>%
    mutate(rnk = row_number() / n(), .by = subjid) %>%
    mutate(subjid = ifelse(rnk < 0.3, NA, subjid)) %>%
    filter(!is.na(deviationdate), !is.na(siteid))

  dfCumCount <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = dfNumerator,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol = "visit_dt",
    strOrphanedMethod = "filter"
  ) %>%
    filter(Denominator == max(Denominator), .by = c(SubjectID))

  expect_equal(nrow(filter(dfNumerator, !is.na(subjid))), sum(dfCumCount$Numerator))

  dfCumCountOrphans <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = dfNumerator,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol = "visit_dt",
    strOrphanedMethod = "assign"
  ) %>%
    filter(Denominator == max(Denominator), .by = c(SubjectID))

  # it is okay when not 100% of all orphaned numerators are assigned when they did not
  # occurr between first and last + 30 days denominator event
  expect_true(
    dplyr::between(
      sum(dfCumCountOrphans$Numerator),
      nrow(dfNumerator) * 0.9,
      nrow(dfNumerator)
    )
  )

  # Orphaned events must not always get assigned to only one patient per site

  n_pats_with_increase_per_site <- dfCumCount %>%
    select(SubjectID, GroupID, NumberatorOriginal = Numerator) %>%
    left_join(
      dfCumCountOrphans %>%
        select(SubjectID, GroupID, NumeratorDist = Numerator),
      by = c("SubjectID", "GroupID")
    ) %>%
    mutate(
      has_increase = NumeratorDist > NumberatorOriginal
    ) %>%
    summarize(
      n_increase = sum(has_increase),
      n = n(),
      .by = GroupID
    ) %>%
    dplyr::pull(n_increase)

  expect_true(any(n_pats_with_increase_per_site > 1))

  # Orphaned Numerator events also need to be assigned to patients
  # with no original Numerator events.

  NewPatientsWithNumerator <- dfCumCount %>%
    select(SubjectID, GroupID, NumberatorOriginal = Numerator) %>%
    left_join(
      dfCumCountOrphans %>%
        select(SubjectID, GroupID, NumeratorDist = Numerator),
      by = c("SubjectID", "GroupID")
    ) %>%
    filter(NumberatorOriginal == 0, NumeratorDist > 0)

  expect_true(nrow(NewPatientsWithNumerator) > 0)
})

test_that("all input data frames must be non-null", {
  expect_error(Input_CumCount(NULL, data.frame(), data.frame()))
  expect_error(Input_CumCount(data.frame(), NULL, data.frame()))
  expect_error(Input_CumCount(data.frame(), data.frame(), NULL))
})


test_that("strSubjectCol must exist in all data frames", {
  test_df_1 <- data.frame(SubjectID = 1:5)
  test_df_2 <- data.frame(DifferentID = 1:5)
  expect_error(Input_CumCount(test_df_1, test_df_1, test_df_2, strSubjectCol = "SubjectID"))
})

test_that("Input_CumCount() - use prexisting eventIDs", {
  dfInput <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae %>%
      mutate(numid = row_number()),
    dfDenominator = clindata::rawplus_visdt %>%
      mutate(
        visit_dt = lubridate::ymd(visit_dt),
        denomid = row_number()
      ),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt",
    strNumeratorCol = "numid",
    strDenominatorCol = "denomid",
  )

  expect_equal(colnames(dfInput), c("SubjectID", "GroupID", "GroupLevel", "Numerator", "Denominator"))
})

test_that("Input_CumCount() - w/o specifying strGroupLevel", {
  dfInput <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = NULL,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  expect_equal(unique(dfInput$GroupLevel), "siteid")
})

test_that("Input_CumCount() - results must not change when strOrphanedMethod == 'assign' w/o oprhans", {
  dfInput <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = NULL,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  dfInputOrph <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = NULL,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt",
    strOrphanedMethod = "assign"
  )
  expect_equal(dfInput, dfInputOrph)
})



test_that("AssignOrphans used with lazy_tbl ", {
  dfNumerator <- clindata::ctms_protdev %>%
    rename(subjid = subjectenrollmentnumber) %>%
    left_join(clindata::rawplus_dm %>% select(subjid, siteid), by = "subjid") %>%
    # set 30% of subjectid per subject to NA
    arrange(runif(n())) %>%
    mutate(rnk = row_number() / n(), .by = subjid) %>%
    mutate(subjid = ifelse(rnk < 0.3, NA, subjid)) %>%
    filter(!is.na(deviationdate), !is.na(siteid))

  db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")

  duckdb::dbWriteTable(db, "dm", clindata::rawplus_dm)
  duckdb::dbWriteTable(db, "visit", clindata::rawplus_visdt)
  duckdb::dbWriteTable(db, "pd", dfNumerator)

  dfCumCountOrphans_duckdb <- Input_CumCount(
      dfSubjects = dplyr::tbl(db, "dm"),
      dfNumerator = dplyr::tbl(db, "pd"),
      dfDenominator = dplyr::tbl(db, "visit") %>% mutate(visit_dt = sql("TRY_CAST(visit_dt AS DATE)")),
      strSubjectCol = "subjid",
      strGroupCol = "siteid",
      strGroupLevel = "Site",
      strNumeratorDateCol = "deviationdate",
      strDenominatorDateCol = "visit_dt",
      strOrphanedMethod = "assign"
    ) %>%
    filter(Denominator == max(Denominator, na.rm = TRUE), .by = c(SubjectID)) %>%
    dplyr::collect() %>%
    arrange(GroupID, SubjectID, Denominator)

  # it is okay when not 100% of all orphaned numerators are assigned when they did not
  # occur between first and last + 30 days denominator event
  expect_true(
    dplyr::between(
      sum(dfCumCountOrphans_duckdb$Numerator),
      nrow(dfNumerator) * 0.9,
      nrow(dfNumerator)
    )
  )

  DBI::dbDisconnect(db)
})
