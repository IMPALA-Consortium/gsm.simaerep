
Input_Rate(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt,
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site"
)

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
    strDenominatorDateCol  = "visit_dt"
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
    strDenominatorDateCol  = "visit_dt"
    ) %>%
    filter(
      Denominator == max(Denominator), .by = c(SubjectID)
    ) %>%
    arrange(SubjectID)

  dfRate <- Input_Rate(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = clindata::rawplus_ae %>% filter(! is.na(aest_dt)),
      dfDenominator = clindata::rawplus_visdt %>% filter(! is.na(lubridate::ymd(visit_dt))),
      strSubjectCol = "subjid",
      strGroupCol = "siteid",
      strGroupLevel = "Site"
    ) %>%
    select(- Metric) %>%
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
    strDenominatorDateCol  = "visit_dt"
  ) %>%
    filter(
      Denominator == max(Denominator), .by = c(SubjectID)
    ) %>%
    arrange(SubjectID)

  dfRate <- Input_Rate(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = clindata::ctms_protdev %>% rename(subjid = subjectenrollmentnumber) %>% filter(! is.na(deviationdate)),
      dfDenominator = clindata::rawplus_visdt %>% filter(! is.na(lubridate::ymd(visit_dt))),
      strSubjectCol = "subjid",
      strGroupCol = "siteid",
      strGroupLevel = "Site"
    ) %>%
    select(- Metric) %>%
    arrange(SubjectID)

  expect_equal(dfCumCount, dfRate)

})

test_that("As Denominator count increases Numerator count must never decrease", {


})

test_that("Same-day Numerator and Denominator Events must always be matched", {


})

test_that("AssignOrphans - Orphaned Numerator events will not be dropped if GroupID available", {


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


test_that("handling of zero denominators and missing data", {
  subjects <- data.frame(
    SubjectID = 1:4,
    GroupID = 10:13
  )
  numerators <- data.frame(
    SubjectID = c(1, 1),
    GroupID = 10:11
  )
  denominators <- data.frame(
    SubjectID = c(1, 2),
    GroupID = 12:13
  )

  result <- Input_Rate(subjects, numerators, denominators)

  expected <- data.frame(
    SubjectID = 1:4,
    GroupID = 10:13,
    GroupLevel = "GroupID",
    Numerator = c(2, 0, 0, 0),
    Denominator = c(1, 1, 0, 0),
    Metric = c(2, 0, NaN, NaN) # NaN because denominator is zero
  )

  expect_equal(result, expected)
})

test_that("if dfNumerator has 0 row data", {
  subjects <- data.frame(
    SubjectID = 1:4,
    GroupID = 10:13
  )
  numerators <- data.frame(
    SubjectID = numeric(),
    GroupID = numeric()
  )
  denominators <- data.frame(
    SubjectID = c(1, 2),
    GroupID = 12:13
  )

  result <- Input_Rate(subjects, numerators, denominators)

  expected <- data.frame(
    SubjectID = 1:4,
    GroupID = 10:13,
    GroupLevel = "GroupID",
    Numerator = c(0, 0, 0, 0),
    Denominator = c(1, 1, 0, 0),
    Metric = c(0, 0, NaN, NaN) # NaN because denominator is zero
  )

  expect_equal(result, expected)
})



test_that("yaml workflow produces same table as R function", {
  source(test_path("testdata", "create_double_data.R"), local = TRUE)
  expect_equal(dfInput$SubjectID, lResults$Analysis_kri0001$Analysis_Input$SubjectID)
  expect_equal(dim(dfInput), dim(lResults$Analysis_kri0001$Analysis_Input))
})


test_that("duckdb returns same result as R function", {
  source(test_path("testdata", "create_double_data.R"), local = TRUE)
  db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  duckdb::dbWriteTable(db, "subjects", clindata::rawplus_dm)
  duckdb::dbWriteTable(db, "numerators", clindata::rawplus_ae)
  duckdb::dbWriteTable(db, "denominators", clindata::rawplus_visdt)
  duckdb::dbWriteTable(db, "results", lResults$Analysis_kri0001$Analysis_Input)

  query <- "
  SELECT * FROM results
  "
  result <- duckdb::dbGetQuery(db, query)

  expect_equal(result$SubjectID, lResults$Analysis_kri0001$Analysis_Input$SubjectID)
  expect_equal(dim(result), dim(lResults$Analysis_kri0001$Analysis_Input))
})