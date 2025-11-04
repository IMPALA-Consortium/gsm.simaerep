test_that("ExtrapolateDenominator", {

    dfDenominator <- clindata::rawplus_visdt %>%
        mutate(visit_dt = lubridate::ymd(visit_dt))

    dfNumerator <- clindata::rawplus_studcomp

    vLikePatternInstanceName <- c("%unsch%", "%disc%")

    strInstanceNameCol <- "instancename"

    dfExtrapolated <- ExtrapolateDenominator(
      dfDenominator = dfDenominator,
      dfNumerator = dfNumerator,
      strSubjectCol = "subjid",
      strInstanceNameCol = strInstanceNameCol,
      vLikePatternInstanceName = vLikePatternInstanceName,
      strDenominatorDateCol = "visit_dt"
    )

    # visit count for subjects with numerator increased

    vist_count_num_extra <- dfExtrapolated %>%
      inner_join(
        dfNumerator %>%
          distinct(subjid),
        by = c("subjid")
      ) %>%
      nrow()

    visit_count_num <- dfDenominator %>%
      filter(! stringr::str_like(.data[[strInstanceNameCol]], "%unsch%")) %>%
      filter(! stringr::str_like(.data[[strInstanceNameCol]], "%disc%")) %>%
      inner_join(
        dfNumerator %>%
          distinct(subjid),
        by = c("subjid")
      ) %>%
      nrow()

    expect_true(vist_count_num_extra > visit_count_num)

    # visit count for subjects without numerator unchanged
    vist_count_no_num_extra <- dfExtrapolated %>%
      anti_join(
        dfNumerator %>%
          distinct(subjid),
        by = c("subjid")
      ) %>%
      nrow()

    visit_count_no_num <- dfDenominator %>%
      filter(! stringr::str_like(.data[[strInstanceNameCol]], "%unsch%")) %>%
      filter(! stringr::str_like(.data[[strInstanceNameCol]], "%disc%")) %>%
      anti_join(
        dfNumerator %>%
          distinct(subjid),
        by = c("subjid")
      ) %>%
      nrow()

    expect_equal(vist_count_no_num_extra, visit_count_no_num)

    # for extrapolated subjects no duplicated visit dates

    check_no_duplicated_visit_dates <- dfExtrapolated %>%
      summarise(
        n_visit_dt = n_distinct(visit_dt),
        n = n(),
        .by = c("subjid")
      ) %>%
      mutate(
        # we allow for an error marrgin of 1 to account for atifacts in the data
        check = n - n_visit_dt <= 1
      ) %>%
      pull(check)

    expect_true(all(check_no_duplicated_visit_dates))

})


test_that("ExtrapolateDenominator called from Input_CumCount()", {

    dfDenominator <- clindata::rawplus_visdt %>%
        mutate(visit_dt = lubridate::ymd(visit_dt))

    dfNumerator <- clindata::rawplus_studcomp %>%
      mutate(mincreated_dts = lubridate::ymd_hms(mincreated_dts))

    vLikePatternInstanceName <- c("%unsch%", "%disc%")

    strInstanceNameCol <- "instancename"

    dfInputExtra <- Input_CumCount(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strGroupCol = "siteid",
      strSubjectCol = "subjid",
      strGroupLevel = "Site",
      strNumeratorDateCol = "mincreated_dts",
      strDenominatorDateCol = "visit_dt",
      vLikePatternInstanceName = vLikePatternInstanceName,
      strInstanceNameCol = strInstanceNameCol,
      nMinSubjectRatioInstance = 0.7
    )

    dfInputRegular <- Input_CumCount(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strGroupCol = "siteid",
      strSubjectCol = "subjid",
      strGroupLevel = "Site",
      strNumeratorDateCol = "mincreated_dts",
      strDenominatorDateCol = "visit_dt"
    )

    MaxNumeratorExtra <- dfInputExtra %>%
      inner_join(
        dfNumerator %>%
          distinct(subjid),
        by = c("SubjectID" = "subjid")
      ) %>%
      pull(Numerator) %>%
      max()

    MedianSumSubjNumeratorExtra <- dfInputExtra %>%
      inner_join(
        dfNumerator %>%
          distinct(subjid),
        by = c("SubjectID" = "subjid")
      ) %>%
      summarise(
        SumSubjectNumeratorExtra = sum(Numerator),
        .by = c("SubjectID")
      ) %>%
      pull(SumSubjectNumeratorExtra) %>%
      median()

    MaxNumeratorRegular <- dfInputRegular %>%
      inner_join(
        dfNumerator %>%
          distinct(subjid),
        by = c("SubjectID" = "subjid")
      ) %>%
      pull(Numerator) %>%
      max()

    MedianSumSubjNumeratorRegular <- dfInputRegular %>%
      inner_join(
        dfNumerator %>%
          distinct(subjid),
        by = c("SubjectID" = "subjid")
      ) %>%
      summarise(
        SumSubjectNumeratorExtra = sum(Numerator),
        .by = c("SubjectID")
      ) %>%
      pull(SumSubjectNumeratorExtra) %>%
      median()

    expect_true(MaxNumeratorExtra == 1)
    expect_true(MaxNumeratorRegular == 1)
    expect_true(MedianSumSubjNumeratorExtra > MedianSumSubjNumeratorRegular)
    expect_true(MedianSumSubjNumeratorRegular == 1)

})


test_that("ExtrapolateDenominator called from Input_CumCount() with lazy table", {

  db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")

  dfDenominator <- clindata::rawplus_visdt %>%
    mutate(visit_dt = lubridate::ymd(visit_dt))

  dfNumerator <- clindata::rawplus_studcomp %>%
    mutate(mincreated_dts = lubridate::ymd_hms(mincreated_dts))

  vLikePatternInstanceName <- c("%unsch%", "%disc%")

  strInstanceNameCol <- "instancename"

  duckdb::dbWriteTable(db, "dm", clindata::rawplus_dm)
  duckdb::dbWriteTable(db, "visit", dfDenominator)
  duckdb::dbWriteTable(db, "disc", dfNumerator)

  DBI::dbExecute(db, "INSTALL 'icu';")
  DBI::dbExecute(db, "INSTALL 'icu';")

  tblInputExtra <- Input_CumCount(
    dfSubjects = dplyr::tbl(db, "dm"),
    dfNumerator = dplyr::tbl(db, "disc"),
    dfDenominator = dplyr::tbl(db, "visit"),
    strGroupCol = "siteid",
    strSubjectCol = "subjid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "mincreated_dts",
    strDenominatorDateCol = "visit_dt",
    vLikePatternInstanceName = vLikePatternInstanceName,
    strInstanceNameCol = strInstanceNameCol,
    nMinSubjectRatioInstance = 0.7
  )

  expect_s3_class(tblInputExtra, "tbl_lazy")

  dfInputExtra <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strGroupCol = "siteid",
    strSubjectCol = "subjid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "mincreated_dts",
    strDenominatorDateCol = "visit_dt",
    vLikePatternInstanceName = vLikePatternInstanceName,
    strInstanceNameCol = strInstanceNameCol,
    nMinSubjectRatioInstance = 0.7
  )

  dfInputExtraDuckDb <- collect(tblInputExtra) %>%
    arrange(GroupID, SubjectID, Denominator)

  expect_equal(nrow(dfInputExtra), nrow(dfInputExtraDuckDb))
  expect_identical(dfInputExtra, dfInputExtraDuckDb)

})
