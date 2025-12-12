test_that("Visualize_Simaerep()", {
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

  p <- Visualize_Simaerep(dfInput, dfFlagged)

  expect_s3_class(p, "ggplot")

  vColors <- c("0" = "#3CAF04", "1" = "#FEAA01", "2" = "#FF5858", "-1" = "#FEAA01", "-2" = "#FF5858")

  p <- Visualize_Simaerep(dfInput, dfFlagged, vColors = vColors)

  expect_s3_class(p, "ggplot")
})
