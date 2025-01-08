#'Analyze Simaerep
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculate over and under-reporting probabilities for numerator denominator ratio using `simaerep`.
#'
#' Uses inframe `simaerep` method and does not apply multiplicity correction as per latest recommendations.
#'
#' This function takes dfInput data frame as returned by Input_CumCount()
#'
#' @param dfInput `data.frame` as returned by Input_CumCount()
#'
#' @return `data.frame` with the following specification:
#'
#' | Column Name              | Description                                  | Type     |
#' |--------------------------|----------------------------------------------|----------|
#' | GroupID                  | The group ID                                 | Character|
#' | MetricExpected           | Expected ratio from simulations              | Numeric  |
#' | MetricGroup              | Ratio all subjects in GroupID                | Numeric  |
#' | OverReportingProbability | Probability over-reporting numerator events  | Numeric  |
#' | OverReportingProbability | Probability under-reporting numerator events | Numeric  |
#'@export
#'@examples
#' # {clindata} Example for cumulative AE per Visit Count
#' dfInput <- Input_CumCount(
#'     dfSubjects = clindata::rawplus_dm,
#'     dfNumerator = clindata::rawplus_ae,
#'     dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = lubridate::ymd(visit_dt)),
#'     strSubjectCol = "subjid",
#'     strGroupCol = "siteid",
#'     strGroupLevel = "Site",
#'     strNumeratorDateCol = "aest_dt",
#'     strDenominatorDateCol  = "visit_dt"
#'   )
#'
#' Analyze_Simaerep(dfInput)
#'
Analyze_Simaerep <- function(dfInput) {

  stopifnot(all(
    c("SubjectID", "GroupID", "GroupLevel", "Numerator", "Denominator") %in% colnames(dfInput)
  ))

  stopifnot(
    "Multiple rows per SubjectId expected. Use Input_CumCount() for dfInput!" =
    pull(count(dfInput), .data$n) >  pull(count(distinct(dfInput, .data$SubjectID)), .data$n)
  )

  colmaps <- c(
    site_number = "GroupID",
    patnum = "SubjectID",
    visit = "Denominator",
    n_ae = "Numerator",
    events_per_visit_study = "MetricExpected",
    events_per_visit_site = "MetricGroup"
  )

  colmaps_inverse <- names(colmaps)
  names(colmaps_inverse) <- colmaps

  eventrep <- dfInput %>%
    rename(
      any_of(colmaps)
    ) %>%
    mutate(
      study_id = "A"
    ) %>%
    simaerep::simaerep(
      r = 1000,
      check = FALSE, # input checks not necessary
      under_only = FALSE,
      visit_med75 = FALSE,
      inframe = TRUE,
      mult_corr = FALSE,
      progress = TRUE
    )

  dfAnalyze <- eventrep$df_eval %>%
    mutate(
      OverReportingProbability = 1 - .data$prob_high,
      UnderReportingProbability = 1 - .data$prob_low
    ) %>%
    select(any_of(c(
      colmaps_inverse,
      "OverReportingProbability",
      "UnderReportingProbability"
    )))
}
