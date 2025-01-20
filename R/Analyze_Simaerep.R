#' Analyze Simaerep
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculate over and under-reporting probabilities for numerator denominator ratio using [simaerep::simaerep()].
#'
#' Uses inframe `simaerep` method and does not apply multiplicity correction as per latest recommendations.
#'
#' The sum of under- and over reporting probability  per group is usually zero, however [simaerep::simaerep()]
#' adjusts edge cases in which group ratio and expected ratio are all zero. We combine both metrics into a score
#' between 0 and 1, where 0 is under-reporting and 1 is over-reporting.
#'
#' @param dfInput `data.frame` as returned by [Input_CumCount()]
#' @inheritParams simaerep::simaerep
#'
#' @return `data.frame` with the following specification:
#'
#' | Column Name               | Description                                  | Type     |
#' |---------------------------|----------------------------------------------|----------|
#' | GroupID                   | The group ID                                 | Character|
#' | GroupLevel                | The group level                              | Character|
#' | Numerator                 | Numerator events                             | Numeric  |
#' | Denominator               | Denominator events                           | Numeric  |
#' | MetricExpected            | Expected ratio from simulations              | Numeric  |
#' | Metric                    | Ratio all subjects in GroupID                | Numeric  |
#' | OverReportingProbability  | Probability over-reporting numerator events  | Numeric  |
#' | UnderReportingProbability | Probability under-reporting numerator events | Numeric  |
#' | Score                     | Combined Score between                       | Numeric  |
#'
#' @seealso [simaerep::simaerep()], [Input_CumCount()]
#' @export
#' @examples
#' # {clindata} Example for cumulative AE per Visit Count
#' dfInput <- Input_CumCount(
#'   dfSubjects = clindata::rawplus_dm,
#'   dfNumerator = clindata::rawplus_ae,
#'   dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = lubridate::ymd(visit_dt)),
#'   strSubjectCol = "subjid",
#'   strGroupCol = "siteid",
#'   strGroupLevel = "Site",
#'   strNumeratorDateCol = "aest_dt",
#'   strDenominatorDateCol = "visit_dt"
#' )
#'
#' Analyze_Simaerep(dfInput)
#'
Analyze_Simaerep <- function(dfInput, r = 1000) {
  stopifnot(all(
    c("SubjectID", "GroupID", "GroupLevel", "Numerator", "Denominator") %in% colnames(dfInput)
  ))

  stopifnot(
    "Multiple rows per SubjectId expected. Use Input_CumCount() for dfInput!" =
      pull(count(dfInput), .data$n) > pull(count(distinct(dfInput, .data$SubjectID)), .data$n)
  )

  colmaps <- c(
    site_number = "GroupID",
    GroupLevel = "GroupLevel",
    patnum = "SubjectID",
    n_ae = "Numerator",
    visit = "Denominator",
    events_per_visit_study = "MetricExpected",
    events_per_visit_site = "Metric"
  )

  grouplvl <- dfInput %>%
    pull(.data$GroupLevel) %>%
    unique()

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
      r = r,
      check = FALSE, # input checks not necessary
      under_only = FALSE,
      visit_med75 = FALSE,
      inframe = TRUE,
      mult_corr = FALSE,
      progress = TRUE
    )

  dfInputCount <- dfInput %>%
    filter(
      .data$Denominator == max(.data$Denominator, na.rm = TRUE),
      .by = c("GroupID", "SubjectID")
    ) %>%
    summarize(
      visit = sum(.data$Denominator, na.rm = TRUE),
      n_ae = sum(.data$Numerator, na.rm = TRUE),
      .by = c("GroupID")
    )

  dfAnalyze <- eventrep$df_eval %>%
    left_join(
      dfInputCount,
      by = c(site_number = "GroupID")
    ) %>%
    mutate(
      GroupLevel = .env$grouplvl,
      OverReportingProbability = 1 - .data$prob_high,
      UnderReportingProbability = 1 - .data$prob_low,
      Score = case_when(
        .data$OverReportingProbability + .data$UnderReportingProbability == 1 ~ .data$OverReportingProbability,
        .data$OverReportingProbability >= .data$UnderReportingProbability ~ .data$OverReportingProbability,
        .data$OverReportingProbability < .data$UnderReportingProbability ~ 1 - .data$UnderReportingProbability
      ),
      Score = ifelse(
        .data$OverReportingProbability >= .data$UnderReportingProbability,
        .data$OverReportingProbability,
        -.data$UnderReportingProbability
      )
    ) %>%
    select(any_of(c(
      colmaps_inverse,
      "OverReportingProbability",
      "UnderReportingProbability",
      "Score"
    ))) %>%
    collect() %>%
    arrange(.data$GroupID)

  return(dfAnalyze)
}
