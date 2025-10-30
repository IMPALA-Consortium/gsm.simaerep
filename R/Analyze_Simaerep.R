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
#' | Metric                    | Ratio all subjects in GroupID                | Numeric  |
#' | Score                     | Reporting Score                              | Numeric  |
#' | ScoreMult                 | Reporting Score with Multiplicity Correction | Numeric  |
#' | ExpectedNumerator         | Count of Expected Numerator Events           | Numeric  |
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
    study_id = "StudyID",
    site_id = "GroupID",
    GroupLevel = "GroupLevel",
    patient_id = "SubjectID",
    n_event = "Numerator",
    visit = "Denominator"
  )

  grouplvl <- dfInput %>%
    pull(.data$GroupLevel) %>%
    unique()

  eventrep <- dfInput %>%
    mutate(
      StudyID = "A"
    ) %>%
    simaerep::simaerep(
      r = r,
      check = FALSE, # input checks not necessary
      under_only = FALSE,
      visit_med75 = FALSE,
      inframe = TRUE,
      mult_corr = TRUE,
      progress = TRUE,
      col_names = colmaps
    )

  dfInputCount <- dfInput %>%
    filter(
      .data$Denominator == max(.data$Denominator, na.rm = TRUE),
      .by = c("GroupID", "SubjectID")
    ) %>%
    summarize(
      Denominator = sum(.data$Denominator, na.rm = TRUE),
      Numerator = sum(.data$Numerator, na.rm = TRUE),
      .by = c("GroupID")
    )

  dfAnalyze <- eventrep$df_eval %>%
    left_join(
      dfInputCount,
      by = c(GroupID = "GroupID")
    ) %>%
    mutate(
      GroupLevel = .env$grouplvl,
      Metric = .data$event_per_visit_site,
      Score = .data$event_prob_no_mult,
      ScoreMult = .data$event_prob,
      ExpectedNumerator = .data$event_delta
    ) %>%
    select(any_of(c(
      unname(colmaps),
      "Metric",
      "Score",
      "ScoreMult",
      "ExpectedNumerator"
    ))) %>%
    select(- "StudyID") %>%
    collect() %>%
    arrange(.data$GroupID)

  return(dfAnalyze)
}
