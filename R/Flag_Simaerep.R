#'Flag Simaerep
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Flag over and under-reporting probabilities. The sum of under- and over reporting probability
#' per group is usually zero, however [simaerep::simaerep()] adjusts edge cases in which group ratio
#' and expected ratio are all zero'
#'
#' @param dfAnalyzed `data.frame` as returned by [Analyze_Simaerep()]
#' @param vThreshold `numeric` vector of thresholds to flag groups. Must be between 0 and 1.
#'
#' @return `data.frame` with the following specification:
#'
#' | Column Name               | Description                                  | Type     |
#' |---------------------------|----------------------------------------------|----------|
#' | GroupID                   | The group ID                                 | Character|
#' | MetricExpected            | Expected ratio from simulations              | Numeric  |
#' | MetricGroup               | Ratio all subjects in GroupID                | Numeric  |
#' | OverReportingProbability  | Probability over-reporting numerator events  | Numeric  |
#' | UnderReportingProbability | Probability under-reporting numerator events | Numeric  |
#' | Flag                      | Flag for group                               | Numeric  |
#' @seealso [Analyze_Simaerep()]
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
#' dfAnalyzed <- Analyze_Simaerep(dfInput)
#' Flag_Simaerep(dfAnalyzed, vThreshold = c(0.95, 0.99))

Flag_Simaerep <- function(dfAnalyzed, vThreshold) {

  vThreshold <- as.numeric(vThreshold)

  stopifnot("Theshold must not be NULL" = ! is.null(vThreshold))

  stopifnot("Threshold must not be NA" = ! any(is.na(vThreshold)))

  stopifnot("Thresholds must be between 0 and 1!" = all(between(vThreshold, 0, 1)))

  stopifnot("Thresholds mus be unique" = length(vThreshold) == length(unique(vThreshold)))

  stopifnot("At least one Threshold must be passed" = length(vThreshold) > 0)

  vThreshold <- sort(vThreshold)

  dfFlagged <- dfAnalyzed %>%
    mutate(
      Flag = 0
    )

  for (i in seq_along(vThreshold)) {

    thresh <- vThreshold[i]

    dfFlagged <- dfFlagged %>%
      mutate(
        Flag = case_when(
          .data$OverReportingProbability >= .env$thresh ~ i,
          .data$UnderReportingProbability >= .env$thresh ~ - i,
          TRUE ~ .data$Flag
        )
      )

  }

  return(dfFlagged)
}
