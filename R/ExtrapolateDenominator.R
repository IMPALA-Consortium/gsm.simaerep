
#' Extrapolate Denominator Events
#' @description internal function called by [Input_CumCount()]
#' Filters all instances not matching vLikePatternInstanceName that occur
#' for a minimum ratio of subjects. Attempts to estimate the order of instances
#' from subject-level instance dates and to establish the median time difference
#' between instances. Missing instances for subjects with entries in numerator
#' data frame will be added using the median days between instances to extrapolate
#' dates. No instances will be added if extrapolated date exceeds the maximum instance
#' date for subjects without numerator entries.
#'@inheritParams Input_CumCount
#'@export
ExtrapolateDenominator <- function(dfDenominator,
                                   dfNumerator,
                                   strSubjectCol,
                                   strDenominatorDateCol,
                                   strInstanceNameCol,
                                   vLikePatternInstanceName = c("%unsch%", "%disc%"),
                                   nMinSubjectRatioInstance = 0.7) {

  FUN <- dplyr::arrange

  vLikePatternInstanceName <- stringr::str_to_lower(vLikePatternInstanceName)

  df_filt <- dfDenominator

  for (i in seq_along(vLikePatternInstanceName)) {

    if (inherits(df_filt, "data.frame")) {
      df_filt <- df_filt %>%
        filter(! stringr::str_like(.data[[strInstanceNameCol]], .env$vLikePatternInstanceName[i]))
    } else {
      query <- glue::glue("LOWER({strInstanceNameCol}) NOT LIKE '{vLikePatternInstanceName[i]}'")

      df_filt <- df_filt %>%
        filter(sql(query))
    }

  }

  df_prep <- df_filt %>%
    SortDf(
      .data[[strSubjectCol]],
      .data[[strDenominatorDateCol]],
      .data[[strInstanceNameCol]]
    ) %>%
    mutate(
      Days = as.numeric(
          .data[[strDenominatorDateCol]] - dplyr::lag(.data[[strDenominatorDateCol]])
      ),
      Days = coalesce(.data$Days, 0),
      Rank = row_number(),
      .by = all_of(c(strSubjectCol))
    ) %>%
    select(all_of(c(strSubjectCol, strDenominatorDateCol, strInstanceNameCol, "Days", "Rank")))

  df_mode_rank <- df_prep %>%
    mutate(
      SubjectCount = n_distinct(.data[[strSubjectCol]])
    ) %>%
    mutate(
      SubjectRatioInstance = n_distinct(.data[[strSubjectCol]]) / .data$SubjectCount,
      .by = all_of(c(strInstanceNameCol))
    ) %>%
    summarise(
      RankCount = dplyr::n(),
      .by = all_of(c(strInstanceNameCol, "Rank", "SubjectRatioInstance"))
    ) %>%
    mutate(
      ModeRank = dense_rank(desc(.data$RankCount)),
      .by = all_of(c(strInstanceNameCol))
    ) %>%
    filter(.data$ModeRank == 1, .data$SubjectRatioInstance > .env$nMinSubjectRatioInstance) %>%
    SortDf(.data$Rank)

  stopifnot(
    "instance rank could not be determined uniquely" =
      pull(summarize(df_mode_rank, n = n()), "n") == n_distinct(pull(df_mode_rank, .data$Rank))
  )

  df_days <- df_prep %>%
    summarise(
      DaysMedian = median(.data$Days, na.rm = TRUE),
      .by = all_of(strInstanceNameCol)
    ) %>%
    mutate(
      DaysMedian = coalesce(.data$DaysMedian, 0)
    ) %>%
    inner_join(
      df_mode_rank,
      by = strInstanceNameCol
    )

  # we extrapolate for all subjects in numerator only
  df_subj_days <- dfNumerator %>%
    select(all_of(strSubjectCol)) %>%
    cross_join(
      df_days
    ) %>%
    SortDf(.data[[strSubjectCol]], .data$Rank)


  df_extr <- df_filt %>%
    select(all_of(c(strSubjectCol, strInstanceNameCol, strDenominatorDateCol))) %>%
    mutate(
      MaxDenominatorDateAll = max(.data[[strDenominatorDateCol]], na.rm = TRUE),
      MaxDenominatorDateAll = case_when(
        MaxDenominatorDateAll > lubridate::today() ~ lubridate::today(),
        TRUE ~ MaxDenominatorDateAll
      )
    ) %>%
    # filter only subjects in numerator
    inner_join(
      select(dfNumerator, all_of(strSubjectCol)),
      by = strSubjectCol
    ) %>%
    full_join(
      df_subj_days,
      by = c(strSubjectCol, strInstanceNameCol)
    ) %>%
    SortDf(.data[[strSubjectCol]], .data$Rank) %>%
    mutate(
      # we need to remove NAs from MaxDenominatorDateAll
      MaxDenominatorDateAll = max(.data$MaxDenominatorDateAll, na.rm = TRUE),
      DaysMedian = ifelse(is.na(.data[[strDenominatorDateCol]]), .data$DaysMedian, 0),
      DaysCumSum = cumsum(.data$DaysMedian),
      MaxDenominatorDateNum = max(.data[[strDenominatorDateCol]], na.rm = TRUE),
      .by = all_of(c(strSubjectCol))
    ) %>%
    mutate(
      ExtraDenominatorDate = case_when(
        is.na(.data[[strDenominatorDateCol]]) ~ MaxDenominatorDateNum + lubridate::days(.data$DaysCumSum),
        TRUE ~ .data[[strDenominatorDateCol]]
      )
    ) %>%
    # extrapolated dates may not be larger than the max denominator date observed
    filter(.data$ExtraDenominatorDate <= .data$MaxDenominatorDateAll)


  # combine extrapolated with non-extrapolated denominator records
  dfDenominatorExtra <- dplyr::union_all(
    df_extr %>%
      mutate(
        {{strDenominatorDateCol}} := .data$ExtraDenominatorDate
      ) %>%
      select(all_of(c(strSubjectCol, strInstanceNameCol, strDenominatorDateCol))),
    df_filt %>%
      anti_join(
        select(dfNumerator, all_of(strSubjectCol)),
        by = strSubjectCol
      ) %>%
      select(all_of(c(strSubjectCol, strInstanceNameCol, strDenominatorDateCol))),
    ) %>%
    SortDf(.data[[strSubjectCol]], .data[[strDenominatorDateCol]])


  return(dfDenominatorExtra)

}
