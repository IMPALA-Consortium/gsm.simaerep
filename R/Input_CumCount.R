#' Cumulative Numerator Event Count per Denominator Event Count
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculate a subject level cumulative numerator event count per denominator event count.
#'
#' This function takes in a list of data frames including dfSUBJ, dfNumerator, and dfDenominator,
#' and calculates a subject level cumulative numerator count per cumulative denominator count. Numerator
#' events between two denominator events are assigned to the later denominator event.
#'
#' Numerator events before first denominator event will be assigned to the first denominator event.
#' Numerator events after last denominator event will be assigned to last denominator event.
#'
#' When numerator events fall on the same day as a denominator event they will be assigned to that event
#' event if the date time columns imply that numerator event occurred after the denominator.
#'
#' The data requirements for the function are as follows:
#'
#' - dfSubjects: A data frame with columns for SubjectID and any other relevant subject information
#' - dfNumerator: A data frame with a column for SubjectID and `strNumeratorCol` if `strNumeratorMethod` is "Sum"
#' - dfDenominator: A data frame with a column for SubjectID and `strDenominatorCol` if `strDenominatorMethod` is "Sum"
#'
#' All other columns are dropped from the output data frame. GroupIDs will be added via dfSubjects if not present events
#' without GroupID and subjects without denominator or numerator event will be dropped.
#'
#' Numerator events with no SubjectID but with GroupID can get assigned to a random enrolled subject
#' of the same GroupID
#'
#' @param dfSubjects `data.frame` with columns for SubjectID and any other relevant subject information
#' @param dfNumerator `data.frame` with a column for SubjectID and `strNumeratorDateCol`
#' @param dfDenominator `data.frame` with a column for SubjectID and `strDenominatorDateCol`
#' @param strGroupCol `character` Column name in `dfSubjects` to use for grouping.
#' @param strGroupLevel `character` value for the group level. Default: NULL defaults to `strGroupCol`
#' @param strSubjectCol `character` Column name in `dfSubjects` to use for subject ID.
#' @param strNumeratorCol `character` Column name in `dfNumerator` to use for numerator ID. Default: NULL
#' @param strDenominatorCol `character` Column name in `dfDenominator` to use for denominator ID. Default: NULL
#' @param strNumeratorDateCol `character` Column name in `dfNumerator` to use for numerator calculation.
#' @param strDenominatorDateCol `character` Column name in `dfDenominator` to use for denominator calculation.
#' @param strOrphanedMethod `character` one_of("filter", "assign") filter orphaned numerator events or assign
#' to random patient enrolled at site the time of event. Default: "filter"
#'
#' @return `data.frame` with the following specification:
#'
#' | Column Name  | Description                          | Type     |
#' |--------------|--------------------------------------|----------|
#' | SubjectID    | The subject ID                       | Character|
#' | GroupID      | The group ID                         | Character|
#' | GroupLevel   | The group type                       | Character|
#' | Numerator    | Cumulative Count Numerator           | Numeric  |
#' | Denominator  | Cumulative Count Denominator         | Numeric  |
#'
#' @examples
#'
#' # dfSubjects tibble with one subject and one site
#' dfSubjects <- tibble::tibble(
#'   SubjectID = 1,
#'   GroupID = 1
#' )
#'
#' dfNumerator <- tibble::tibble(
#'   SubjectID = rep(1, 10),
#'   num_dt = as.Date("2000-01-01") + c(months(0:4), rep(months(7), 2), months(9:11)),
#' ) %>%
#'   dplyr::mutate(
#'     num_dt = num_dt + lubridate::hours(12)
#'   )
#'
#' # dfNumerator tibble with one subject 10 AEs, two of which on same day
#' dfNumerator
#'
#' dfDenominator <- tibble::tibble(
#'   SubjectID = rep(1, 4),
#'   denom_dt = c(as.Date(c("2000-01-03", "2000-04-12", "2000-08-01", "2000-11-12")))
#' ) %>%
#'   dplyr::mutate(
#'     denom_dt = denom_dt + lubridate::hours(1)
#'   )
#'
#' # dfDenominator tibble with one subject 4 visits, one on same day as two Numerator events
#' # Denominator time indicates that they occurr before Numerator events
#' dfDenominator
#'
#'
#' # numerator before first denominator rolls up to first denominator
#' # numerator after last denominator rolls back to last denominator
#' # two numerator events on 08-01 add to the 3rd denominator event on same day
#'
#' Input_CumCount(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfDenominator,
#'   strSubjectCol = "SubjectID",
#'   strGroupCol = "GroupID",
#'   strGroupLevel = "Site",
#'   strNumeratorDateCol = "num_dt",
#'   strDenominatorDateCol = "denom_dt"
#' )
#'
#'
#'
#' # {clindata} Example for cumulative AE per Visit Count
#' Input_CumCount(
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
#' @export

Input_CumCount <- function(
    dfSubjects,
    dfNumerator,
    dfDenominator,
    strGroupCol,
    strGroupLevel = NULL,
    strSubjectCol,
    strNumeratorCol = NULL,
    strDenominatorCol = NULL,
    strNumeratorDateCol,
    strDenominatorDateCol,
    strOrphanedMethod = c("filter", "assign")) {
  CheckDf(dfSubjects)
  CheckDf(dfDenominator)
  CheckDf(dfNumerator)

  # must be it
  strOrphanedMethod <- match.arg(strOrphanedMethod)

  # check that "strSubjectCol" is in all dfs and not all NULL
  stopifnot(
    strSubjectCol %in% colnames(dfSubjects),
    strSubjectCol %in% colnames(dfNumerator),
    strSubjectCol %in% colnames(dfDenominator)
  )

  CheckNotAllNA(dfSubjects, strSubjectCol)
  CheckNotAllNA(dfNumerator, strSubjectCol)
  CheckNotAllNA(dfDenominator, strSubjectCol)

  # check that "strGroupCol" is in dfSubjects
  stopifnot(strGroupCol %in% colnames(dfSubjects))

  # DateCol for Numerator and Denominator must be present in dfNumerator and dfDenominator
  # and have date type and must not be all NULL.
  stopifnot(
    strNumeratorDateCol %in% colnames(dfNumerator),
    strDenominatorDateCol %in% colnames(dfDenominator)
  )

  CheckNotAllNA(dfNumerator, strNumeratorDateCol)
  CheckNotAllNA(dfDenominator, strDenominatorDateCol)
  CheckDataType(dfNumerator, strNumeratorDateCol, lubridate::is.instant)
  CheckDataType(dfDenominator, strDenominatorDateCol, lubridate::is.instant)

  # If supplied strNumeratorCol and strDenominatorCol must be in dfNumerator and dfDenominator
  # and renamed to NumeratorID and DenominatorID. If not supplied use row_number() as default.
  if (!is.null(strNumeratorCol)) {
    stopifnot(strNumeratorCol %in% colnames(dfNumerator))
    dfNumerator <- dfNumerator %>%
      rename(EventID = {{ strNumeratorCol }})
  } else {
    dfNumerator <- dfNumerator %>%
      mutate(EventID = row_number())
  }

  if (!is.null(strDenominatorCol)) {
    stopifnot(strDenominatorCol %in% colnames(dfDenominator))
    dfDenominator <- dfDenominator %>%
      rename(EventID = {{ strDenominatorCol }})
  } else {
    dfDenominator <- dfDenominator %>%
      mutate(EventID = row_number())
  }

  # Filter dfNumerator and dfDenominator to only include events with ID and Date
  # and select distinct SubjectID, EventID, and Date. Create a new column for EventType

  dfNumerator <- dfNumerator %>%
    rename(
      "SubjectID" = !!strSubjectCol,
      "Date" = !!strNumeratorDateCol
    ) %>%
    mutate(EventType = "Numerator") %>%
    filter(
      !is.na(.data$Date),
      !is.na(.data$EventID)
    )

  dfDenominator <- dfDenominator %>%
    rename(
      "SubjectID" = !!strSubjectCol,
      "Date" = !!strDenominatorDateCol
    ) %>%
    mutate(EventType = "Denominator") %>%
    filter(
      !is.na(.data$Date),
      !is.na(.data$EventID),
      # denomoinator events must have subject id
      !is.na(.data$SubjectID)
    )

  dfNumerator <- AddGroupCol(dfNumerator, dfSubjects, strSubjectCol, strGroupCol, strGroupLevel)
  dfDenominator <- AddGroupCol(dfDenominator, dfSubjects, strSubjectCol, strGroupCol, strGroupLevel)

  if (strOrphanedMethod == "assign") {
    dfNumerator <- AssignOrphans(dfNumerator, dfDenominator)
  }

  dfNumerator <- dfNumerator %>%
    filter(!is.na(.data$SubjectID))

  dfEvents <- union_all(
    dfNumerator %>% distinct(.data$SubjectID, .data$GroupID, .data$GroupLevel, .data$EventID, .data$Date, .data$EventType),
    dfDenominator %>% distinct(.data$SubjectID, .data$GroupID, .data$GroupLevel, .data$EventID, .data$Date, .data$EventType)
  )

  dfCumCount <- dfEvents %>%
    mutate(
      # control numerator/denominator order.
      Date = as.Date(.data$Date),
      Date = ifelse(
        .data$EventType == "Numerator",
        .data$Date + lubridate::hours(1),
        .data$Date + lubridate::hours(2)
      )
    ) %>%
    SortDf(.data$SubjectID, .data$Date) %>%
    group_by(.data$SubjectID) %>%
    mutate(
      "Numerator" = cumsum(ifelse(.data$EventType == "Numerator", 1, 0)),
      "Denominator" = cumsum(ifelse(.data$EventType == "Denominator", 1, 0))
    ) %>%
    group_by(.data$GroupID, .data$GroupLevel, .data$SubjectID, .data$Denominator) %>%
    summarize(
      MaxNumerator = max(.data$Numerator),
      MinNumerator = min(.data$Numerator)
    ) %>%
    group_by(.data$GroupID, .data$GroupLevel, .data$SubjectID) %>%
    mutate(
      # assign Numerator after last Denominator to last Denominator
      Numerator = ifelse(.data$Denominator == max(.data$Denominator), .data$MaxNumerator, .data$MinNumerator)
    ) %>%
    ungroup() %>%
    filter(.data$Denominator > 0) %>%
    select(c("SubjectID", "GroupID", "GroupLevel", "Numerator", "Denominator"))

  return(dfCumCount)
}

#' Handle all dfSubjects operations, only if GroupID is not yet in Numerator or Denominator frames.
#' Will also filter all events with no GroupID
#' @keywords internal
AddGroupCol <- function(df, dfSubjects, strSubjectCol, strGroupCol, strGroupLevel) {
  # if `strGroupLevel` is null, use `strGroupCol`
  if (is.null(strGroupLevel)) {
    strGroupLevel <- strGroupCol
  }

  if (!strGroupCol %in% colnames(df)) {
    stopifnot(!any(duplicated(dfSubjects[[strSubjectCol]])))

    # Rename SubjectID in dfSubjects
    dfSubjects <- dfSubjects %>%
      rename(
        "SubjectID" = !!strSubjectCol
      )

    df <- df %>%
      inner_join(
        dfSubjects %>%
          select(all_of(c("SubjectID", strGroupCol))),
        by = "SubjectID"
      )
  }

  df <- df %>%
    rename(
      "GroupID" = {{ strGroupCol }}
    ) %>%
    mutate(
      GroupLevel = strGroupLevel
    ) %>%
    filter(!is.na(.data$GroupID))

  return(df)
}

#' @keywords internal
AssignOrphans <- function(dfNumerator, dfDenominator) {
  if (!AnyNA(dfNumerator, "SubjectID")) {
    return(dfNumerator)
  }

  cols <- intersect(colnames(dfNumerator), colnames(dfDenominator))

  dfEventMinMax <- union_all(
    select(dfNumerator, all_of(cols)),
    select(dfDenominator, all_of(cols))
  ) %>%
    group_by(.data$GroupID, .data$SubjectID) %>%
    summarize(
      "MinDate" = min(.data$Date, na.rm = TRUE),
      "MaxDate" = max(.data$Date, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # add 30 days tolerance to MaxDate
    mutate(
      "MaxDate" = .data$MaxDate + lubridate::days(30)
    ) %>%
    select("SubjectID", "GroupID", "MinDate", "MaxDate")

  dfOrphans <- dfNumerator %>%
    filter(is.na(.data$SubjectID)) %>%
    select(-"SubjectID")

  dfOrphansAssigned <- dfOrphans %>%
    # join in all subjects from group to event and filter only eligible subjects
    left_join(dfEventMinMax, by = "GroupID", relationship = "many-to-many") %>%
    filter(between(.data$Date, .data$MinDate, .data$MaxDate)) %>%
    # select a random patient for event
    mutate(rwn = runif(n())) %>%
    SortDf(.data$rwn) %>%
    select(-"rwn") %>%
    group_by(.data$EventID) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(c("EventID", SubjectID_New = "SubjectID"))


  dfNumerator <- dfNumerator %>%
    left_join(dfOrphansAssigned, by = "EventID") %>%
    mutate(
      "SubjectID" = coalesce(.data$SubjectID, .data$SubjectID_New)
    )

  return(dfNumerator)
}

#' @keywords internal
AnyNA <- function(df, col) {
  if (inherits(df, "data.frame")) {
    return(any(is.na(df[[col]])))
  } else if (inherits(df, "tbl_lazy")) {
    return(GetTblNA(df, col) > 0)
  }
}

#' @keywords internal
CheckNotAllNA <- function(df, col) {
  if (inherits(df, "data.frame")) {
    stopifnot(any(!is.na(df[[col]])))
  } else if (inherits(df, "tbl_lazy")) {
    na_ratio <- GetTblNA(df, col)
    stopifnot(na_ratio < 1)
  }
}

#' @keywords internal
CheckDataType <- function(df, col, fun) {
  if (inherits(df, "data.frame")) {
    stopifnot(fun(df[[col]]))
  } else if (inherits(df, "tbl_lazy")) {
    df %>%
      head(5) %>%
      pull(.data[[col]]) %>%
      fun() %>%
      stopifnot()
  }
}

#' @keywords internal
GetTblNA <- function(tbl_lazy, col) {
  tbl_lazy %>%
    summarize(
      na_ratio = sum(ifelse(is.na(.data[[col]]), 1, 0), na.rm = TRUE) / n()
    ) %>%
    pull(.data$na_ratio)
}

#' @keywords internal
SortDf <- function(data, ...) {
  if (inherits(data, "data.frame")) {
    data <- data %>%
      dplyr::arrange(...)
  } else if (inherits(data, "tbl_lazy")) {
    data <- data %>%
      dbplyr::window_order(...)
  }

  return(data)
}

#' @keywords internal
CheckDf <- function(x) {
  # Check if x is a data.frame or tbl_lazy
  if (!inherits(x, "data.frame") && !inherits(x, "tbl_lazy")) {
    # Get the name of the variable in the parent environment
    var_name <- deparse(substitute(x))
    # Throw an error with a message that includes the variable name
    stop(sprintf("%s is not a data.frame or tbl_lazy.", var_name), call. = FALSE)
  }
  TRUE
}
