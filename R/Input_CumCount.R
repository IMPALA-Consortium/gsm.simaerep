#' Cumulative Numerator Event Count per Denominator Event Count
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculate a subject level cumulative numerator event count per denominator event count.
#'
#' This function takes in a list of data frames including dfSUBJ, dfNumerator, and dfDenominator,
#' and calculates a subject level cumulative count.
#'
#' The data requirements for the function are as follows:
#'
#' - dfSubjects: A data frame with columns for SubjectID and any other relevant subject information
#' - dfNumerator: A data frame with a column for SubjectID and `strNumeratorCol` if `strNumeratorMethod` is "Sum"
#' - dfDenominator: A data frame with a column for SubjectID and `strDenominatorCol` if `strDenominatorMethod` is "Sum"
#'
#' All other columns are dropped from the output data frame. Note that if no values for a subject are
#' found in dfNumerator/dfDenominator numerator and denominator values are filled with 0 in the output data frame.
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
#' # Run for AE KRI
#' dfInput <- Input_Rate(
#'   dfSubjects = clindata::rawplus_dm,
#'   dfNumerator = clindata::rawplus_ae,
#'   dfDenominator = clindata::rawplus_visdt,
#'   strSubjectCol = "subjid",
#'   strGroupCol = "siteid",
#'   strGroupLevel = "Site",
#'   strNumeratorCol = ,
#'   strDenominatorCol,
#'   strNumeratorDateCol,
#'   strDenominatorDateCol
#' )
#'
#' @export
#' @keywords internal

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
  strOrphanedMethod = c("filter", "assign")
) {
  # Check if data frames are NULL
  if (is.null(dfSubjects)) {
    stop("dfSubjects must be provided")
  }
  if (is.null(dfDenominator)) {
    stop("dfDenominator, must be provided")
  }
  if (is.null(dfNumerator)) {
    stop("dfNumerator, must be provided")
  }

  # must be it
  strOrphanedMethod <- match.arg(strOrphanedMethod)

  # check that "strSubjectCol" is in all dfs and not all NULL
  stopifnot(
    strSubjectCol %in% colnames(dfSubjects),
    strSubjectCol %in% colnames(dfNumerator),
    strSubjectCol %in% colnames(dfDenominator),
    any(!is.na(dfSubjects[[strSubjectCol]])),
    any(!is.na(dfNumerator[[strSubjectCol]])),
    any(!is.na(dfDenominator[[strSubjectCol]]))
  )

  # check that "strGroupCol" is in dfSubjects
  stopifnot(strGroupCol %in% colnames(dfSubjects))

  # DateCol for Numerator and Denominator must be present in dfNumerator and dfDenominator
  # and have date type and must not be all NULL.
  stopifnot(
    strNumeratorDateCol %in% colnames(dfNumerator),
    strDenominatorDateCol %in% colnames(dfDenominator),
    lubridate::is.instant(dfNumerator[[strNumeratorDateCol]]),
    lubridate::is.instant(dfDenominator[[strDenominatorDateCol]]),
    any(!is.na(dfNumerator[[strNumeratorDateCol]])),
    any(!is.na(dfDenominator[[strDenominatorDateCol]])
    )
  )

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
      dfDenominator %>% distinct(.data$SubjectID, .data$GroupID, .data$GroupLevel,.data$EventID, .data$Date, .data$EventType)
    )

  dfCumCount <- dfEvents %>%
    mutate(
      # truncate Date to Date and add two hours to numerator date so numerator events are
      # placed after denominator events so that numerator events are always linked to same
      # day denominator events.
      Date = as.Date(.data$Date),
      Data = .data$Date + lubridate::hours(2)
    ) %>%
    arrange(.data$SubjectID, .data$Date) %>%
    group_by(.data$SubjectID) %>%
    mutate(
      "Numerator" = cumsum(.data$EventType == "Numerator"),
      "Denominator" = cumsum(.data$EventType == "Denominator")
    ) %>%
    group_by(.data$GroupID, .data$GroupLevel, .data$SubjectID, .data$Denominator) %>%
    summarize(
      MaxNumerator = max(.data$Numerator),
      MinNumerator = min(.data$Numerator)
    ) %>%
    group_by(.data$GroupID, .data$GroupLevel, .data$SubjectID) %>%
    mutate(
      # assign Numerator after last Denominator to last Denominator
      Numerator = ifelse(Denominator == max(Denominator), MaxNumerator, MinNumerator)
    ) %>%
    ungroup() %>%
    filter(.data$Denominator > 0) %>%
    select(c("SubjectID", "GroupID", "GroupLevel", "Numerator", "Denominator"))

  return(dfCumCount)
}

#' Handle all dfSubjects operations, only if GroupID is not yet in Numerator or Denominator frames.
#' Will also filter all events with no GroupID
#'@keywords internal
AddGroupCol <- function(df, dfSubjects, strSubjectCol, strGroupCol, strGroupLevel) {

  # if `strGroupLevel` is null, use `strGroupCol`
  if (is.null(strGroupLevel)) {
    strGroupLevel <- strGroupCol
  }
  
  if (! strGroupCol %in% colnames(df)) {

    stopifnot(! any(duplicated(dfSubjects[[strSubjectCol]])))

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
    filter(! is.na(.data$GroupID))

 return(df)

}

#'@keywords internal
AssignOrphans <- function(dfNumerator, dfDenominator) {

  if (! any(is.na(dfNumerator$SubjectID))) {
    LogMessage(
      level = "info",
      message = "No orphaned numerator events found"
    )

    return(dfNumerator)
  }

  dfEventMinMax <- bind_rows(dfNumerator, dfDenominator) %>%
    group_by(.data$GroupID, .data$SubjectID) %>%
    summarize(
      "MinDate" = min(.data$Date, na.rm = TRUE),
      "MaxDate" = max(.data$Date, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # add 30 days tolerance to MaxDate
    mutate(
      "MaxDate" = .data$MaxDate + 30
    ) %>%
    select("SubjectID", "GroupID", "MinDate", "MaxDate")

  dfOrphans <- dfNumerator %>%
    filter(is.na(.data$SubjectID)) %>%
    select(- "SubjectID")

  dfOrphansAssigned <- dfOrphans %>%
    # inner join with dfEventMinMax will filter all events
    # outside of of patient active period and cross join
    # all available patients
    inner_join(
      dfEventMinMax,
      by = join_by(
        "GroupID",
         between(x$Date, y$MinDate, y$MaxDate)
      )
    ) %>%
    # select a random patient for event
    arrange(runif(n())) %>%
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