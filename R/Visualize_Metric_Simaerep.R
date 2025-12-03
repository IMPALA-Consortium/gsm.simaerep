#' Visualize_Metric Function
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' The function creates all available charts for a metric using the data provided
#'
#' @inheritParams gsm.kri::Visualize_Metric
#'
#' @return A list containing the following charts:
#' - scatterPlot: A scatter plot using JavaScript.
#' - barChart: A bar chart using JavaScript with metric on the y-axis.
#' - timeSeries: A time series chart using JavaScript with score on the y-axis.
#' - metricTable: A table containing all
#'
#' @examples
#' lCharts <- Visualize_Metric_Simaerep(
#'   dfResults = gsm.core::reportingResults,
#'   dfBounds = gsm.core::reportingBounds,
#'   dfGroups = gsm.core::reportingGroups,
#'   dfMetrics = gsm.core::reportingMetrics,
#'   strMetricID = "Analysis_kri0001"
#' )
#'
#' @export

Visualize_Metric_Simaerep <- function(
  dfResults,
  dfInput,
  dfFlagged = NULL,
  dfMetrics = NULL,
  dfGroups = NULL,
  dfBounds = NULL,
  strMetricID = NULL,
  strSnapshotDate = NULL,
  bDebug = FALSE,
  vColors = c("0" = "#9ED782", "1" = "#FEAA01", "2" = "#FF5858", "-1" = "#FEAA01", "-2" = "#FF5858", "NA" = "#a9a9a9"),
  ...
) {
  # Check for multiple snapshots --------------------------------------------
  # if SnapshotDate is missing set it to today for all records
  if (!"SnapshotDate" %in% colnames(dfResults)) {
    dfResults$SnapshotDate <- as.Date(Sys.Date())
  }

  if (!"SnapshotDate" %in% colnames(dfBounds) & !is.null(dfBounds)) {
    dfBounds$SnapshotDate <- as.Date(Sys.Date())
  }

  # get number of snapshots
  number_of_snapshots <- length(unique(dfResults$SnapshotDate))

  # use most recent snapshot date if strSnapshotDate is missing
  if (is.null(strSnapshotDate)) {
    strSnapshotDate <- max(dfResults$SnapshotDate)
  }

  # Filter to selected MetricID ----------------------------------------------
  if (!is.null(strMetricID)) {
    if (!(strMetricID %in% unique(dfResults$MetricID))) {
      gsm.core::LogMessage(
        level = "info",
        message = "MetricID not found in dfResults. No charts will be generated.",
        cli_detail = "alert_info"
      )
      return(NULL)
    } else {
      dfResults <- dfResults %>% filter(.data$MetricID == strMetricID)
    }
  }
  if (!is.null(strMetricID)) {
    if (!(strMetricID %in% unique(dfBounds$MetricID))) {
      gsm.core::LogMessage(
        level = "info",
        message = "MetricID not found in dfBounds. Please double check input data if intentional.",
        cli_detail = "inform"
      )
      dfBounds <- NULL
    } else {
      dfBounds <- dfBounds %>% filter(.data$MetricID == strMetricID)
    }
  }

  if (!is.null(strMetricID)) {
    if (!(strMetricID %in% unique(dfMetrics$MetricID))) {
      gsm.core::LogMessage(
        level = "info",
        message = "MetricID not found in dfMetrics. Please double check input data if intentional.",
        cli_detail = "inform"
      )
      dfMetrics <- NULL
    } else {
      dfMetrics <- dfMetrics %>% filter(.data$MetricID == strMetricID)
    }
  }

  if (!is.null(strMetricID)) {
    if (!(strMetricID %in% unique(dfInput$MetricID))) {
      gsm.core::LogMessage(
        level = "info",
        message = "MetricID not found in dfInput Please double check input data if intentional.",
        cli_detail = "inform"
      )
      dfInput <- NULL
    } else {
      dfInput <- dfInput %>% filter(.data$MetricID == strMetricID)
    }
  }

  if (!is.null(strMetricID)) {
    if (!(strMetricID %in% unique(dfFlagged$MetricID))) {
      gsm.core::LogMessage(
        level = "info",
        message = "MetricID not found in dfFlagged Please double check input data if intentional.",
        cli_detail = "inform"
      )
      dfFlagged <- NULL
    } else {
      dfFlagged <- dfFlagged %>% filter(.data$MetricID == strMetricID)
    }
  }

  if (
    length(unique(dfResults$MetricID)) > 1 |
      length(unique(dfBounds$MetricID)) > 1 |
      length(unique(dfMetrics$MetricID)) > 1 |
      length(unique(dfInput$MetricID)) > 1 |
      length(unique(dfFlagged$MetricID)) > 1
  ) {
    gsm.core::LogMessage(
      level = "fatal",
      message = "Multiple MetricIDs found in dfResults, dfBounds, dfMetrics, dfFlagged or dfInput. Specify `MetricID` to subset. No charts will be generated."
    )
    return(NULL)
  }

  # Prep chart inputs ---------------------------------------------------------
  if (is.null(dfMetrics)) {
    lMetric <- NULL
    vThreshold <- NULL
  } else {
    lMetric <- as.list(dfMetrics)
    vThreshold <- gsm.core::ParseThreshold(lMetric$Threshold, bSort = FALSE)
  }

  # Cross-sectional Charts using most recent snapshot ------------------------
  lCharts <- list()

  dfResults_latest <- gsm.kri::FilterByLatestSnapshotDate(dfResults, strSnapshotDate)
  dfInput_latest <- gsm.kri::FilterByLatestSnapshotDate(dfInput, strSnapshotDate)

  if (is.null(dfBounds)) {
    dfBounds_latest <- NULL
  } else {
    dfBounds_latest <- gsm.kri::FilterByLatestSnapshotDate(dfBounds, strSnapshotDate)
  }

  if (is.null(dfFlagged)) {
    dfFlagged_latest <- NULL
  } else {
    dfFlagged_latest <- gsm.kri::FilterByLatestSnapshotDate(dfFlagged, strSnapshotDate)
  }

  if (nrow(dfResults_latest) == 0) {
    gsm.core::LogMessage(
      level = "warn",
      message = "No data found for specified snapshot date: {strSnapshotDate}. No charts will be generated."
    )
  } else {

    lCharts$simaerepChart <- do.call(
      "Widget_Simaerep",
      list(
        dfInput = dfInput_latest,
        dfFlagged = dfFlagged_latest,
        dfGroups = dfGroups,
        lMetric = lMetric,
        vColors = vColors,
        ...
      )
    )

    lCharts$scatterPlot <- gsm.kri::Widget_ScatterPlot(
        dfResults = dfResults_latest,
        lMetric = lMetric,
        dfGroups = dfGroups,
        dfBounds = dfBounds_latest,
        bDebug = bDebug,
        ...
    )

    lCharts$barChart <- gsm.kri::Widget_BarChart(
        dfResults = dfResults_latest,
        lMetric = lMetric,
        dfGroups = dfGroups,
        vThreshold = vThreshold,
        bDebug = bDebug,
        ...
      )

    if (!is.null(lMetric)) {
      lCharts$metricTable <- gsm.kri::Report_MetricTable(
        dfResults = dfResults_latest,
        dfGroups = dfGroups,
        strGroupLevel = lMetric$GroupLevel
      )
    } else {
      lCharts$metricTable <- gsm.kri::Report_MetricTable(dfResults_latest)
    }

  }
  # Continuous Charts -------------------------------------------------------
  if (number_of_snapshots <= 1) {
    gsm.core::LogMessage(
      level = "info",
      message = "Only one snapshot found. Time series charts will not be generated.",
      cli_detail = "alert_info"
    )
  } else {
    lCharts$timeSeries <- do.call(
      gsm.kri::Widget_TimeSeries,
      list(
        dfResults = dfResults,
        lMetric = lMetric,
        dfGroups = dfGroups,
        vThreshold = vThreshold,
        bDebug = bDebug,
        ...
      )
    )
  }

  return(lCharts)
}
