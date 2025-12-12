#' Visualize_Metric Function
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' The function creates all available charts for a metric using the data provided
#'
#' @inheritParams gsm.kri::Visualize_Metric
#' @inheritParams Widget_Simaerep
#' @inheritParams Widget_BarChartSimaerep
#' @return A list containing the following charts:
#' - simaerep: A simaerep plot using JavaScript.
#' - scatterPlot: A scatter plot using JavaScript.
#' - barChart: A bar chart using JavaScript with metric on the y-axis.
#' - timeSeries: A time series chart using JavaScript with score on the y-axis.
#' - metricTable: A table containing all
#'
#' @examples
#'
#'  dfInput <- Input_CumCount(
#'    dfSubjects = clindata::rawplus_dm,
#'    dfNumerator = clindata::rawplus_ae,
#'    dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = lubridate::ymd(visit_dt)),
#'    strSubjectCol = "subjid",
#'    strGroupCol = "invid",
#'    strGroupLevel = "Site",
#'    strNumeratorDateCol = "aest_dt",
#'    strDenominatorDateCol = "visit_dt"
#'  )
#'
#'  dfAnalyzed <- Analyze_Simaerep(dfInput)
#'  dfFlagged <- Flag_Simaerep(dfAnalyzed, vThreshold = c(-0.99, -0.95, 0.95, 0.99))
#'
#' Visualize_Metric_Simaerep(
#'   dfResults = dfFlagged,
#'   dfInput = dfInput
#' )
#'
#' @export

Visualize_Metric_Simaerep <- function(
  dfResults,
  dfInput,
  dfMetrics = NULL,
  dfGroups = NULL,
  strMetricID = NULL,
  strSnapshotDate = NULL,
  bDebug = FALSE,
  vColors = c("0" = "#9ED782", "1" = "#FEAA01", "2" = "#FF5858", "-1" = "#FEAA01", "-2" = "#FF5858", "NA" = "#a9a9a9"),
  vResultTooltipKeys = c(
    "ExpectedNumerator",
    "Score",
    "Metric",
    "Numerator",
    "Denominator"
  ),
  ...
) {
  # Check for multiple snapshots --------------------------------------------
  # if SnapshotDate is missing set it to today for all records
  if (!"SnapshotDate" %in% colnames(dfResults)) {
    dfResults$SnapshotDate <- as.Date(Sys.Date())
  }

  if (!"SnapshotDate" %in% colnames(dfInput)) {
    dfInput$SnapshotDate <- as.Date(Sys.Date())
  }

  # get number of snapshots
  number_of_snapshots <- length(unique(dfResults$SnapshotDate))

  # use most recent snapshot date if strSnapshotDate is missing
  if (is.null(strSnapshotDate)) {
    strSnapshotDate <- max(dfResults$SnapshotDate)
  }

  # Filter to selected MetricID ----------------------------------------------

  if (!is.null(strMetricID) && "MetricID" %in% colnames(dfResults)) {
    if (!(strMetricID %in% unique(dfResults$MetricID))) {
      gsm.core::LogMessage(
        level = "info",
        message = "MetricID not found in dfResults. No charts will be generated.",
        cli_detail = "alert_info"
      )
      return(NULL)
    } else if("MetricID" %in% colnames(dfResults)){
      dfResults <- dfResults %>% filter(.data$MetricID == strMetricID)
    }
  }

  if (!is.null(strMetricID) && "MetricID" %in% colnames(dfInput)) {
    if (!(strMetricID %in% unique(dfInput$MetricID))) {
      gsm.core::LogMessage(
        level = "info",
        message = "MetricID not found in dfInput. No charts will be generated.",
        cli_detail = "alert_info"
      )
     return(NULL)
    } else if("MetricID" %in% colnames(dfInput) && "MetricID" %in% colnames(dfInput)){
      dfInput <- dfInput %>% filter(.data$MetricID == strMetricID)
    }
  }

  if (!is.null(strMetricID) && ! is.null(dfMetrics) && "MetricID" %in% colnames(dfMetrics)) {
    if (!(strMetricID %in% unique(dfMetrics$MetricID))) {
      gsm.core::LogMessage(
        level = "info",
        message = "MetricID not found in dfMetrics. Please double check input data if intentional.",
        cli_detail = "inform"
      )
      dfMetrics <- NULL
    } else if(! is.null(dfMetrics) && "MetricID" %in% colnames(dfMetrics)){
      dfMetrics <- dfMetrics %>% filter(.data$MetricID == strMetricID)
    }
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

  if (nrow(dfResults_latest) == 0) {
    gsm.core::LogMessage(
      level = "warn",
      message = "No data found for specified snapshot date: {strSnapshotDate}. No charts will be generated."
    )
    return(NULL)
  } else if (nrow(dfInput_latest) == 0) {
    gsm.core::LogMessage(
      level = "warn",
      message = "No data found for specified snapshot date: {strSnapshotDate}. No charts will be generated."
    )
    return(NULL)
  } else {

    lCharts$simaerepChart <- do.call(
      "Widget_Simaerep",
      list(
        dfInput = dfInput_latest,
        dfFlagged = dfResults_latest,
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
        bDebug = bDebug,
        vResultTooltipKeys = vResultTooltipKeys,
        ...
    )

    lCharts$barChart <- Widget_BarChartSimaerep(
        dfResults = dfResults_latest,
        lMetric = lMetric,
        dfGroups = dfGroups,
        vThreshold = vThreshold,
        bDebug = bDebug,
        vResultTooltipKeys = vResultTooltipKeys,
        ...
      )

    if (!is.null(lMetric)) {
      lCharts$metricTable <- gsm.kri::Report_MetricTable(
        dfResults = dfResults_latest,
        dfGroups = dfGroups,
        strGroupLevel = lMetric$GroupLevel
      )
    } else {
      dfResults_latest$MetricID <- NA
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
