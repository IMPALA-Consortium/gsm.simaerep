
#' ChartsMetricSimaerep Function
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' The function creates all metric charts for a simaerep metric using the data provided
#'
#' @inheritParams gsm.kri::ChartsMetricDefault
#' @param lCharts `list`, Default: list()
#'
#' @return A list containing the following charts:
#' - scatterPlot: A scatter plot using JavaScript.
#' - barChart: A bar chart using JavaScript with metric on the y-axis.
#' - timeSeries: A time series chart using JavaScript with score on the y-axis.
#' @examples
#' lCharts <- list()
#' strMetricID = "Analysis_kri0001"
#' 
#' dfResults_latest <-  gsm.core::reportingResults %>%
#'  dplyr::filter(MetricID == strMetricID) %>%
#'  FilterByLatestSnapshotDate()
#' 
#' dfBounds_latest <- gsm.core::reportingBounds %>%
#'  dplyr::filter(MetricID == strMetricID) %>%
#'  FilterByLatestSnapshotDate()
#' 
#' dfGroups <- gsm.core::reportingGroups
#' 
#' lMetric <- gsm.core::reportingMetrics %>%
#'  dplyr::filter(MetricID == strMetricID) %>%
#'  as.list()
#' 
#' vThreshold = gsm.core::ParseThreshold(lMetric$Threshold, bSort = FALSE)
#' 
#' lCharts <- lCharts %>%
#'   ChartsMetricDefault(
#'     dfResults= dfResults_latest,
#'     lMetric = lMetric,
#'     dfGroups = dfGroups,
#'     dfBounds = dfBounds_latest,
#'     vThreshold = vThreshold,
#'     bDebug = FALSE
#'   )
#' 
#' lCharts
#' @export
ChartsMetricSimaerep <- function(
    lCharts,
    dfResults,
    lMetric,
    dfGroups,
    dfBounds,
    vThreshold,
    bDebug
) {

  lCharts <- lCharts %>%
    gsm.kri::ChartsMetricDefault(
      dfResults = dfResults,
      lMetric = lMetric,
      dfGroups = dfGroups,
      dfBounds = dfBounds,
      vThreshold = vThreshold,
      bDebug = bDebug
    )
                              
  lCharts$scatterPlot <- Widget_ScatterPlot(
      dfResults = dfResults,
      lMetric = lMetric,
      dfGroups = dfGroups,
      dfBounds = dfBounds,
      bDebug = bDebug
    )

  scatterPlotName <- paste0(fontawesome::fa("shoe-prints", fill = "#F46526"), "  {simaerep} Scatter Plot")
  attr(lCharts$scatterPlot, "chart_name") <- scatterPlotName

  return(lCharts)

}

