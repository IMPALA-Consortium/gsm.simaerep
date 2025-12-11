#' Helper function to create charts for multiple metrics
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @inheritParams gsm.kri::MakeCharts
#' @param strVisualizeFun Character string specifying the visualization function to use
#' @param ... Additional chart configuration settings.
#'
#' @return A list of charts for each metric.
#'
#' @export

MakeCharts <- function(
  dfResults,
  dfMetrics,
  dfGroups,
  dfBounds,
  bDebug = FALSE,
  strVisualizeFun = "gsm.simaerep::Visualize_Metric_Simaerep",
  ...
) {
  strMetrics <- unique(dfMetrics$MetricID)

  lArgs <- list(
    dfResults = dfResults,
    dfMetrics = dfMetrics,
    dfGroups = dfGroups,
    dfBounds = dfBounds,
    bDebug = bDebug,
    ...
  )

  fun <- eval(parse(text = strVisualizeFun))

  lCharts <- strMetrics %>%
    purrr::map(~ {
      lArgs$strMetricID <- .x

      do.call(
        fun,
        lArgs
      )
    }) %>%
    stats::setNames(strMetrics)

  return(lCharts)
}
