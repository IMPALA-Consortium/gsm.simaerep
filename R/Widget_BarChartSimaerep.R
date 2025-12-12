#' Simawerep Bar Chart Widget
#'
#' @description
#'
#' modified [gsm.kri::Widget_BarChart()] to include ExpectedNumerator as the default outcome.
#'
#' @inheritParams Widget_Simaerep
#' @inheritParams gsm.kri::Widget_BarChart
#' @param vThreshold `numeric` Threshold values.
#' @param vResultTooltipKeys `character` Result tooltip keys. 
#'   Default: c("ExpectedNumerator", "Score", "Metric", "Numerator", "Denominator").
#' @param strOutcome `character` Outcome variable. Default: 'ExpectedNumerator'.
#' @examples
#' ## Filter data to one metric and snapshot
#' reportingResults_filter <- gsm.core::reportingResults %>%
#'   dplyr::filter(MetricID == "Analysis_kri0001" & SnapshotDate == max(SnapshotDate))
#'
#' reportingMetrics_filter <- gsm.core::reportingMetrics %>%
#'   dplyr::filter(MetricID == "Analysis_kri0001") %>%
#'   as.list()
#'
#' ## Make chart
#' Widget_BarChartSimaerep(
#'   dfResults = reportingResults_filter,
#'   dfGroups = gsm.core::reportingGroups,
#'   lMetric = reportingMetrics_filter,
#'   vThreshold = reportingMetrics_filter$Threshold
#' )
#'
#' @export

Widget_BarChartSimaerep <- function(
  dfResults,
  lMetric = NULL,
  dfGroups = NULL,
  vThreshold = NULL,
  strOutcome = "ExpectedNumerator",
  bAddGroupSelect = TRUE,
  strShinyGroupSelectID = "GroupID",
  strOutputLabel = paste0(
    fontawesome::fa("chart-simple", fill = "#337ab7"),
    "  Bar Chart"
  ),
  vResultTooltipKeys = c(
    "ExpectedNumerator",
    "Score",
    "Metric",
    "Numerator",
    "Denominator"
  ),
  ...
) {
  gsm.core::stop_if(cnd = !is.data.frame(dfResults), message = "dfResults is not a data.frame")
  gsm.core::stop_if(
    cnd = !(is.null(lMetric) || (is.list(lMetric) || (is.data.frame(lMetric) && nrow(lMetric) == 1))),
    message = "lMetric must be a list, but not a data.frame"
  )
  gsm.core::stop_if(cnd = !(is.null(dfGroups) || is.data.frame(dfGroups)), message = "dfGroups is not a data.frame")
  gsm.core::stop_if(cnd = !length(strOutcome) == 1, message = "strOutcome must be length 1")
  gsm.core::stop_if(cnd = !is.character(strOutcome), message = "strOutcome is not a character")
  gsm.core::stop_if(cnd = !is.logical(bAddGroupSelect), message = "bAddGroupSelect is not a logical")
  gsm.core::stop_if(cnd = !is.character(strShinyGroupSelectID), message = "strShinyGroupSelectID is not a character")

  if (is.data.frame(lMetric) && nrow(lMetric) == 1) {
    # convert to named vector to named list as the first item of a list
    lMetric <- list(as.list(unlist(lMetric)))
  }

  # Parse `vThreshold` from comma-delimited character string to numeric vector.
  if (!is.null(vThreshold)) {
    if (is.character(vThreshold)) {
      vThreshold <- strsplit(vThreshold, ",")[[1]] %>% as.numeric()
    }
  }

  # Disable threshold if outcome is not 'Score'.
  if (strOutcome != "Score") {
    vThreshold <- NULL
  }

  # define widget inputs
  lChartConfig <- gsm.kri::MakeChartConfig(
      lMetric = lMetric,
      strChartFunction = "Widget_BarChart",
      y = strOutcome,
      ...
  )

  # define widget inputs
  lInput <- list(
    dfResults = dfResults,
    lMetric = lMetric,
    dfGroups = dfGroups,
    vThreshold = vThreshold,
    lChartConfig = lChartConfig,
    strOutcome = strOutcome,
    bAddGroupSelect = bAddGroupSelect,
    strShinyGroupSelectID = strShinyGroupSelectID
  )

  # create widget
  lWidget <- htmlwidgets::createWidget(
    name = "Widget_BarChart",
    purrr::map(
      lInput,
      ~ jsonlite::toJSON(
        .x,
        null = "null",
        na = "string",
        auto_unbox = TRUE
      )
    ),
    package = "gsm.simaerep"
  )

  base::attr(lWidget, "output_label") <- strOutputLabel

  return(lWidget)
}

# nocov start

#' Shiny bindings for Widget_BarChart
#'
#' @description
#'
#' Output and render functions for using Widget_BarChart within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a Widget_BarChart
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name Widget_BarChartSimaerep-shiny
#'
#' @export
Widget_BarChartSimaerepOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "Widget_BarChart", width, height, package = "gsm.kri")
}

#' @rdname Widget_BarChartSimaerep-shiny
#' @export
renderWidget_BarChartSimaerep <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, Widget_BarChartSimaerepOutput, env, quoted = TRUE)
}

# nocov end
