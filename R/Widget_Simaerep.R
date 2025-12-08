#' Simaerep Widget
#'
#' A widget that creates a simaerep visualisation of group-level metric results.
#' It plots the mean cumulative numerator count per denominator in the left panel and
#' highlights groups based on the over and under-reporting probability calculated
#' by the simaerep bootstrap algorithm. Flagged groups are shown in the right panel
#' including the total numerator counts per single patient.
#'
#' @inheritParams Visualize_Simaerep
#' @inheritParams gsm.kri::Widget_ScatterPlot
#' @return An htmlwidget object that can be rendered in R Markdown reports or Shiny apps.
#' @export
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
#'  Widget_Simaerep(dfInput, dfFlagged)
#'
#' @seealso
#' \code{\link{Widget_SimaerepOutput}} for use in Shiny apps
#' \code{\link{renderWidget_Simaerep}} for use in Shiny apps
#'
#' @export
Widget_Simaerep <- function(
  dfInput,
  dfFlagged,
  dfGroups = NULL,
  lMetric = NULL,
  strStudyId = "StudyID",
  strScoreCol = "Score",
  vColors = NULL,
  bAddGroupSelect = TRUE,
  strShinyGroupSelectID = "GroupID",
  strOutputLabel = paste0(
    fontawesome::fa("chart-line", fill = "#337ab7"),
    "  Simaerep"
  ),
  bDebug = FALSE,
  ...
) {

  lsData <- prepare_visualization_data(
    dfInput = dfInput,
    dfFlagged = dfFlagged,
    strScoreCol = strScoreCol,
    nSiteMax = NULL,
    vColors = vColors
  )

  if (is.data.frame(lMetric) && nrow(lMetric) == 1) {
    # convert to named vector to named list as the first item of a list
    lMetric <- list(as.list(unlist(lMetric)))
  }

  is_metric_valid <-  (is.list(lMetric) && ! inherits(lMetric, "data.frame")) | is.null(lMetric)

  stopifnot("lMetric must be a list" = is_metric_valid)

  # Prepare configuration object
  Config <- list(
    showGroupSelector = bAddGroupSelect,
    groupLabelKey = strShinyGroupSelectID,
    strStudyId = strStudyId,
    strScoreCol = strScoreCol,
    dfGroups = if (is.data.frame(dfGroups)) dfGroups else data.frame(),
    lMetric = lMetric,
    ...
  )

  # Create htmlwidget
  lWidget <- htmlwidgets::createWidget(
    name = 'Widget_Simaerep',
    x = list(
      data = lsData,
      config = Config
    ),
    package = 'gsm.simaerep',
  )

  base::attr(lWidget, "output_label") <- strOutputLabel

  if (bDebug) {
    viewer <- getOption("viewer")
    options(viewer = NULL)
    print(lWidget)
    options(viewer = viewer)
  }
  return(lWidget)
}

#' Shiny bindings for Widget_Simaerep
#'
#' @description
#'
#' Output and render functions for using Widget_Simaerep within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a Widget_ScatterPlot
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name Widget_Simaerep-shiny
#'
#' @export
Widget_SimaerepOutput <- function(
    outputId,
    width = "100%",
    height = "400px"
) {
  htmlwidgets::shinyWidgetOutput(
    outputId,
    "Widget_Simaerep",
    width,
    height,
    package = "gsm.simaerep"
  )
}

#' @rdname Widget_Simaerep-shiny
#' @export
renderWidget_Simaerep <- function(
    expr,
    env = parent.frame(),
    quoted = FALSE
) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(
    expr,
    Widget_SimaerepOutput,
    env,
    quoted = TRUE
  )
}

