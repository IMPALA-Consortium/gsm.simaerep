#' Scatter Plot Widget
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' A widget that generates a scatter plot of group-level metric results, plotting the denominator
#' on the x-axis and the numerator on the y-axis.
#'
#' @inheritParams gsm.kri::Widget_ScatterPlot
#' @param bAddGroupSelect `logical` Add a dropdown to highlight sites? Default: `TRUE`.
#' @param strShinyGroupSelectID `character` Element ID of group select in Shiny context. Default: `'GroupID'`.
#'
#' @examples
#' lMetric <- system.file('workflow', '2_metrics', 'kri0001.yaml', package = 'gsm.simaerep') %>%
#'     yaml::read_yaml() %>%
#'     .[[ 'meta' ]]
#' lMetric$MetricID <- glue::glue('{lMetric$Type}_{lMetric$ID}')
#' 
#' # {clindata} Example for cumulative AE per Visit Count
#' dfInput <- Input_CumCount(
#'     dfSubjects = clindata::rawplus_dm,
#'     dfNumerator = clindata::rawplus_ae,
#'     dfDenominator = clindata::rawplus_visdt %>%
#'         dplyr::mutate(visit_dt = lubridate::ymd(visit_dt)),
#'     strSubjectCol = "subjid",
#'     strGroupCol = "siteid",
#'     strGroupLevel = "Site",
#'     strNumeratorDateCol = "aest_dt",
#'     strDenominatorDateCol = "visit_dt"
#' )
#'
#' dfResults <- dfInput %>%
#'     Analyze_Simaerep() %>%
#'     Flag_Simaerep(
#'         vThreshold = gsm.core::ParseThreshold(lMetric$Threshold)
#'     ) %>%
#'     dplyr::mutate(
#'         MetricID = lMetric$MetricID
#'     )
#'
#' Widget_ScatterPlot(
#'     dfResults = dfResults,
#'     lMetric = lMetric,
#'     resultTooltipKeys = list(
#'         ExpectedNumerator = 'Expected Numerator',
#'         Numerator = lMetric$Numerator,
#'         Denominator = lMetric$Denominator,
#'         Metric = lMetric$Metric,
#'         Score = lMetric$Score
#'     )
#' )
#'
#' @export

Widget_ScatterPlot <- function(
    dfResults,
    lMetric = NULL,
    dfGroups = NULL,
    dfBounds = NULL,
    bAddGroupSelect = TRUE,
    strShinyGroupSelectID = "GroupID",
    lAdditionalSettings = list(),
    bDebug = FALSE,
    ...
) {
  gsm.core::stop_if(cnd = !is.data.frame(dfResults), "dfResults is not a data.frame")
  gsm.core::stop_if(cnd = !(is.null(lMetric) || (is.list(lMetric) && !is.data.frame(lMetric))), "lMetric must be a list, but not a data.frame")
  gsm.core::stop_if(cnd = !(is.null(dfGroups) || is.data.frame(dfGroups)), "dfGroups is not a data.frame")
  gsm.core::stop_if(cnd = !(is.null(dfBounds) || is.data.frame(dfBounds)), "dfBounds is not a data.frame")
  gsm.core::stop_if(cnd = !is.logical(bAddGroupSelect), "bAddGroupSelect is not a logical")
  gsm.core::stop_if(cnd = !is.character(strShinyGroupSelectID), "strShinyGroupSelectID is not a character")
  gsm.core::stop_if(cnd = !is.logical(bDebug), "bDebug is not a logical")

  # define widget inputs
  input <- c(
      list(
        dfResults = dfResults,
        lMetric = c(
            lMetric,
            list(...) # additional chart configuration
        ),
        dfGroups = dfGroups,
        dfBounds = dfBounds,
        bAddGroupSelect = bAddGroupSelect,
        strShinyGroupSelectID = strShinyGroupSelectID,
        strFootnote = ifelse(!is.null(dfGroups), "Point size is relative to the number of enrolled participants.", ""),
        bDebug = bDebug
      )
    )

  # create widget
  widget <- htmlwidgets::createWidget(
    name = "Widget_ScatterPlotSimaerep",
    purrr::map(
      input,
      ~ jsonlite::toJSON(
        .x,
        null = "null",
        na = "string",
        auto_unbox = TRUE
      )
    ),
    package = "gsm.simaerep"
  )

  if (bDebug) {
    viewer <- getOption("viewer")
    options(viewer = NULL)
    print(widget)
    options(viewer = viewer)
  }

  return(widget)
}

#' Shiny bindings for Widget_ScatterPlot
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Output and render functions for using Widget_ScatterPlot within Shiny
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
#' @name Widget_ScatterPlot-shiny
#'
#' @export
Widget_ScatterPlotOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "Widget_ScatterPlot", width, height, package = "gsm.kri")
}

#' @rdname Widget_ScatterPlot-shiny
#' @export
renderWidget_ScatterPlot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, Widget_ScatterPlotOutput, env, quoted = TRUE)
}
