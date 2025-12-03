#' Site List Widget
#'
#' Create an interactive site list visualization with cross-widget selection support
#' for gsm.kri reports.
#'
#' @inheritParams Visualize_Simaerep
#' @param data Data frame containing site information. Must include a \code{GroupID} column.
#'   Additional columns like \code{InvestigatorLastName}, \code{Country}, \code{Status} are
#'   optional but recommended for better site identification.
#' @param selectedGroupIDs Character string specifying initially selected site ID.
#'   Default is \code{"None"} (no selection).
#' @param maxHeight Character string specifying maximum height for the scrollable list.
#'   Default is \code{"600px"}. Use CSS units (px, vh, etc.).
#' @param showGroupSelector Logical indicating whether to show the group selector dropdown.
#'   Default is \code{TRUE}.
#' @param groupLabelKey Character string specifying which column to use for site labels
#'   in the dropdown. Default is \code{"GroupID"}.
#' @param dfGroups Optional data frame containing group metadata in Param/Value format
#'   (columns: GroupID, Param, Value, GroupLevel). Used to display investigator names,
#'   countries, status, and other metadata in tooltips. Default is \code{NULL}.
#' @param width Widget width. Default is \code{NULL} for automatic sizing.
#' @param height Widget height. Default is \code{NULL} for automatic sizing.
#' @param elementId Optional element ID for the widget container. Useful for Shiny apps.
#'
#' @return An htmlwidget object that can be rendered in R Markdown reports or Shiny apps.
#' @export
#' @examples
#' \dontrun{
#' library(gsm.simaerep)
#'
#' # Prepare site data
#' dfSites <- data.frame(
#'   GroupID = c("S0001", "S0002", "S0003"),
#'   InvestigatorLastName = c("Smith", "Jones", "Brown"),
#'   Country = c("USA", "UK", "Canada"),
#'   Status = c("Active", "Active", "Inactive"),
#'   SubjectCount = c(25, 30, 15)
#' )
#'
#' # Create site list widget
#' Widget_Simaerep(
#'   data = dfSites,
#'   selectedGroupIDs = "None",
#'   maxHeight = "500px"
#' )
#'
#' # With custom label
#' Widget_Simaerep(
#'   data = dfSites,
#'   groupLabelKey = "InvestigatorLastName"
#' )
#'
#' # In an R Markdown report
#' Widget_Simaerep(
#'   data = dfSites,
#'   selectedGroupIDs = "S0001",
#'   maxHeight = "400px",
#'   showGroupSelector = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link{siteListOutput}} for use in Shiny apps
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
  selectedGroupIDs = "None",
  maxHeight = "600px",
  showGroupSelector = TRUE,
  groupLabelKey = "GroupID",
  width = NULL,
  height = NULL,
  elementId = NULL,
  strOutputLabel = paste0(
    fontawesome::fa("chart-line", fill = "#337ab7"),
    "  Simaerep"
  ),
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
    selectedGroupIDs = selectedGroupIDs,
    maxHeight = maxHeight,
    showGroupSelector = showGroupSelector,
    groupLabelKey = groupLabelKey,
    strStudyId = strStudyId,
    strScoreCol = strScoreCol,
    dfGroups = if (is.data.frame(dfGroups)) dfGroups else data.frame(),
    lMetric = lMetric
  )

  # jsData <- purrr::map(
  #   lsData,
  #   ~ jsonlite::toJSON(.x, null = "null", na = "string", auto_unbox = TRUE)
  # )

  # jsConfig <- purrr::map(
  #   Config,
  #   ~ jsonlite::toJSON(.x, null = "null", na = "string", auto_unbox = TRUE)
  # )

  jsData <- lsData
  jsConfig <- Config

  # Create htmlwidget
  lWidget <- htmlwidgets::createWidget(
    name = 'Widget_Simaerep',
    x = list(
      data = jsData,
      config = jsConfig
    ),
    # width = width,
    # height = height,
    package = 'gsm.simaerep',
    elementId = elementId
    # sizingPolicy = htmlwidgets::sizingPolicy(
    #   defaultWidth = "100%",
    #   defaultHeight = 400,
    #   padding = 10,
    #   viewer.padding = 10,
    #   browser.fill = TRUE,
    #   viewer.fill = TRUE,
    #   knitr.figure = TRUE,
    #   knitr.defaultWidth = "100%",
    #   knitr.defaultHeight = 400
    # )
  )

  base::attr(lWidget, "output_label") <- strOutputLabel

  return(lWidget)
}

