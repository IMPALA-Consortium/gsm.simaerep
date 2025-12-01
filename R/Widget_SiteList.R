#' Site List Widget
#'
#' Create an interactive site list visualization with cross-widget selection support
#' for gsm.kri reports.
#'
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
#' @param width Widget width. Default is \code{NULL} for automatic sizing.
#' @param height Widget height. Default is \code{NULL} for automatic sizing.
#' @param elementId Optional element ID for the widget container. Useful for Shiny apps.
#'
#' @return An htmlwidget object that can be rendered in R Markdown reports or Shiny apps.
#'
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
Widget_SiteList <- function(
  data,
  selectedGroupIDs = "None",
  maxHeight = "600px",
  showGroupSelector = TRUE,
  groupLabelKey = "GroupID",
  width = NULL,
  height = NULL,
  elementId = NULL
) {

  # Validate required inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!"GroupID" %in% names(data)) {
    stop("data must contain a 'GroupID' column")
  }

  if (!groupLabelKey %in% names(data)) {
    stop(sprintf("groupLabelKey '%s' not found in data columns", groupLabelKey))
  }

  # Convert data to list format for JSON serialization
  data_list <- lapply(seq_len(nrow(data)), function(i) {
    as.list(data[i, ])
  })

  # Prepare configuration object
  config <- list(
    selectedGroupIDs = selectedGroupIDs,
    maxHeight = maxHeight,
    showGroupSelector = showGroupSelector,
    groupLabelKey = groupLabelKey
  )

  # Create htmlwidget
  htmlwidgets::createWidget(
    name = 'Widget_SiteList',
    x = list(
      data = data_list,
      config = config
    ),
    width = width,
    height = height,
    package = 'gsm.simaerep',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      padding = 0,
      browser.fill = TRUE,
      viewer.fill = TRUE,
      knitr.figure = FALSE
    )
  )
}

