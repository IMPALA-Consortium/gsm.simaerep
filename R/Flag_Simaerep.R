#' Flag_Simaerep
#'
#' #' @description
#' `r lifecycle::badge("experimental")`
#'
#' Wrapper for `gsm::Flag()`
#'
#' @seealso [gsm::Flag()]
#' @param ... parameters passed to [gsm::Flag()]
#' @inheritParams gsm::Flag
#'
#' @export

Flag_Simaerep <- function(dfAnalyzed, ...) {

  # we force the data.frame to be collected in case lazy table was passed
  dfAnalyzed <- collect(dfAnalyzed)

  dfFlagged <- gsm::Flag(dfAnalyzed, ...)

  return(dfFlagged)
}
