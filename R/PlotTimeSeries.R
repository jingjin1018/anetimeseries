#' @title Create interactive plot for selected time series
#'
#' @description Constructor function for creating an interactive plot for
#'   selected time series.
#'
#' @param data A data.table containing the time series data.
#' @param timeCol A string specifies the time column - this column must be of a
#'   known time-based class. If missing, time will be set as starting from "1990-01-01".
#' @param valCol A vector specifies the value column(s). If missing, all
#'   non-time columns will be assigned to it.
#' @param main Main plot title (optional).
# @param sep A logical value specifying whether each series to be plotted in an
#   independent graph. If missing, all series will be plotted in a consolidated
#   single graph.
#' @return An interactive dygraph plot showing all time series specified in
#'   valCol.
#' @seealso \code{\link[dygraphs]{dygraph}}
#' @examples
#'  PlotTimeSeries(ProcessorTime, "time")
#'  PlotTimeSeries(ProcessorTime, "time", c("BN2", "CO4"))
#'  PlotTimeSeries(data.table(x = runif(100)))
#' @export
#' @importFrom xts xts
#' @importFrom dplyr "%>%"
#' @importFrom data.table data.table
#' @import dygraphs
#'

PlotTimeSeries <- function(data, timeCol = NULL, valCol = "All", main = ""){
  time <- as.Date("1990-01-01") + c(1:dim(data)[1])
  if(is.null(timeCol)){
    data$Time <- time
    timeCol <- "Time"
  }
  if (any(valCol != "All")){
    val <- data[, valCol, with = FALSE]
    if (!all(valCol %in% colnames(data))) stop("Error: some of the value columns doens't exist in data")
  } else {
    val <- data[, !timeCol, with = FALSE]
  }
  if(!all(sapply(val, is.numeric))){
    stop("Error: not all value columns are numeric")
  }
  time <- data[[timeCol]]
  ts <- xts(val, order.by = time)
  p <- dygraph(ts) %>%
    dyRangeSelector() %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3), highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)
  p$x$css = "
    .dygraph-legend > span {display:none;}
    .dygraph-legend > span.highlight {display:inline;}
  "
  p
}
