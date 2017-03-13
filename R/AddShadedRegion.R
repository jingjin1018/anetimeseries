#'@title Add Background Shaded Region to an dygraph
#'
#'@description Specify the region of a \code{dyGraph}, this function will add a
#'  background shading of \code{color} at specified region
#'
#'@param dyGraph A dygraph object to add shading to
#'@param hightlight A logical vector - must be of the same length as the time of
#'  \code{dyGraph}
#'@param color Color of shading. This can be of the form "#AABBCC" or "rgb(255,
#'  100, 200)" or "yellow". Defaults to be red.
#'@return A dygraph with the specified shading
#'@examples
#'set.seed(1)
#'p <- PlotTimeSeries(ProcessorTime, "time", c("BN2", "CO4"))
#'highlight <- sample(c(T, F), length(ProcessorTime$time), replace = TRUE, prob = c(0.1, 0.9))
#'AddShadedRegion(p, highlight)
#'@importFrom dygraphs dyShading
#'@importFrom dplyr "%>%"
#'@export

AddShadedRegion <- function(dyGraph, highlight, color = "red") {
  time <- attr(dyGraph$x, "time")
  if(length(highlight) != length(time)){
    stop("Error: the length of highlight doesn't equal to the time of dygraph")
  }
  indices <- GetStartEndIndices(highlight)
  start <- indices$start
  end <- indices$end
  for(i in seq_along(start)) {
    dyGraph <- dyGraph %>% dyShading(from = time[start[i]], to = time[end[i]+1], color = color)
  }
  dyGraph
}
