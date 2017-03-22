#' @title Anomalies and Change Points Detection using FLAM
#'
#' @description A technique for detecting both past and current anomalies in an
#'   univariate time series given all observations up to and including the
#'   current time. \cr \cr The algorithm used in this function is as follows:
#'   \enumerate{\item Create a time series object using \code{data},
#'   \code{frequency} and \code{time}. \item Decompose the time series into
#'   three components using STL, i.e. seasonal, trend and residual. \item Fit
#'   the trend component using Fused Lasso Additive Model (FLAM), where it is
#'   estimated to piecewise constant with a small number of adaptively-chosen
#'   knots. \item According to the change point detected in last step, split the
#'   original time series into non-overlapping windows of varying size. \item If
#'   the window size is large than 2 periods, detect anomalies within the window
#'   by applying the \code{\link[AnomalyDetection]{AnomalyDetectionVec}}
#'   function provided by twitter. Otherwise, mark all the observations that
#'   deviate from the median by more than 1.96 Median Absolute Deviation (MAD)
#'   as anomalies.}
#'
#' @param data A numeric vector containing values observed at times specified in
#'   \code{time}.
#' @param period A numeric value indicating the number of observations in a
#'   single period, and used during seasonal decomposition.
#' @param time A corresponding vector specifying times/dates. If this parameter
#'   is missing, it will be set as starting from "1990-01-01".
#' @param max_anoms Maximum number of anomalies will be detected as a percentage
#'   of the \code{data}
#' @param direction Directionality of the anomalies to be deteted. Options are:
#'   \emph{'pos'}, \emph{'neg'} and \emph{'both'}. Defaults to be \emph{'both'}
#'
#' @seealso \code{\link[stats]{stl}} \code{\link[flam]{flamCV}}
#'   \code{\link[AnomalyDetection]{AnomalyDetectionVec}}
#'
#' @examples
#' data <- SessionSuccessRate
#' p1 <- PlotTimeSeries(data, "Day", c("Value"))
#' anomalies <- DetectAnomalies_FLAM(data$Value, 7, time = data$Day)
#' p1 <- AddShadedRegion(p1, c(1:dim(data)[1]) %in% anomalies$change.points, color = "red")
#' p1 <- AddShadedRegion(p1, c(1:dim(data)[1]) %in% anomalies$anomaly.points, color = "blue")
#' p1
#'
#' @import flam
#' @importFrom AnomalyDetection AnomalyDetectionVec
#' @export
#'
DetectAnomalies_FLAM <- function(data, period, time = NULL, max_anoms = 0.03, direction = "both", ...){
  if (is.null(time)){
    time <- as.Date("1990-01-01") + seq_along(data)
  }
  signal <- ts(data, frequency = period)
  #fit <- stl(signal, s.window = "periodic")
  fit <- stl(signal, s.window = period, t.window = 2*period, robust = TRUE)

  ## detect change point using FLAM, which is heavily dependent on the value of
  ## lambda
  trend <- fit$time.series[, c("trend")]
  flamCV.out <- flamCV(x = time, y = trend, alpha = 1, ...)
  flam.out <- flam(x = time, y = trend, alpha.seq = 1, lambda.seq = c(flamCV.out$lambda.cv, 0.01)) # only lambda.cv is used
  flam.signal <- flam.out$theta.hat.list[[1]][, 1]

  remainder <- fit$time.series[, c("remainder")]
  sigma <- sd(remainder)
  minDiff <- signif(sigma, 1)
  merged.signal <- MergeLevels(flam.signal, minDiff)

  ## cut the original signal into pieces according to the change point
  ## detected in previous step
  change.points <- setdiff(seq_along(data), merged.signal$levels.index)
  level.index <- GetStartEndIndices(seq_along(data) %in% merged.signal$levels.index)
  start <- level.index$start
  end <- level.index$end
  anomaly.points <- vector("integer")
  for (i in seq_along(start)){
    subSignal <- data[start[i]:end[i]]
    subAnomalies <- vector("integer")
    if (length(subSignal) > 2 * period) {
      result <- AnomalyDetectionVec(subSignal, max_anoms = max_anoms, period = period, longterm_period = length(subSignal), direction = direction)
      subAnomalies <- result$anoms$index
    } else {
      residual <- subSignal - median(subSignal)
      subAnomalies <- which(residual < -1.96*mad(residual) | residual > 1.96*mad(residual))
    }
    anomaly.points <- c(anomaly.points, subAnomalies + start[i] - 1)
  }
  list(change.points = change.points, anomaly.points = anomaly.points)
}
