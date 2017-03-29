#' @title Tiered Anomaly Detection using Robust Linear Regression
#'
#' @description A technique for detecting whether current observation in a
#'   multivariate time series is anomaly given all observations up to and
#'   including the current time. \cr \cr The algorithm used in this function is
#'   as follows. Define a moving window of certain length. All the
#'   \code{num.train} points fall within this moving window are taken as
#'   training set. At each sample time, we first determine whether the response
#'   variable of the test point is above the sample quantile corresponds to the
#'   probability \code{prob} and calculated using the training set. Only those
#'   above the calculated sample quantile are considered as anomaly candidates.
#'   \cr For each of those anomaly candiates, construct a robust linear
#'   regression model (M estimation with inverse variance weighting) using
#'   predictors defined in the \code{formula}. We assume the normalized residual
#'   follows \code{dist} distribution. The test point is marked as anomaly if
#'   the p-value is smaller than the predifined threshold \code{p}. To minimize
#'   the effect of anomalies, those marked as anomalies are excluded from the
#'   sample quantile calculation and robust linear regression model fitting. \cr
#'   We believe that spikes that last for a short period of time are of less
#'   concern, therefore, the 1st-tier warnings are fired only when more than
#'   \code{min.anom} of past adjacent points are all anomalies. This method
#'   successfully decreases the false positive rate by suppressing the amount of
#'   warnings for short-time self-healing incident.\cr Two-tiered approach is
#'   used to differenciate incidents with different severity. When warning is
#'   triggered, the weighted average score of all previous adjacent anomalies is
#'   calculated according to \eqn{(s[0] + 0.5*s[-1] + 0.5^2*s[-2] + ...)/(1 +
#'   0.5 + 0.5^2 + ...)}. If the weighted average is larger than the predefined
#'   threshold \code{min.score}, 2nd-tier alert is fired.
#'
#' @param data data.frame from which variables specified in \code{formula} are
#'   preferentially to be taken.
#' @param formula  a robust linear regression formula of the form \code{response
#'   ~ predictor1 + predictor2 + ...}.
#' @param num.train an integer specifying the number of training points.
#' @param prob probability threshold with values in \emph{[0, 1]}. Each test
#'   point is compared with the sample quantile calculated using this
#'   probability. Depends on the \code{direction}, if \itemize{\item
#'   \code{direction = "pos"} \code{prob} should be a numeric value. Only those
#'   above the sample quantile are considered as anomaly candidates; \item
#'   \code{direction = "neg"} \code{prob} should be a numeric value. Only those
#'   below the sample quantile are considered as anomaly candidates; \item
#'   \code{direction = "both"} \code{prob} should be a numeric vector of two
#'   values in \emph{[0, 1]}. Only those outside of the calculated quantile
#'   range are considered as anomaly candidates}.
#' @param direction Directionality of the anomalies to be deteted. Options are:
#'   \emph{'pos'}, \emph{'neg'} and \emph{'both'}. Defaults to be \emph{'pos'}.
#' @param min.anom the minimum number of adjacent anomolies detected previously
#'   to fire the 1st-tier warning about current point.
#' @param min.score the minimum weighted average score required to fire the
#'   2nd-tier alert
#' @param p p-value threshold with values in \emph{[0, 1]}.
#' @param dist A string specifies the distribution to fit. Options are
#'   t-distribution(default: \code{dist = "t"}), normal distribution(\code{dist
#'   = "normal"}). If t-distribution is selected, remember to pass in parameter
#'   \code{degrees of freedom, i.e. df}

#' @return The returned value is a list with the following five items: \itemize{
#'   \item \code{alert} a logical vector specifying whether points at current
#'   index should fire the 1st-tier alert; \item \code{warning} a logical vector
#'   specifying whether points at current index should fire the 2nd-tier
#'   warning; \item \code{residuals} a numeric vector specifying the residuals
#'   predicted using robust linear regression models at each point; \item
#'   \code{mads} a numeric vector specifying the mean absolute deviation of the
#'   training residuals at each point; \item \code{hists} a list of static
#'   plots, each of which contains histogram of training points overlaid with
#'   stats function used to fit}
#' @seealso \code{\link[MASS]{rlm}}
#' @importFrom MASS rlm
#' @importFrom data.table data.table
#' @importFrom lubridate hour
#' @importFrom lubridate minute
#' @examples
#'  ProcessorTimeBN2 <- ProcessorTime$BN2
#'  QPS <- QueryPerSecond$BN2
#'  time <- QueryPerSecond$time
#'  Weekdays <- weekdays(time)
#'  Hours <- (lubridate::hour(time) * 60 + lubridate::minute(time))/20 # in 20-mins interval
#'  data <- data.frame(ProcessorTimeBN2, QPS, Weekdays, Hours)
#'  result <- DetectAnomalies_RLM(data, ProcessorTimeBN2 ~ QPS + Weekdays + Hours)
#'  dt <- data.table::data.table(ProcessorTimeBN2, time, residuals = result$residuals, mads = result$mads)
#'  p <- PlotTimeSeries(dt, "time")
#'  p <- AddShadedRegion(p, result$warning, "yellow")
#'  p <- AddShadedRegion(p, result$alert, "red")
#'  p
#'  result$hists[[21860]]
#'
#'  result <- DetectAnomalies_RLM(data, ProcessorTimeBN2 ~ QPS + Weekdays + Hours, prob = 0.9, direction = "pos", p = 1e-5, dist = "t", df = 10)
#'  dt <- data.table::data.table(ProcessorTimeBN2, time, residuals = result$residuals, mads = result$mads)
#'  p <- PlotTimeSeries(dt, "time")
#'  p <- AddShadedRegion(p, result$warning, "yellow")
#'  p <- AddShadedRegion(p, result$alert, "red")
#'  p
#'  result$hists[[21860]]
#'
#' @export
#'
DetectAnomalies_RLM <- function(data, formula, num.train = 1008, prob = c(0.1, 0.9), direction = "both", min.anom = 3, min.score = 10, p = 0.01, dist = "normal", ...) {
  len <- dim(data)[1]
  num.test <- len - num.train
  if(num.test <= 0) stop("Error: zero test point. Check the argument 'num.train'.")
  if(class(formula) != "formula") stop("Error: 'formula' is not a formula class object.")
  if(!all(all.vars(formula) %in% colnames(data))) stop("Error: at least one variable specified in 'formula' is not in 'data'.")
  if (!direction %in% c("pos", "neg", "both")) stop("direction options are: pos | neg | both.")
  if (direction == "both" && length(prob) != 2) stop("If direction is both, 'prob' has to be a vector of two elements")
  LHS <- data[, all.vars(formula)[1]]
  excluded <- rep(FALSE, len)
  warning <- rep(FALSE, len)
  alert <- rep(FALSE, len)
  numPrevAnom <- rep(0, len)
  residuals <- c(rep(NA, num.train), rep(0, num.test))
  mads <- c(rep(NA, num.train), rep(0, num.test))
  scores <- c(rep(NA, num.train), rep(0, num.test))
  hists <- vector("list", len)
  for (i.trainS in (1:num.test)) {
    i.trainE <- i.trainS + num.train - 1
    i.test <- i.trainS + num.train
    e <- excluded[i.trainS:i.trainE]
    y <- LHS[i.trainS:i.test]
    # print(paste(i.trainS, sum(e)))
    if (LastPointVSQuantile(y, e, prob, direction)) {
      data.train <- data[i.trainS:i.trainE, ]
      data.train$e <- e
      data.test <- data[i.test, ]
      y.test <- LHS[i.test]
      m <- rlm(formula, data.train, subset = (!e), maxit = 200, method = "M", wt.method = "inv.var")
      residual.train <- m$residuals
      residual.test <- y.test - predict(m, data.test)
      isAnomaly <- TestPointIsAnomaly(residual.train, residual.test, dist = dist, exclude = NULL, p = p, direction = direction, ...)
      residuals[i.test] <- residual.test
      scores[i.test] <- attr(isAnomaly, "score")
      mads[i.test] <- attr(isAnomaly, "mad")
      hists[[i.test]] <- attr(isAnomaly, "hist")
      excluded[i.test] <- isAnomaly
    }
    if (excluded[i.test]) numPrevAnom[i.test] <- numPrevAnom[i.trainE] + 1
    if (numPrevAnom[i.test] > min.anom) {
      warning[i.test] <- TRUE
      tSum <- 0
      j <- 0
      while(excluded[i.test-j]) {
        tSum <- tSum + 0.5^j*scores[i.test-j]
        j <- j + 1
      }
      tAvg <- tSum/(2-0.5^(j-1))
      if (tAvg > min.score) alert[i.test] <- TRUE
    }
  }
  return(list(alert = alert, warning = warning, residuals = residuals, mads = mads, hists = hists))
}
