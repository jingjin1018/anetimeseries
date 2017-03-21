#' @title Tiered Anomaly Detection using Robust Linear Regression
#'
#' @description A technique for detecting whether current observation in a
#'   multivariate time series is anomaly given all observations up to and
#'   including the current time. \cr \cr The algorithm used in this function is as
#'   follows. Define a moving window of certain length. All the \code{num.train}
#'   points fall within this moving window are taken as training set. At each
#'   sample time, we first determine whether the response variable of the test
#'   point is above the sample quantile corresponds to the probability
#'   \code{prob} and calculated using the training set. Only those above the
#'   calculated sample quantile are considered as anomaly candidates. \cr For
#'   each of those anomaly candiates, construct a robust linear regression model
#'   (M estimation with inverse variance weighting) using predictors defined in
#'   the \code{formula}. We assume the normalized residual follows
#'   t-distribution with certain degrees of freedom \code{degF}. The test point
#'   is marked as anomaly if the p-value is smaller than the predifined
#'   threshold \code{p.t}. To minimize the effect of anomalies, those marked as
#'   anomalies are excluded from the sample quantile calculation and robust
#'   linear regression model fitting. \cr We believe that spikes that last for a
#'   short period of time are of less concern, therefore, the 1st-tier warnings
#'   are fired only when more than \code{min.anom} of past adjacent points are
#'   all anomalies. This method successfully decreases the false positive rate
#'   by suppressing the amount of warnings for short-time self-healing
#'   incident.\cr Two-tiered approach is used to differenciate incidents with
#'   different severity. When warning is triggered, the weighted average t-score
#'   of all previous adjacent anomalies is calculated according to \eqn{(t[0] +
#'   0.5*t[-1] + 0.5^2*t[-2] + ...)/(1 + 0.5 + 0.5^2 + ...)}. If the weighted
#'   average is larger than the predefined threshold \code{min.tscore}, 2nd-tier
#'   alert is fired.
#'
#' @param data data frame from which variables specified in \code{formula} are
#'   preferentially to be taken.
#' @param formula  a robust linear regression formula of the form \code{response
#'   ~ predictor1 + predictor2 + ...}.
#' @param num.train an integer specifying the number of training points.
#' @param prob probability threshold with values in \emph{[0, 1]}. Each test
#'   point is compared with the sample quantile calculated using this
#'   probability. Only those above the sample quantile are considered as an
#'   anomaly candidates.
#' @param p.t p-value threshold with values in \emph{[0, 1]}.
#' @param degF degrees of freedom (>0, maybe non-integer), which determines the
#'   particular form of t-distribution used to fit the residuals.
#' @param min.anom the minimum number of adjacent anomolies detected previously
#'   to fire the 1st-tier warning about current point.
#' @param min.tscore the minimum weighted average t-score required to fire the
#'   2nd-tier alert about current point
#' @return The returned value is a list with the following four vectors:
#'   \itemize{ \item \code{alert} a logical vector specifying whether points at
#'   current index should fire the 1st-tier alert; \item \code{warning} a
#'   logical vector specifying whether points at current index should fire the
#'   2nd-tier warning; \item \code{residuals} a numeric vector specifying the
#'   residuals predicted using robust linear regression models at each point;
#'   \item \code{stdevs} a numeric vector specifying the standard deviation of
#'   the training residuals at each point; }
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
#'  dt <- data.table::data.table(ProcessorTimeBN2, time, residuals = result$residuals, stdevs = result$stdevs)
#'  p <- PlotTimeSeries(dt, "time")
#'  p <- AddShadedRegion(p, result$warning, "yellow")
#'  p <- AddShadedRegion(p, result$alert, "red")
#'  p
#' @export
#'
DetectAnomalies_RLM <- function(data, formula, num.train = 1008, prob = 0.9, p.t = 1e-5, degF = 10, min.anom = 3, min.tscore = 10) {
  len <- dim(data)[1]
  num.test <- len - num.train
  if(num.test <= 0) stop("Error: too many training point. Check the argument 'num.train'.")
  if(class(formula) != "formula") stop("Error: 'formula' is not a formula class object.")
  if(!all(all.vars(formula) %in% colnames(data))) stop("Error: at least one variable specified in 'formula' is not in 'data'.")
  LHS <- data[, all.vars(formula)[1]]
  excluded <- rep(FALSE, len)
  warning <- rep(FALSE, len)
  alert <- rep(FALSE, len)
  numPrevAnom <- rep(0, len)
  residuals <- c(rep(NA, num.train), rep(0, num.test))
  stdevs <- c(rep(NA, num.train), rep(0, num.test))
  tscores <- c(rep(NA, num.train), rep(0, num.test))
  for (i.trainS in (1:num.test)) {
    i.trainE <- i.trainS + num.train - 1
    i.test <- i.trainS + num.train
    e <- excluded[i.trainS:i.trainE]
    y <- LHS[i.trainS:i.test]
    # print(paste(i.trainS, sum(e)))
    if (LastPointAboveQuantile(y, e, prob)) {
      data.train <- data[i.trainS:i.trainE, ]
      data.train$e <- e
      data.test <- data[i.test, ]
      y.test <- LHS[i.test]
      m <- rlm(formula, data.train, subset = (!e), maxit = 200, method = "M", wt.method = "inv.var")
      residual.train <- m$residuals
      residual.test <- y.test - predict(m, data.test)
      isAnomaly <- TestPointIsAnomaly_TDist(residual.train, residual.test, NULL, p.t, degF)
      residuals[i.test] <- residual.test
      tscores[i.test] <- attr(isAnomaly, "tscore")
      stdevs[i.test] <- attr(isAnomaly, "stdev")
      excluded[i.test] <- isAnomaly
    }
    if (excluded[i.test]) numPrevAnom[i.test] <- numPrevAnom[i.trainE] + 1
    if (numPrevAnom[i.test] > min.anom) {
      warning[i.test] <- TRUE
      tSum <- 0
      j <- 0
      while(excluded[i.test-j]) {
        tSum <- tSum + 0.5^j*tscores[i.test-j]
        j <- j + 1
      }
      tAvg <- tSum/(2-0.5^(j-1))
      if (tAvg > min.tscore) alert[i.test] <- TRUE
    }
  }
  return(list(alert = alert, warning = warning, residuals = residuals, stdevs = stdevs))
}
