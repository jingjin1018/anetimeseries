#' @title Determine if the Last Point is Above the Sample Quantile
#'
#' @description This function \code{LastPointAboveQuantile} is used to determine
#'   whether the last observation of \code{ts} is above the sample quantile
#'   calculated using the rest of the points in \code{ts}. If argument
#'   \code{exclude} is specified, elements at those designated positions are
#'   further removed from the sample quantile calculation.
#'
#' @param ts A numeric vector whose \itemize{ \item last value is the one will
#'   be compared with the sample quantile; \item all but the last one are used
#'   to calculate the sample quantile.}
#' @param exclude A logical vector with length equals to \code{length(ts)-1}. It
#'   is used to remove elements at designated positions from calculating the
#'   sample quantile. By default, \code{exclude = NULL}, which means no element
#'   is excluded when calculating the sample quantile.
#' @param probT probability threshold with values in \emph{[0, 1]}. The sample
#'   quantile corresponds to this probability is calculated.
#' @return returns \code{TRUE} if the last element of \code{ts} is above the
#'   sample quantiles and \code{FALSE} otherwise.
#' @seealso \code{\link[stats]{quantile}}
#' @examples
#' set.seed(1)
#' ts <- c(runif(1000), 0.95)
#' exclude <- sample(c(T, F), 1000, replace = T, prob = c(0.005, 0.995))
#' LastPointAboveQuantile(ts, exclude)
#' LastPointAboveQuantile(ts, exclude, 0.95)
#' @export

LastPointAboveQuantile <- function(ts, exclude = NULL, probT = 0.9){
  test <- tail(ts, 1)
  training <- vector(mode = "numeric", length = 0L)
  if (is.null(exclude)) {
    training <- ts[1:(length(ts)-1)]
  } else {
    training <- ts[!c(exclude, T)]
  }
  quantileT <- as.numeric(quantile(training, probT, na.rm = TRUE))
  ifelse(test >= quantileT, T, F)
}
