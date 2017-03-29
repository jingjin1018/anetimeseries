#' @title Compare the Last Point with the Sample Quantile
#'
#' @description This function \code{LastPointVSQuantile} is used to determine
#'   the relationship between the last observation of \code{ts} and the sample
#'   quantile calculated using the rest of the points in \code{ts}. If argument
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
#'   quantile corresponds to this probability is calculated. Depends on the
#'   \code{direction}, if \itemize{\item \code{direction = "pos"} or
#'   \code{direction = "neg"} \code{probT} should be a numeric value; \item
#'   \code{direction = "both"} \code{prob} should be a numeric vector of two
#'   values in \emph{[0, 1]}}.
#' @param direction Directionality of the anomalies to be deteted. Options
#'   are: \emph{'pos'}, \emph{'neg'} and \emph{'both'}. Defaults to be
#'   \emph{'pos'}.
#' @return returns \code{TRUE} if the last element of \code{ts} is above or below the
#'   sample quantiles, \code{FALSE} otherwise.
#' @seealso \code{\link[stats]{quantile}}
#' @examples
#' set.seed(1)
#' ts <- c(rnorm(1000), 5)
#' exclude <- sample(c(T, F), 1000, replace = T, prob = c(0.005, 0.995))
#' LastPointVSQuantile(ts, exclude, 0.95, "pos")
#' LastPointVSQuantile(ts, exclude, 0.95, "neg")
#' LastPointVSQuantile(ts, exclude, c(0.1, 0.9), "both")
#' @export

LastPointVSQuantile <- function(ts, exclude = NULL, probT = 0.9, direction = "pos"){
  if (!direction %in% c("pos", "neg", "both")) stop("direction options are: pos | neg | both.")
  test <- tail(ts, 1)
  training <- vector(mode = "numeric", length = 0L)
  if (is.null(exclude)) {
    training <- ts[1:(length(ts)-1)]
  } else {
    training <- ts[!c(exclude, T)]
  }
  quantileT <- as.numeric(quantile(training, probT, na.rm = TRUE))
  switch(direction, pos = ifelse(test >= quantileT, T, F), neg = ifelse(test <= quantileT, T, F), both = ifelse((test <= min(quantileT) || test >= max(quantileT)), T, F))
}
