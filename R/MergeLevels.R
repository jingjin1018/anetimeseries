#' @title Signal Quantization
#'
#' @description This function rounds the input \code{signal} with a large set of
#'   input levels (values that more than 1 observation have) to a countable
#'   smaller set. The quantization error - the difference between an input value
#'   and its quantized value - is smaller than \code{minDiff}.
#'
#' @param signal a numeric vector.
#' @param minDiff a numeric value indicating the maximum round-off error
#' @return The returned value is a list of two vectors: \itemize{\item signal a
#'   numeric vector specifying the output signal (with smaller set of levels);
#'   \item levels.index an integer vector specifying the index of all the
#'   observations that belongs to any level}
#' @examples
#' signal <- c(rep(-1.1, 10), rep(5, 10), rep(-1, 10), 3, rep(4, 10), rep(3.9, 10))
#' output <- MergeLevels(signal, 0.2)
#' ts <- data.table(input = signal, output = output$signal, time = (as.Date("2017-01-01") + seq_along(signal)))
#' PlotTimeSeries(ts, "time")
#' @export
#'
MergeLevels <- function(signal, minDiff) {
  DecimalPlaces <- nchar(strsplit(as.character(minDiff), "\\.")[[1]][2])
  signal <- round(signal, DecimalPlaces)
  while(TRUE){
    ContingencyTable <- as.data.frame(table(signal), stringsAsFactors = FALSE)
    ContingencyTable$signal <- as.numeric(ContingencyTable$signal)
    levels <- ContingencyTable$signal[ContingencyTable$Freq > 1]
    levels.index <- which(signal %in% levels)
    levels.timesorted <- unique(signal[levels.index])
    changes <- abs(diff(levels.timesorted))
    if(length(changes) == 0 | sum(changes < minDiff) == 0) {
      return(list(signal = signal, levels.index = levels.index))
    }
    index <- which.min(changes)
    index.max <- max(which(signal == levels.timesorted[index + 1]))
    index.min <- min(which(signal == levels.timesorted[index]))
    v1 <- levels.timesorted[index]
    v2 <- levels.timesorted[index + 1]
    w1 <- ContingencyTable$Freq[ContingencyTable$signal == levels.timesorted[index]]
    w2 <- ContingencyTable$Freq[ContingencyTable$signal == levels.timesorted[index + 1]]
    signal[index.min:index.max] <- round((v1*w1 + v2*w2)/(w1 + w2), DecimalPlaces)
  }
}
