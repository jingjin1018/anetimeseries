#'@title Indices of Continuous Subsequences of Certain Value
#'
#'@description Give all start and end indices of continuous subsequeces of
#'  \code{value} inside vector \code{x}
#'
#'@param x a vector of any given length and mode, e.g. "logical", "integer",
#'  "numeric" etc, from which to find the index of \code{value}
#'@param value an element to search for
#'@return A list of two vectors \itemize{\item \code{start} contains the start indices of continuous subsequeces of
#'  \code{value} inside vector \code{x} \item \code{end} contains the end indices of continuous subsequeces of
#'  \code{value} inside vector \code{x}}
#'@examples
#'x <- c(T, T)
#'GetStartEndIndices(x, T)
#'x <- c(F, F, T, F, T, T, T, F)
#'GetStartEndIndices(x, T)
#'x <- c(1, 1, 2, 3, 4, 1, 1, 1)
#'GetStartEndIndices(x, 1)
#'@export
#'
GetStartEndIndices <- function(x, value = TRUE){
  i <- which(x == value)
  if(length(i) == 0) return(list(start = NA, end = NA))
  i_head <- i[1:length(i)-1]
  i_tail <- i[2:length(i)]
  start <- c(i[1], i_tail[i_tail != i_head + 1])
  end  <- c(i_head[i_head != i_tail - 1], tail(i, 1))
  list(start = start, end = end)
}
