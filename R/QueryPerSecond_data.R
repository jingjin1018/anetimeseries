#' @title Average Web Answer Request Queries Per Second in 3 Clusters
#'
#' @description The data was extracted from Perf Counter Data in Cosmos for three
#'   clusters, i.e. \emph{BN2}, \emph{CH1B}, \emph{CO4}. It spans from \emph{2015-10-21} to
#'   \emph{2016-10-21} and gives the measurements in count of the variable
#'   \emph{web answer request queries per second}.
#'
#' @docType data
#' @usage QueryPerSecond
#' @format A data.table with 1 time column and 3 value columns named after
#'   clusters.
#' @keywords datasets
#' @references \href{http://sharepoint/sites/autopilot/default.aspx}{Perf Counter Data in Cosmos}
#' @examples
#' QueryPerSecond
"QueryPerSecond"
