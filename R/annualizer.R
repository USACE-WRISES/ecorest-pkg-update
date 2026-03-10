#' Time-averaged restoration project outcomes
#'
#' \code{annualizer} computes time-averaged quantities based on linear interpolation.
#'
#' @param timevec numeric vector of time intervals.
#' @param benefits numeric vector of values to be interpolated.
#'
#' @return A time-averaged value over the specified time horizon.
#'
#' @references
#' Robinson R., Hansen W., and Orth K. 1995. Evaluation of environmental investments
#'   procedures manual interim: Cost effectiveness and incremental cost analyses.
#'   IWR Report 95-R-1. Institute for Water Resources, U.S. Army Corps of Engineers,
#'   Alexandria, Virginia.
#'
#' @examples
#' #Constant value through time
#' annualizer(c(0,50), c(100,100))
#' annualizer(seq(0,50), rep(100,51))
#'
#' #Simple time series
#' annualizer(seq(0,50), seq(0,50))
#'
#' #User-specified time intervals
#' demo.timevec <- c(0,2,20,50)
#' demo.ben <- c(0,100,90,80)
#' annualizer(demo.timevec, demo.ben)
#'
#' @export
annualizer <- function(timevec, benefits){
  # Compute general properties of the time and benefits data
  if(length(timevec) != length(benefits)){
    benefits.avgann <- "Number of time points does not equal number of benefit values."

  # Compute annualization
  } else {
    # Compute general properties of the time and benefits data
    ntime <- length(timevec)
    time.intervals <- timevec[-1] - timevec[-ntime]

    # Compute the area under the curve of the ecological outcomes
    area.rec <- time.intervals * apply(cbind(benefits[-ntime], benefits[-1]), 1, min)
    area.tri <- 0.5 * time.intervals * abs(benefits[-ntime] - benefits[-1])
    benefits.avgann <- sum(area.rec + area.tri) / (max(timevec) - min (timevec))
  }
  # Return annualized outcomes
  return(benefits.avgann)
}
