#' Finds cost-effective frontier
#'
#' \code{CEfinder} returns cost-effectiveness analysis for a particular set of alternatives.
#'
#' @param benefit a vector of restoration benefits. Typically, these are time-averaged
#'   ecological outcomes (e.g., average annual habitat units). Often project benefits
#'   are best presented as the "lift" associated with a restoration action
#'   (i.e., the benefits of an alternative minus the benefits of a "no action" plan).
#' @param cost a vector of restoration costs. Typically, these are monetary costs
#'   associated with a given restoration action such as project first cost or
#'   annualized economic cost. Notably, these functions are agnostic to units, so costs
#'   could also be non-monetary such as lost political capital or social costs
#'   of each alternative.
#'
#' @return A numeric vector identifying each plan as cost-effective (1) or
#'   non-cost-effective (0). The cost-effective actions comprise the Pareto frontier
#'   of non-dominated alternatives at a given level of cost or benefit.
#'
#' @references
#' Robinson R., Hansen W., and Orth K. 1995. Evaluation of environmental investments
#' procedures manual interim: Cost effectiveness and incremental cost analyses.
#' IWR Report 95-R-1. Institute for Water Resources, U.S. Army Corps of Engineers, Alexandria, Virginia
#'
#' @examples
#' #Identify cost-effective actions based on random vectors of benefits and costs
#' CEfinder(runif(50,min=0,max=10), runif(50, min=0,max=1000))
#'
#' #Identify cost-effective actions based on a small number of user-specified benefits and costs
#' restben <- c(0, 10, 5, 20, 20)
#' restcost <- c(0, 100, 100, 200, 150)
#' CEfinder(restben, restcost)
#'
#' @export
CEfinder <- function(benefit, cost){

  # Create empty vector to store cost-effectiveness status
  CE <- c()
  for(i in 1:length(benefit)){

    # Isolate each restoration action with benefits larger than this action
    bigben <- which(benefit >= benefit[i])

    # Count the number of plans with larger benefits than have lower cost
    # Store cost-effectiveness status for this action
    # 1 = non-dominated, cost-effective and 0 = dominated, cost-ineffective
    CE[i] <- ifelse(length(which(cost[bigben] <= cost[i]))==1, 1, 0)
  }

  # Return cost-effectiveness status of each restoration action
  return(CE)
}
