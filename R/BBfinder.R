#' Identifies "best buy" actions
#'
#' \code{BBfinder} this analysis examines the slope of the cost-effectiveness frontier to
#'   isolate how unit cost (cost/benefit) increases with increasing environmental benefit.
#'   Restoration actions with the lowest slope of unit cost are considered "best buys".
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
#' @param CE numeric vector of 0's and 1's indicating whether a plan is cost-effective (1)
#'   or non-cost-effective (0). Can be derived from ecorest::CEfinder.
#'
#' @return A list with summaries of all restoration actions as well as best buy plans only.
#'
#' @references
#' Robinson R., Hansen W., and Orth K. 1995. Evaluation of environmental investments
#' procedures manual interim: Cost effectiveness and incremental cost analyses.
#' IWR Report 95-R-1. Institute for Water Resources, U.S. Army Corps of Engineers,
#' Alexandria, Virginia
#'
#' @examples
#' #Identify cost-effective actions based on random vectors of benefits and costs
#' benefit <- runif(50,min=0,max=10)
#' cost <- runif(50, min=0,max=1000)
#' CE <- CEfinder(benefit, cost)
#' BBfinder(benefit, cost, CE)
#'
#' #Identify cost-effective actions based on a small number of user-specified benefits and costs
#' restben <- c(0, 10, 5, 20, 20)
#' restcost <- c(0, 100, 100, 200, 150)
#' restCE <- CEfinder(restben, restcost)
#' BBfinder(restben, restcost, restCE)
#'
#'
#' @export
BBfinder <- function(benefit, cost, CE){
  # Isolate cost-effective actions
  ben.CE <- benefit[which(CE==1)]
  cost.CE <- cost[which(CE==1)]
  nCE <- length(ben.CE)

  # Rank order cost-effective actions based on cost
  ben.CE2 <- ben.CE[order(cost.CE)]
  cost.CE2 <- cost.CE[order(cost.CE)]

  # Identify the lowest cost, cost-effective plan as the first "best buy"
  BB <- c(1)

  # Sequentially loop through cost-effective actions to identify "best buys"
  for(i in 1:nCE){
    # Compute incremental cost of all larger plans
    ce.bentemp <- ben.CE2[-1:-BB[i]]
    ce.costtemp <- cost.CE2[-1:-BB[i]]
    inccost <- (ce.costtemp - cost.CE2[BB[i]]) / (ce.bentemp - ben.CE2[BB[i]])

    # Isolate plan with lowest incremental cost
    BB[i+1] <- which(inccost==min(inccost)) + BB[i]
    if(BB[i+1]>=nCE){break}
  }

  # Count the number of best buys and identify the cost, benefit,
  # and incremental cost associated with each
  nBB <- length(BB)
  ben.BB <- ben.CE2[BB]
  cost.BB <- cost.CE2[BB]
  inccost.BB <- (cost.BB[-1] - cost.BB[-nBB]) / (ben.BB[-1] - ben.BB[-nBB])

  # Locate the best buys in the original vector of cost effective plans
  BB.find <- c()
  for(i in 1:nBB){BB.find[i] <- which(benefit==ben.BB[i] & cost==cost.BB[i])}
  BB.loc <- rep_len(0, length.out=length(benefit))
  BB.loc[BB.find] <- 1

  # Create a list summarizing incremental cost analysis in multiple formats
  BB.out <- list()
    # Summary of all plans
    BB.out[[1]] <- cbind(benefit, cost, CE, BB.loc)
    colnames(BB.out[[1]]) <- c("benefit", "cost", "CE", "BB")

    #Summary of best buy plans
    BB.out[[2]] <- cbind(ben.BB, cost.BB, c(0,inccost.BB))
    colnames(BB.out[[2]]) <- c("benefit", "cost", "inccost")

  # Return best buy status of each restoration action
  return(BB.out)
}
