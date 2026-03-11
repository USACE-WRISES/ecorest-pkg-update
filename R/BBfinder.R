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
  # Length check (prevents recycling) and adds mismatched length error in benefit, costs or CE
  if (length(benefit) != length(cost) || length(benefit) != length(CE)) {
    stop("Lengths of `benefit`, `cost`, and `CE` must match (one value per plan).", call. = FALSE)
  }
  # Stop non-finite, negative, and non-numeric input values
  if (any(!is.finite(benefit[!is.na(benefit)]))) {
    stop("`benefit` must contain only finite numeric values.", call. = FALSE)
  }
  if (any(!is.finite(cost[!is.na(cost)]))) {
    stop("`cost` must contain only finite numeric values.", call. = FALSE)
  }
  if (any(benefit < 0, na.rm = TRUE)) {
    stop("`benefit` must be non-negative.", call. = FALSE)
  }
  if (any(cost < 0, na.rm = TRUE)) {
    stop("`cost` must be non-negative.", call. = FALSE)
  }
  if (!is.numeric(benefit) || !is.numeric(cost) || !is.numeric(CE)) {
    stop("`benefit`, `cost`, and `CE` must be numeric vectors.", call. = FALSE)
  }
  # Isolate cost-effective actions
  ben.CE <- benefit[which(CE==1)]
  cost.CE <- cost[which(CE==1)]
  nCE <- length(ben.CE)
  
  # Single-CE value handling (prevents empty inccost)
  if (nCE == 1) {
    idx <- which(CE == 1)[1]   # index in the original vectors
    
    BB.loc <- rep(0, length(benefit))
    BB.loc[idx] <- 1
    
    BB.out <- list()
    BB.out[[1]] <- cbind(benefit, cost, CE, BB.loc)
    colnames(BB.out[[1]]) <- c("benefit", "cost", "CE", "BB")
    
    BB.out[[2]] <- cbind(benefit[idx], cost[idx], 0)
    colnames(BB.out[[2]]) <- c("benefit", "cost", "inccost")
    
    return(BB.out)
  }
  # Rank order cost-effective actions based on cost
  ben.CE2 <- ben.CE[order(cost.CE)]
  cost.CE2 <- cost.CE[order(cost.CE)]
  
  # Duplicate-benefit warning (removing duplicates to choose lowest cost)
  dup_benefit <- duplicated(ben.CE2)
  # A duplicate is "dominated" if it repeats a benefit AND has higher cost than the first occurrence
  dominated_dup <- dup_benefit & (cost.CE2 > ave(cost.CE2, ben.CE2, FUN = min))
  if (any(dominated_dup)) {
    warning(
      "Duplicate benefit values among cost-effective plans with higher costs detected. ",
      "Incremental costs are undefined when benefit differences are zero; ",
      "retaining the lowest-cost plan for each benefit level.",
      call. = FALSE
    )
    # Drop dominated duplicates: keep lowest-cost plan for each benefit
    keep <- !dup_benefit
    ben.CE2  <- ben.CE2[keep]
    cost.CE2 <- cost.CE2[keep]
    nCE <- length(ben.CE2)
  }
  # After removing dominated duplicate-benefit CE plans, it is possible that only one CE plan remains.
  # In that case, no incremental cost comparisons can be computed (empty inccost), so we flag the
  # remaining CE plan as the sole best buy and return early with inccost = 0.
  if (nCE == 1) {
    idx <- which(benefit == ben.CE2[1] & cost == cost.CE2[1] & CE == 1)[1]
    BB.loc <- rep(0, length(benefit))
    BB.loc[idx] <- 1
    BB.out <- list()
    BB.out[[1]] <- cbind(benefit, cost, CE, BB.loc)
    colnames(BB.out[[1]]) <- c("benefit", "cost", "CE", "BB")
    BB.out[[2]] <- cbind(benefit[idx], cost[idx], 0)
    colnames(BB.out[[2]]) <- c("benefit", "cost", "inccost")
    return(BB.out)
  }
  
  # Identify the lowest cost, cost-effective plan as the first "best buy"
  BB <- c(1)
  
  # Sequentially loop through cost-effective actions to identify "best buys"
  for(i in 1:nCE){
    # Compute incremental cost of all larger plans
    ce.bentemp <- ben.CE2[-1:-BB[i]]
    ce.costtemp <- cost.CE2[-1:-BB[i]]
    inccost <- (ce.costtemp - cost.CE2[BB[i]]) / (ce.bentemp - ben.CE2[BB[i]])
    
    # Isolate plan with lowest incremental cost
    BB[i+1]<-which(inccost== min(inccost,na.rm=TRUE))[1]+BB[i]
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
  for(i in 1:nBB){BB.find[i] <- which(benefit==ben.BB[i] & cost==cost.BB[i])[1]
  }
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
