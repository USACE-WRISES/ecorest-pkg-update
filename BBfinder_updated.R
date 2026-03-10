###Purpose: To fix bugs in BBfinder after failed tests
#Authors: Darixa Hernandez

#Addresses these Failed tests:
#single CE plan
#mismatched number of benefits
#mismatched number of costs
#mismatched number of cost-effective plans
#Invalid inputs that are negative or non-finite numbers
#Invalid non-numeric inputs
# It also adds a guardrail for duplicate BB plans that could have a 0 denominator and cause an error

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

