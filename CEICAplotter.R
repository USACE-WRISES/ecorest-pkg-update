#' Plots cost-effectiveness and incremental cost analysis
#'
#' \code{CEICAplotter} Plots Cost-effective Incremental Cost Analysis (CEICA) in *.jpeg format.
#'
#' @import graphics
#' @import grDevices
#' @import viridis
#'
#' @param altnames vector of numerics or characters as unique restoration action identifiers.
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
#' @param BB numeric vector of 0's and 1's indicating whether a plan is a best buy (1)
#'   or not (0). Can be derived from ecorest::BBfinder.
#' @param figure.name output figure file name structured as "filename.jpeg".
#'
#' @return A multi-panel *.jpeg figure summarizing cost-effectiveness and incremental
#'  cost analyses.
#'
#' @references
#' Robinson R., Hansen W., and Orth K. 1995. Evaluation of environmental investments
#' procedures manual interim: Cost effectiveness and incremental cost analyses.
#' IWR Report 95-R-1. Institute for Water Resources, U.S. Army Corps of Engineers, Alexandria, Virginia
#'
#' @examples
#'
#' #Identify cost-effective actions based on random vectors of benefits and costs
#' altnames<- paste("Alt",seq(1,50), sep="")
#' benefit <- runif(50,min=0,max=10)
#' cost <- runif(50, min=0,max=1000)
#' CE <- CEfinder(benefit, cost)
#' BB <- BBfinder(benefit, cost, CE)[[1]][,4]
#' CEICAplotter(altnames, benefit, cost, CE, BB, tempfile("CEICAexample",fileext=".jpeg"))
#'
#' @export
CEICAplotter <- function(altnames, benefit, cost, CE, BB, figure.name){
  # Save old par and restore upon exiting function
    oldpar <- par("mfrow", "mgp", "mar")
    on.exit(par(oldpar))

  # Compute number of alternatives (total, CE, and BB)
    nalt <- length(altnames)
    nCE <- sum(CE)
    nBB <- sum(BB)

  # Isolate cost-effective plans and reorder from lowest to highest cost
    ben.CE <- benefit[which(CE==1)]; cost.CE <- cost[which(CE==1)]
    ben.CE.order <- ben.CE[order(cost.CE)]; cost.CE.order <- cost.CE[order(cost.CE)]

  # Isolate best buy plans and reorder from lowest to highest cost
    ben.BB <- benefit[which(BB==1)]; cost.BB <- cost[which(BB==1)]
    ben.BB.order <- ben.BB[order(cost.BB)]; cost.BB.order <- cost.BB[order(cost.BB)]
    inccost.BB <- (cost.BB.order[-1] - cost.BB.order[-nBB]) / (ben.BB.order[-1] - ben.BB.order[-nBB])

  # Assign colors and labels to the BBs
    CEICA.col <- viridis(nBB)
    leg.name <- c()
    for(i in 1:nBB){
      leg.name[i] <- altnames[which(benefit==ben.BB.order[i] & cost==cost.BB.order[i])]
    }

  # Create plot and set formatting
    jpeg(filename=figure.name, units="in", width=12, height=6, res=400)
    par(mfrow=c(1,2), mgp=c(2,0.5,0), mar=c(3.5,3.5,3,1))

  # PLOT 1 - COST-EFFECTIVENESS
    # Format blank plot
    plot(c(0,1), c(0,1), type="n", xlim=range(benefit), ylim=range(cost), axes=FALSE,
         xlab="Ecological Benefit", ylab="Cost",
         main="(A) Cost-Effectiveness Analysis"); grid(); box()
    axis(1)
    axis(2, at=axTicks(2), labels=prettyNum(axTicks(2), big.mark=",", scientific=FALSE), las=0, cex.axis=0.75, tick=TRUE)

    # Plot all alternatives
    points(benefit, cost, pch=1, cex=0.8, col="black")

    # Plot efficiency frontier of cost-effective plans
    lines(ben.CE.order, cost.CE.order, lwd=2)
    points(ben.CE.order, cost.CE.order, pch=19, cex=1, col="black")

    # Circle BB alternatives
    points(ben.BB.order, cost.BB.order, pch=1, lwd=3, cex=3, col=CEICA.col)

    # Add legend
    legend("topleft", legend=c("All Plans", "Cost-Effective Frontier", "Best Buys"),
           lwd=c(NA, 2, NA), pch=c(1,19,1), pt.cex=c(0.8,1,3),
           col=c("black", "black", viridis(1)), bg="white")

  # PLOT 2 - INCREMENTAL COST
    # Format blank plot
      plot(c(0,1), c(0,1), type="n", xlim=range(ben.BB), ylim=c(0,max(inccost.BB)), axes=FALSE,
           xlab="Ecological Benefit", ylab="Incremental Cost per Benefit (cost/unit)",
           main="(B) Incremental Cost Analysis"); box()
      axis(1)
      axis(2, at=axTicks(2), labels=prettyNum(axTicks(2), big.mark=",", scientific=FALSE), las=0, cex.axis=0.75, tick=TRUE)

    # Loop to add incremental cost polygons of varying size
      for(i in 1:(nBB-1)){
        xtemp <- c(ben.BB.order[i], ben.BB.order[i+1], ben.BB.order[i+1], ben.BB.order[i])
        ytemp <- c(0, 0, inccost.BB[i], inccost.BB[i])
        polygon(xtemp, ytemp, col=CEICA.col[i+1], border="black")
      }

    # Add legend
      legend("topleft", legend=leg.name[-1], fill=CEICA.col[-1], bg="white")

  # Close graphic device
    invisible(dev.off())
}
