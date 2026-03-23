#' Plots habitat suitability index curves
#'
#' \code{HSIplotter} plots all suitability curves.
#'
#' @import graphics
#' @import grDevices
#'
#' @param SI matrix or dataframe of suitability curves ordered as parameter breakpoints and
#'   associated suitability indices for each parameter with appropriate column names.
#'   Models containing both categorical and continuous parameters must be entered as a dataframe.
#' @param figure.name output figure file name structured as "filename.jpeg".
#'
#' @return A multi-panel *.jpeg figure showing all suitability curves.
#'
#' @references
#' US Fish and Wildlife Service. (1980). Habitat as a basis for environmental assessment.
#' Ecological Services Manual, 101.
#'
#' US Fish and Wildlife Service. (1980). Habitat Evaluation Procedures (HEP).
#' Ecological Services Manual, 102.
#'
#' US Fish and Wildlife Service. (1981). Standards for the Development of
#' Habitat Suitability Index Models. Ecological Services Manual, 103.
#'
#' @examples
#' #Build and define a matrix of the Barred Owl suitability curves
#' #Allen A.W. 1982. Habitat Suitability Index Models: Barred owl. FWS/OBS 82/10.143.
#' #U.S. Fish and Wildlife Service. https://pubs.er.usgs.gov/publication/fwsobs82_10_143.
#' var1 <- cbind(c(0,2,4,NA), c(0.1,1,1,NA)) #Number of trees > 51cm diameter per 0.4 ha plot
#' var2 <- cbind(c(0,5,20,NA), c(0,0,1,NA)) #Mean diameter of overstory trees
#' var3 <- cbind(c(0,20,60,100), c(0,0,1,1)) #Percent canopy cover of overstory trees
#' barredowl <- cbind(var1, var2, var3)
#' colnames(barredowl)<- c("tree.num", "tree.num.SIV",
#'   "avg.dbh.in", "avg.dbh.SIV", "can.cov", "can.cov.SIV")
#'
#' #Create suitability curve summary plot
#' HSIplotter(barredowl, tempfile("BarredOwl",fileext=".jpeg"))
#' 
#' 
#' #Build and define a matrix of the alewifeSAEL curves
#' #Pardue, GB. 1983. Habitat suitability index models: alewife and blueback herring. 
#' U.S. Dept. Int. Fish Wildl. Serv. FWS/OBS-82/10.58. 22pp.
#' var1 = data.frame(subs.type.class = c("a", "b", "c", NA, NA, NA), 
#'                   subs.type.class.SIV = c(1, 0.5, 0.1, NA, NA, NA))
#' var2 = data.frame("avg.daily.wtr.temp.spwn.C" = c(5, 10, 15, 20, 27, 30), 
#'                 "avg.daily.wtr.temp.spwn.C.SIV" = c(0, 0, 1, 1, 0, 0))
#' alewifeSAEL = data.frame(var1, var2)
#' 
#' #Create suitability curve summary plot
#' HSIplotter(alewifeSAEL, tempfile("AlewifeSAEL",fileext=".jpeg"))
#' 
#' #Use HSIplotter with inputs from HSImodels
#' HSIplotter(HSImodels$barredowl, tempfile("BarredOwl",fileext=".jpeg"))
#'
#' @export
HSIplotter <- function(SI, figure.name){
  # Save old par and restore upon exiting function
    oldpar <- par("mfrow", "mgp", "mar")
    on.exit(par(oldpar))

  # Number of variables in the suitability index model
  nSI <- length(colnames(SI)) / 2
  
  # Check that no columns contain invalid values
  if (any(is.infinite(SI))) {
    stop("Suitability Index values cannot be infinite", call. = FALSE)
  }
  
  # Check that all suitability indices are between 0 and 1
  even_cols <- seq(2, ncol(SI), by = 2)
  
  if(any(SI[, even_cols] < 0 | SI[, even_cols] > 1, na.rm = TRUE)){
    stop("Suitability Index values must be between 0 and 1.", call. = FALSE)
  }

  #Identify continuous and categorical variables based on first entry of each suitability curve
  SI.cont <- c()
  for(i in 1:nSI){SI.cont[i] <- is.numeric(SI[1,2*i-1])}

  # Create plot and set formatting
  jpeg(filename=figure.name, units="in", width=12, height=4*ceiling(nSI/3), res=400)
  par(mfrow=c(ceiling(nSI/3),3), mgp=c(2,0.5,0), mar=c(3.5,3.5,3,1))

  # Loop to add suitability curves as separate plots
  for(i in 1:nSI){
    # Continuous variables
    if(SI.cont[i] == TRUE){
      plot(SI[,2*i-1], SI[,2*i], pch=19, col="black",
           xlab=colnames(SI)[2*i-1], ylab="Suitability Index",
           ylim=c(0,1))
      lines(SI[,2*i-1], SI[,2*i], lwd=2, col="black")
      box()

    # Categorical variables
    } else {
      barplot(SI[,2*i], names.arg=SI[,2*i-1], col="black",
              xlab=colnames(SI)[2*i-1], ylab="Suitability Index",
              ylim=c(0,1))
      box()
    }
  }
  invisible(dev.off())
}
