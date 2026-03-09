#' Computes Suitability Indices
#'
#' \code{SIcalc} computes suitability indices given a set of suitability curves
#'   and project-specific inputs. Suitability indices may be computed based on
#'   either linear interpolation (for continuous variables)
#'   or a lookup method (for categorical variables).
#'
#' @import stats
#'
#' @param SI matrix of suitability curves ordered as parameter breakpoints and
#'   associated suitability indices for each parameter. Note that users should 
#'   enter NA for excluded variables in HSImodels.
#' @param input.proj numeric or categorical vector of application-specific input
#'   parameters associated with the suitability curve data from SI.
#'
#' @return A vector of the suitability index values that match given user inputs.
#'   Values are returned as equal to the extreme of a range if inputs are outside
#'   of model range.
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
#' #Set user input variables that should return (1, 0, 0)
#' input.demo1 <- c(2, 5, 20)
#' SIcalc(barredowl, input.demo1)
#'
#' #Set user input variables that should return (1, 1, 1)
#' input.demo2 <- c(4, 20, 60)
#' SIcalc(barredowl, input.demo2)
#'
#' #Set user input variables that should return (1, 1, 0.5)
#' input.demo3 <- c(4, 20, 40)
#' SIcalc(barredowl, input.demo3)
#'
#' #Set user input variables that should return (0.1, 0.5, 0.5)
#' input.demo4 <- c(0, 12.5, 40)
#' SIcalc(barredowl, input.demo4)
#'
#' #Set user input variables that should return (1, 1, 1)
#' input.demo5 <- c(4, 40, 60)
#' SIcalc(barredowl, input.demo5)
#'
#' #Set user input variables that should return (1, NA, 1)
#' input.demo6 <- c(4, NA, 60)
#' SIcalc(barredowl, input.demo6)
#'
#' #Suitability curves may also be drawn from HSImodels (data within ecorest)
#' #Import Barred Owl suitability curves with HSImodels$barredowl
#' #The input examples are repeated from above
#'
#' #Set user input variables that should return (1, 0, 0)
#' SIcalc(HSImodels$barredowl, input.demo1)
#'
#' #Set user input variables that should return (1, 1, 1)
#' SIcalc(HSImodels$barredowl, input.demo2)
#'
#' #Set user input variables that should return (1, 1, 0.5)
#' SIcalc(HSImodels$barredowl, input.demo3)
#'
#' #Set user input variables that should return (0.1, 0.5, 0.5)
#' SIcalc(HSImodels$barredowl, input.demo4)
#'
#' #Set user input variables that should return (1, 1, 1)
#' SIcalc(HSImodels$barredowl, input.demo5)
#'
#' #Set user input variables that should return (1, NA, 1)
#' SIcalc(HSImodels$barredowl, input.demo6)
#'
#' @export
SIcalc <- function(SI, input.proj){
  #Number of variables in the suitability index model
  nSI <- length(colnames(SI)) / 2

  #Identify continuous and categorical variables based on first entry of each suitability curve
  SI.cont <- c() #TRUE = continuous, FALSE = categorical
  for(i in 1:nSI){SI.cont[i] <- is.numeric(SI[1, 2*i-1])}

  #Loop over each variable and compute suitability index values for each input
  SI.out <- c()
  for(i in 1:nSI){
    #Check that the number of inputs equals the number of SI values.
    if(length(input.proj) != nSI){
      SI.out <- "Number of inputs does not equal number of SI values."
      break

      #Send NA if NA or "NA" is input
    } else if(is.na(input.proj[i]) | input.proj[i]=="NA"){
      SI.out[i] <- NA

      #Compute suitability for continuous variables
    } else if(SI.cont[i] == TRUE){
      SI.out[i] <- approx(SI[,2*i-1], SI[,i*2], xout=input.proj[i],
                          method="linear", rule=2, ties="ordered")$y

      #Compute suitability for categorical variables
    } else {
      SI.out[i] <- SI[which(SI[,i*2-1] == input.proj[i]),i*2]
    }
  }
  return(SI.out) #Send output
}
