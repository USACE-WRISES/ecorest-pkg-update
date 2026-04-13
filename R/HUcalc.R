#' Computes Habitat Quality, Quantity, and Units
#'
#' \code{HUcalc} computes habitat units given a set of suitability indices,
#'  a habitat suitability index equation, and habitat quantity. Note that U.S. 
#'  Army Corps of Engineers users applying HUcalc must have approval from the 
#'  National Ecosystem Planning Center of Expertise (Eco-PCX) prior to 
#'  development or application of a new model.
#'
#' @param SI.out is a vector, matrix, or data frame of application-specific 
#' suitability indices between 0 and 1, which can be produced from SIcalc.
#' @param habitat.quantity is a numeric value representing habitat size associated 
#' with these suitability indices (i.e., length, area, or volume).
#' @param HSIfunc is a function used to combine suitability indices into a 
#' composite habitat suitability index (HSI score) (e.g., ecorest functions 
#' like HSIarimean or HSIgeomean or functions outside ecorest like max or mean)
#' @param ... optional arguments to HSIfunc.
#'
#' @return A vector of habitat quality, habitat quantity, and index
#'   units (quantity times quality).
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
#' #Summarize habitat outcomes based on a vector of two suitability indices
#' #using multiple combination equations.
#' HUcalc(c(0.1,1), 100, HSIarimean)
#' HUcalc(c(0.1,1), 100, HSIgeomean)
#' HUcalc(c(0.1,1), 100, HSImin)
#' HUcalc(c(0.1,1), 100, HSIwarimean, c(1,0))
#' HUcalc(c(0.1,1), 100, HSIwarimean, c(0,1))
#'
#' #HSIfunc can also represent functions outside of the ecorest package
#' HUcalc(c(0.1,1), 100, mean)
#' HUcalc(c(0.1,1), 100, max)
#' 
#' #Summarize habitat outcomes based on a data frame of three suitability indices
#' SI.out = data.frame(SI = c(0.1, 0.4, 0.5))
#' HUcalc(SI.out, 10, HSImin)
#'
#' @export
HUcalc <- function(SI.out, habitat.quantity, HSIfunc,...){
    warning("U.S. Army Corps of Engineers users must have approval from the National Ecosystem 
  Planning Center of Expertise (Eco-PCX) prior to application of a new model.")
  
  # Convert input to vector
  SI.out = unlist(SI.out)
  habitat.quantity = unlist(habitat.quantity)
  
  # Create an empty vector to store outputs
  HU.out <- as.data.frame(matrix(NA,nrow=1,ncol=3))
  colnames(HU.out) <- c("Quality", "Quantity", "IndexUnits")
  
  # Test whether inputs are valid
  if (any(is.infinite(SI.out) | !is.numeric(SI.out))) {
    stop("Non-NA inputs must be finite numeric values.")
  }
  
  if (any(SI.out < 0 | SI.out > 1, na.rm = TRUE)) {
    stop("Suitability indices in SI.out must be between 0 and 1.", call. = FALSE)
  }
  
  if (habitat.quantity < 0 | !is.numeric(habitat.quantity) | is.infinite(habitat.quantity)) {
    stop("Habitat quantity must be a finite positive number.", call. = FALSE)
  } 
  
  # Compute outputs
  HU.out$Quality <- HSIfunc(SI.out,...)
  HU.out$Quantity <- habitat.quantity
  HU.out$IndexUnits <- HU.out$Quality * HU.out$Quantity
  
  # Return habitat summary
  return(HU.out)
}
