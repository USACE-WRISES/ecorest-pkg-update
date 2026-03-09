#' Computes Habitat Quality, Quantity, and Units
#'
#' \code{HUcalc} computes habitat units given a set of suitability indices,
#'  a habitat suitability index equation, and habitat quantity.
#'
#' @param SI.out is a vector of application-specific suitability indices,
#'   which can be produced from SIcalc.
#' @param habitat.quantity is a numeric of habitat size associated with these
#'   suitability indices (i.e., length, area, or volume).
#' @param HSIfunc is a function for combination of the suitability indices.
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
#' @export
HUcalc <- function(SI.out, habitat.quantity, HSIfunc,...){
  # Create an empty vector to store outputs
  HU.out <- as.data.frame(matrix(NA,nrow=1,ncol=3))
  colnames(HU.out) <- c("Quality", "Quantity", "IndexUnits")

  # Compute outputs
  HU.out$Quality <- HSIfunc(SI.out,...)
  HU.out$Quantity <- habitat.quantity
  HU.out$IndexUnits <- HU.out$Quality * HU.out$Quantity

  # Return habitat summary
  return(HU.out)
}
