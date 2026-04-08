#' Habitat Suitability Index with Limiting Factors
#'
#' \code{HSImin} uses the minimum of given suitability indices to calculate an
#'   overarching habitat suitability index. Note that U.S. Army Corps of 
#'   Engineers users applying the HSImin function must have approval from the 
#'   National Ecosystem Planning Center of Expertise (Eco-PCX) prior to 
#'   development or application of a new model.
#'
#' @param x a vector, matrix, or data frame of suitability indices ranging from 0 to 1.
#'
#' @return A value of habitat quality ranging from 0 to 1 (ignoring NA values).
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
#' #Determine patch quality based on a vector of four suitability indices.
#' HSImin(c(0.1, 0.25, 0.25, 0.25))
#'
#' #Determine patch quality based on a vector of suitability indices with an NA.
#' HSImin(c(0.1, 0.25, NA, 0.25))
#' 
#' #Determine patch quality based on a data frame of suitability indices
#' x = data.frame(0.1, 0.25, 0.25, 0.25)
#' colnames(x) = c("var1", "var2", "var3", "var4")
#' HSImin(x)
#'
#' @export
HSImin <- function(x){
  warning("U.S. Army Corps of Engineers users must have approval from the National Ecosystem 
  Planning Center of Expertise (Eco-PCX) prior to development or application of a new model.")
  HSI <- min(x, na.rm=TRUE)
  
  if (any(x < 0 | x > 1, na.rm = TRUE)) {
    stop("Suitability indices must be between 0 and 1.", call. = FALSE)
  } else if(HSI < 0 | HSI > 1){
    stop("Habitat suitability index not within 0 to 1 range.", call. = FALSE)
  } else {
    HSIout <- HSI
  }
  
  # Return HSI outcome
  return(HSIout)
}
