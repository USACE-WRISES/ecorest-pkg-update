#' Habitat Suitability Index with Geometric Mean
#'
#' \code{HSIgeomean} uses a geometric mean to combine suitability indices into an
#'   overarching habitat suitability index. Note that U.S. Army Corps of 
#'   Engineers users applying the HSIgeomean function must have approval from 
#'   the National Ecosystem Planning Center of Expertise (Eco-PCX) prior to 
#'   development or application of a new model.
#'
#' @param x a vector, matrix, or data frame of suitability indices with values ranging from 0 to 1.
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
#' HSIgeomean(c(0.25, 0.25, 0.25, 0.25))
#'
#' #Determine patch quality based on a vector of suitability indices with an NA.
#' HSIgeomean(c(0.25, 0.25, NA, 0.25))
#'
#' #Determine patch quality based on a vector of suitability indices with a zero-value.
#' HSIgeomean(c(0.25, 0.25, 0.0, 0.25))
#' 
#' #Determine patch quality based on a dataframe of suitability indices
#' x = data.frame(0.25, 0.25, 0.25, 0.25)
#' colnames(x) = c("var1", "var2", "var3", "var4")
#' HSIgeomean(x)
#'
#' @export
HSIgeomean <- function(x){
  warning("U.S. Army Corps of Engineers users must have approval from the National Ecosystem 
  Planning Center of Expertise (Eco-PCX) prior to development or application of a new model.")
  
  # Convert all inputs to vectors
  x = unlist(x)
  
  # Test whether x is valid
  if (any(is.infinite(x) | !is.numeric(x))) {
    stop("Non-NA inputs must be finite numeric values.")
  }
  
  # Test whether numeric inputs are between zero and one
  if (any(x < 0 | x > 1, na.rm = TRUE)) {
    stop("Suitability indices must be between 0 and 1.", call. = FALSE)
  }
  
  
  HSI <- prod(x, na.rm=TRUE)^(1/length(which(is.na(x)!=TRUE)))
  
 if(HSI < 0 | HSI > 1){
    stop( "Habitat suitability index is not numeric or is not within 0 to 1 range.", call. = FALSE)
  } else {
    HSIout <- HSI
  }
  
  # Return HSI outcome
  return(HSIout)
}
