#' Habitat Suitability Index with a Weighted Arithmetic Mean
#'
#' \code{HSIwarimean} uses a weighted arithmetic mean to combine suitability
#'   indices into an overarching habitat suitability index. Note that U.S. Army 
#'   Corps of Engineers users applying the HSIwarimean function must have 
#'   approval from the National Ecosystem Planning Center of Expertise (Eco-PCX) 
#'   prior to development or application of a new model or weighting system.
#'
#' @param x is a vector, matrix, or data frame of suitability indices ranging from 0 to 1.
#' @param w is a vector, matrix, or data frame of weights ranging from 0 to 1 that must sum to one.
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
#' #Determine patch quality based on a vector of four, equal-weight suitability indices.
#' HSIwarimean(c(1, 0, 0, 0), c(0.25, 0.25, 0.25, 0.25))
#'
#' #Determine patch quality based on a vector of four, unequal-weight suitability indices.
#' HSIwarimean(c(1, 0, 0, 0), c(1, 0, 0, 0))
#'
#' #Determine patch quality based on a vector of four, unequal-weight suitability indices.
#' HSIwarimean(c(1, 0, 0, 0), c(0, 1, 0, 0))
#' 
#' #Determine patch quality based on a data frame of four, unequal-weight suitability indices
#' df = data.frame(x = c(0.25, 0.5, 0.5, 0.5), w = c(0.25, 0.2, 0.5, 0.05))
#' HSIwarimean(df$x, df$w)
#'
#' @export
HSIwarimean <- function(x, w){
  warning("U.S. Army Corps of Engineers users must have approval from the National Ecosystem 
  Planning Center of Expertise (Eco-PCX) prior to development or application of a new model
  or weighting system.")
  
  HSI <- mean(as.matrix(x), na.rm=TRUE)
  if(length(w) != length(x)){
    stop("Number of weights does not equal number of suitability indices.", call. = FALSE)
  } else if (sum(w, na.rm=TRUE)!= 1){
    stop("The sum of weights must be 1.", call. = FALSE)
  } else if (any(x < 0 | x > 1, na.rm = TRUE)) {
    stop("Suitability indices must be between 0 and 1.", call. = FALSE)
  }  else {
    wmean <- sum(x * w, na.rm=TRUE)
  }
  if (wmean < 0 | wmean > 1){
    stop("Habitat suitability index not within 0 to 1 range.", call. = FALSE)
  }
  
  return(wmean)
}
