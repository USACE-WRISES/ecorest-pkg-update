#' Habitat Suitability Index with a Weighted Arithmetic Mean
#'
#' \code{HSIwarimean} uses a weighted arithmetic mean to combine suitability
#'   indices into an overarching habitat suitability index.
#'
#' @param x is a vector of suitability indices ranging from 0 to 1.
#' @param w is a vector of weights (0 to 1 values that must sum to one).
#'
#' @return A value of habitat quality from 0 to 1 ignoring NA values.
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
#' #Demonstrate error for mismatching inputs.
#' HSIwarimean(c(1, 0, 0, 0), c(0, 0, 0))
#'
#' #Demonstrate error for incorrect weighting.
#' HSIwarimean(c(1, 0, 0, 0), c(1, 1, 0, 0))
#'
#' #Demonstrate error for out of range output.
#' HSIwarimean(c(1, 1, 1, 10), c(0.2, 0.3, 0.3, 0.2))
#'
#' @export
HSIwarimean <- function(x, w){
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
