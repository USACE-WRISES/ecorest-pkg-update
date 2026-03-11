#' Habitat Suitability Index with Minimum
#'
#' \code{HSImin} uses the minimum of given suitability indices to calculate an
#'   overarching habitat suitability index.
#'
#' @param x a vector of suitability indices ranging from 0 to 1.
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
#' #Determine patch quality based on a vector of four suitability indices.
#' HSImin(c(0.1, 0.25, 0.25, 0.25))
#'
#' #Determine patch quality based on a vector of suitability indices with an NA.
#' HSImin(c(0.1, 0.25, NA, 0.25))
#'
#' #Demonstrate error message associated with out of range outcomes.
#' HSImin(c(2, 4, NA, 3))
#'
#' @export
HSImin <- function(x){
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
