#' Habitat Suitability Index with Geometric Mean
#'
#' \code{HSIgeomen} uses geometric mean to combine suitability indices into an
#'   overarching habitat suitability index.
#'
#' @param x a vector of suitability indices
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
#' HSIgeomean(c(0.25, 0.25, 0.25, 0.25))
#'
#' #Determine patch quality based on a vector of suitability indices with an NA.
#' HSIgeomean(c(0.25, 0.25, NA, 0.25))
#'
#' #Determine patch quality based on a vector of suitability indices with a zero-value.
#' HSIgeomean(c(0.25, 0.25, 0.0, 0.25))
#'
#' #Demonstrate error message associated with out of range outcomes.
#' HSIgeomean(c(2, 2, NA, 3))
#'
#' @export
HSIgeomean <- function(x){
  HSI <- prod(x, na.rm=TRUE)^(1/length(which(is.na(x)!=TRUE)))

  if(HSI < 0 | HSI > 1){
    HSIout <- "Habitat suitability index not within 0 to 1 range."
  } else {
    HSIout <- HSI
  }

  # Return HSI outcome
  return(HSIout)
}
