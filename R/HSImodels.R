#' Habitat suitability index (HSI) models
#'
#' This list of data frames contains 351 U.S. Fish and Wildlife Service
#'  Habitat suitability index (HSI) models. Please note that some of the original 
#'  HSI documents provide little reference data for constructing suitability curves;
#'  hence, some suitability curves are estimated using the authors' best judgement. 
#'  Users should always cross-reference results with the original documentation.
#'
#'  @format A list with 351 data frames each containing an HSI model with multiple
#'    independent variables and associated habitat suitability indices (a 0 to 1 value).
#'    Data represent break points in curves with linear extrapolation between.
#'    Categorical input variables are coded as letters.
#'
#' \describe{
#'   \item{variable1}{independent variable for assessing habitat suitability}
#'   \item{SIV1}{suitability index value relative to variable1}
#'   \item{...}{additional variables and suitability indices}
#' }
#' @source \url{https://pubs.usgs.gov/}
"HSImodels"
