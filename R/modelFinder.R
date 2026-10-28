#' Returns models matching user-input species' common names
#'
#' \code{modelFinder} returns model metadata including model name, submodel description, 
#' and included SIV variables for models in HSImetadata that contain a string matching 
#' a user-entered species' common or scientific name. Note that models compatible with modelFinder 
#' are certified for use by the U.S. Army Corps of Engineers by the National Ecosystem 
#' Planning Center of Expertise (Eco-PCX).
#'
#' @param species a character string in quotations that must match the common or scientific 
#' name of a species included in an existing model in HSImetadata. Species' common names 
#' can be written exactly as they appear in HSImetadata (e.g., barredowl) but will also
#' be recognized with spaces or capitalization (e.g., barred owl or Barred Owl).
#' @param type a character string equal to "commmon" or "scientific" denoting whether
#' the user-entered species parameter is a common name or scientific name.
#' @param HSImetadata a data frame of HSI model metadata within the ecorest package.
#'
#' @return a summary of any models in HSImetadata containing a string that matches
#' the user-entered common or scientific name, including the model name, species, 
#' a submodel description, and the variables included in the model.
#'
#' @examples
#'
#' #Find models for turkey
#' modelFinder(species = "turkey", type = "common", HSImetadata = HSImetadata) # common name
#' modelFinder(species = "Meleagris gallopavo sylvestris", type = "scientific", HSImetadata = HSImetadata) # scientific name
#'
#' #Find models for Northern Pike
#' modelFinder(species = "northernpike", type = "common", HSImetadata = HSImetadata) # as appears in HSImetadata
#' modelFinder(species = "northern pike", type = "common", HSImetadata = HSImetadata) # with spaces
#' modelFinder(species = "Northern Pike", type = "common", HSImetadata = HSImetadata) # with spaces and capitalization
#' modelFinder(species = "Esox lucius", type = "scientific", HSImetadata = HSImetadata) # scientific name
#'
#'
#' @export
modelFinder <- function(species, type = "common", HSImetadata){
  
  if (!type %in% c("common", "scientific")) {
    stop(
      "Invalid type. Please set type to 'common' for common names or 'scientific' for
      scientific names",
      call. = FALSE
    )
  }
  
  # Remove spaces/tabs and standardize case before matching
  species_search <- tolower(gsub("[[:space:]]+", "", species))
  
  if (type == "common") {
    model_search <- tolower(
      gsub("[[:space:]]+", "", HSImetadata$model)
    )
  } else {
    model_search <- tolower(
      gsub("[[:space:]]+", "", HSImetadata$species)
    )
  }
  
  # Find models containing the normalized species name
  species_index <- which(
    grepl(species_search, model_search, fixed = TRUE)
  )
  
  # Stop if no models are found
  if (length(species_index) == 0) {
    stop(
      "Species was not found in HSImetadata. Please confirm spelling and type (common vs. scientific).",
      call. = FALSE
    )
  }
  
  # Store model and submodel identifiers
  submodel <- HSImetadata[
    species_index,
    c("model", "species", "submodel"),
    drop = FALSE
  ]
  
  # Isolate only potential SIV-variable columns
  metadata_SIVs <- HSImetadata[
    species_index,
    9:40,
    drop = FALSE
  ]
  
  model_variables <- lapply(
    seq_len(nrow(metadata_SIVs)),
    function(i) {
      variables <- unlist(
        lapply(metadata_SIVs[i, , drop = FALSE], as.character),
        use.names = FALSE
      )
      
      variables[!is.na(variables) & variables != ""]
    }
  )
  
  names(model_variables) <- paste(
    submodel$model,
    submodel$submodel,
    sep = " — "
  )
  
  # Print model summaries
  cat(paste0("The following models are available in ecorest for ", species, 
               ". Please review the included submodel information and variables to select the most appropriate model.\n"))
  
  for (i in seq_along(model_variables)) {
    
    cat("\nModel:    ", submodel$model[i], "\n", sep = "")
    cat("\nSpecies:    ", submodel$species[i], "\n", sep = "")
    cat("Submodel: ", submodel$submodel[i], "\n", sep = "")
    cat("Variables:\n")
    cat(paste0("  - ", model_variables[[i]]), sep = "\n")
    cat("\n", strrep("-", 60), "\n", sep = "")
  }
}
