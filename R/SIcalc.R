#' Computes Suitability Indices
#'
#' \code{SIcalc} computes suitability indices given a set of suitability curves
#'   and project-specific inputs. Suitability indices may be computed based on
#'   either linear interpolation (for continuous variables)
#'   or a lookup method (for categorical variables).
#'
#' @import stats
#'
#' @param SI matrix or dataframe of suitability curves ordered as parameter breakpoints and
#'   associated suitability indices for each parameter. Suitability curves that contain
#'   both continuous and categorical variables should be formatted as a dataframe rather than a matrix.
#' @param input.proj numeric or categorical vector of application-specific input
#'   parameters associated with the suitability curve data from SI. Categorical
#'   inputs are denoted with lowercase letters and should be entered in quotations 
#'   (e.g., "a", "b"). Users may need to review the model documentation to determine the 
#'   appropriate category. Model-specific curves can be viewed with the print function 
#'   (e.g., print(HSImodels$alewifeJuvAndSAEL)). Note that users can enter NA 
#'   for excluded variables in HSImodels, but this is not a requirement.
#'
#' @return A named vector of suitability index values ranging from 0 to 1 that match 
#' given user inputs. Outputs are displayed with two decimal places, but returned 
#' numeric values retain full precision for subsequent calculations. Users should 
#' consider the appropriate number of significant digits to retain when reporting results.
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
#' #Build and define a matrix of the Barred Owl suitability curves
#' #Allen A.W. 1982. Habitat Suitability Index Models: Barred owl. FWS/OBS 82/10.143.
#' #U.S. Fish and Wildlife Service. https://pubs.er.usgs.gov/publication/fwsobs82_10_143.
#' var1 <- cbind(c(0,2,4,NA), c(0.1,1,1,NA)) #Number of trees > 51cm diameter per 0.4 ha plot
#' var2 <- cbind(c(0,13,51,NA), c(0,0,1,NA)) #Mean diameter of overstory trees
#' var3 <- cbind(c(0,20,60,100), c(0,0,1,NA)) #Percent canopy cover of overstory trees
#' barredowl <- cbind(var1, var2, var3)
#' colnames(barredowl)<- c("tree.num", "tree.num.SIV",
#'   "avg.dbh.cm", "avg.dbh.SIV", "can.cov", "can.cov.SIV")
#'
#' #Set user input variables (in cm) that should return (1, 0, 0)
#' input.demo1 <- c(2, 0, 0)
#' SIcalc(barredowl, input.demo1)
#'
#' #Set user input variables (in cm) that should return (1, 1, 1)
#' input.demo2 <- c(4, 51, 60)
#' SIcalc(barredowl, input.demo2)
#'
#' #Set user input variables (in cm) that should return (1, 1, 0.5)
#' input.demo3 <- c(4, 51, 40)
#' SIcalc(barredowl, input.demo3)
#'
#' #Suitability curves may also be drawn from HSImodels (data within ecorest)
#' #Import Barred Owl suitability curves with HSImodels$barredowl
#' #The input examples are repeated from above
#'
#' #Set user input variables that should return (1, 0, 0)
#' SIcalc(HSImodels$barredowl, input.demo1)
#'
#' #Set user input variables that should return (1, 1, 1)
#' SIcalc(HSImodels$barredowl, input.demo2)
#'
#' #Set user input variables that should return (1, 1, 0.5)
#' SIcalc(HSImodels$barredowl, input.demo3)
#'
#' 
#' #Import juvenile Alewife suitability curves with HSImodels$alewifeJuv
#' #Demonstrate inclusion and exclusion of NA for excluded variables in HSImodels
#' #Pardue, GB. 1983. Habitat suitability index models: alewife and blueback herring. 
#' #U.S. Dept. Int. Fish Wildl. Serv. FWS/OBS-82/10.58. 22pp.
#' 
#' #Set user variables that should return (1, 1, 0)
#' input.demo4 <- c(NA, NA, 125, 5, 5)
#' SIcalc(HSImodels$alewifeJuv, input.demo4)
#' 
#' #Demonstrate use without NA inputs (should return 1, 1, 0)
#' input.demo5 <- c(125, 5, 5)
#' SIcalc(HSImodels$alewifeJuv, input.demo5)
#' 
#' #Demonstrate use with out of range continuous inputs (should return 1, 1, 0 with a warning)
#' input.demo6 <- c(140, 5, 5)
#' SIcalc(HSImodels$alewifeJuv, input.demo6)
#'
#' @export
SIcalc <- function(SI, input.proj){
  
  # If any factors are entered, convert to character before unlisting
  if (is.data.frame(input.proj) || is.list(input.proj)) {
    input.proj[vapply(input.proj, is.factor, logical(1))] <-
      lapply(input.proj[vapply(input.proj, is.factor, logical(1))], as.character)
  }
  
  # Standardize user input to a simple unnamed vector
  # This allows the function to work whether input.proj is vector or one row data frame
  input.proj <- unlist(input.proj, use.names = TRUE)
  
  # Store original SI structure
  og_SI = SI
  
  # Store user-input names if they exist
  og_names = names(input.proj)
  
  # Number of variables in HSImodels, including NA variables
  nSI <- ncol(SI) / 2
  
  # Number of non-NA variables in HSImodels
  non_NA_SI <- sum(colSums(!is.na(SI)) > 0) / 2
  
  # Stop if number of inputs does not match number of SI variables.
  if(length(input.proj) != nSI & length(input.proj) != non_NA_SI) {
    stop("Number of inputs does not equal number of SI values.", call. = FALSE)
  } else if(length(input.proj) == nSI & length(input.proj) != non_NA_SI) {
    # If NAs are included in the input, assign a value in input.proj to every variable column in HSImodels
    names(input.proj) = names(SI)[seq(2, ncol(SI), by = 2)]
    
    # Find all columns in SI where every row is NA
    all_na_cols <- names(SI)[vapply(SI, function(x) all(is.na(x)), logical(1))]
    
    # Remove NA columns from SI
    SI <- SI[!(names(SI) %in% all_na_cols)]
    
    # Delete any NA inputs where the data in SI is all NA
    remove_idx <- names(input.proj) %in% all_na_cols # Store position of values that will be removed
    input.proj <- input.proj[!(names(input.proj) %in% all_na_cols)] # Remove values
    
    # If the user input names, remove the names corresponding to the removed parameters
    if (!is.null(og_names)) {
      og_names <- og_names[!remove_idx]
    }
    
    # Count number of NAs removed
    na_num = nSI - non_NA_SI
    
    if (na_num == 1) {
      message(
        paste0(na_num),
        " parameter entered as NA and not found in SI was removed from calculations.\n"
      )
    } else {
      message(
        paste0(na_num),
        " parameters entered as NA and not found in SI were removed from calculations.\n"
      )
    }
    
    
  } else if (length(input.proj) == non_NA_SI & length(input.proj) != nSI) {
    
    # Find all columns in SI where every row is NA
    all_na_cols <- names(SI)[vapply(SI, function(x) all(is.na(x)), logical(1))]
    
    # Remove NA columns from SI
    SI <- SI[!(names(SI) %in% all_na_cols)]
  }
  
  # Check that all suitability indices in SI are between 0 and 1
  even_cols <- seq(2, ncol(SI), by = 2)
  if(any(SI[, even_cols] < 0 | SI[, even_cols] > 1, na.rm = TRUE)){
    stop("Suitability index values in SI must be between 0 and 1.", call. = FALSE)
  }
    # Check for infinite inputs (numeric +/-Inf and character "Inf"/"-Inf")
  if(any(is.infinite(suppressWarnings(as.numeric(input.proj))), na.rm = TRUE) ||
      any(as.character(input.proj) %in% c("Inf", "-Inf"), na.rm = TRUE)) {
    stop("input.proj contains infinite values (Inf or -Inf), which are not allowed.", call. = FALSE)
  }
  # Pre-allocate output.
  SI.out <- rep(NA_real_, non_NA_SI)
  capped_any <- FALSE
  
  # Loop over variables.
  for(i in 1:non_NA_SI){
    
    # Breakpoint/class column and associated SIV column.
    xcol <- SI[,2*i - 1]
    ycol <- SI[, 2*i]
    
    # User input for this variable.
    x <- input.proj[i]
    
    # If the model variable is excluded, return NA.
    if(all(is.na(xcol)) && all(is.na(ycol))){
      SI.out[i] <- NA
      next
    }
    
    # If the user input is NA, return NA.
    if(is.na(x) || identical(as.character(x), "NA")){
      SI.out[i] <- NA
      next
    }
    
    # Try to interpret breakpoint column as numeric.
    xcol_num <- suppressWarnings(as.numeric(as.character(xcol)))
    ycol_num <- suppressWarnings(as.numeric(as.character(ycol)))
    x_num    <- suppressWarnings(as.numeric(as.character(x)))
    
    # Treat as continuous if the breakpoint column contains usable numeric values.
    is_cont <- any(!is.na(xcol_num))
    
    if(is_cont){
      keep <- !is.na(xcol_num) & !is.na(ycol_num)
      
      min_x <- min(xcol_num[keep], na.rm = TRUE)
      max_x <- max(xcol_num[keep], na.rm = TRUE)
      
      if(is.na(x_num)){
        stop(
          paste0("Input for variable ", i, " must be numeric for a continuous suitability curve."),
          call. = FALSE
        )
      }
      
      if(x_num < min_x || x_num > max_x){
        capped_any <- TRUE
      }
      
      SI.out[i] <- approx(
        x = xcol_num[keep],
        y = ycol_num[keep],
        xout = x_num,
        method = "linear",
        rule = 2,
        ties = "ordered"
      )$y
      
    } else {
      allowed <- unique(as.character(xcol))
      allowed <- allowed[!is.na(allowed)]
      
      if(!(as.character(x) %in% allowed)){
        stop("Values in input.proj must fall within the ranges provided in SI.", call. = FALSE)
      }
      
      idx <- which(as.character(xcol) == as.character(x))
      
      if(length(idx) == 0){
        stop(
          paste0(
            "No categorical match found for variable ", i,
            " with input value: ", as.character(x)
          ),
          call. = FALSE
        )
      }
      
      SI.out[i] <- suppressWarnings(as.numeric(as.character(ycol[idx[1]])))
    }
  }

  # Determine whether SI is a blue book model stored in HSImodels and HSImetdata
  if (any(vapply(HSImodels, identical, logical(1), y = og_SI))) {
    # If true, pull SIV names based on column names in SI
    model = names(which(vapply(HSImodels, identical, logical(1), y = og_SI))) # Pull name of target model
    model_info = HSImetadata[which(HSImetadata$model == model[1]),9:40, drop = FALSE] # Store the model's SIV metadata
    SIV_names = names(HSImetadata)[9:40][model_info %in% names(og_SI)] # Store names of SIVs
    names(SI.out) = SIV_names # Set SIV names as the names of input.proj
    
    if (!is.null(og_names) & !identical(og_names, names(SI.out)) & length(model) == 1) {
      message(
        "SI was found in HSImodels (",
        paste(model),
        "), so parameter names were altered ",
        "to match HSImetadata for compatibility with HSIeqtn:\n",
        "User-input parameter names: ", paste(og_names, collapse = ", "), "\n",
        "Parameter names in HSImetadata: ", paste(names(SI.out), collapse = ", ")
      )
    } else if (!is.null(og_names) & !identical(og_names, names(SI.out)) & length(model) == 1) {
      message(
        "SI matched several models in HSImodels (",
        paste(model, collapse = ", "),
        "), so parameter names were altered ",
        "to match HSImetadata for compatibility with HSIeqtn:\n",
        "User-input parameter names: ", paste(og_names, collapse = ", "), "\n",
        "Parameter names in HSImetadata: ", paste(names(SI.out), collapse = ", ")
      )
    } else if (length(model) > 1) {
      message(
        "SI matched several models in HSImodels: ",
        paste(model, collapse = ", ")
      )
    }
  } else {
    if (!is.null(og_names)) {
      names(SI.out) <- og_names
    } else {
      names(SI.out) <- paste0("SIV", seq_len(non_NA_SI))
    }
    
    warning(
      "U.S. Army Corps of Engineers users must have approval from the National Ecosystem Planning Center of Expertise (Eco-PCX) prior to application of a new model.",
      call. = FALSE
    )
  }
  
  
  if(capped_any){
    warning(
      "Numeric values outside the suitability curve range were capped at the nearest minimum or maximum breakpoint.",
      call. = FALSE
    )
  }
  
  # Return HSI outcome: display two decimal places, retain full precision
  writeLines(sprintf("%s: %.2f\n", names(SI.out), SI.out))
  return(invisible(SI.out))
}
