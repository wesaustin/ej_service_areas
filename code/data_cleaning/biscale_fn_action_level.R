# GENERATE BI CLASS OBJECT, WITH ACTION LEVELS
# X <-- VARIABLE WITH THE ACTION LEVEL
# ACTION_VECTOR <- A VECTOR OF TWO NUMBERS (CAN BE MODIFIED)


bi_class_al <- function(.data, x, y, style = "quantile", dim = 3, keep_factors = FALSE,
                        action_level = T, action_vector){
  
  # global bindings
  bi_x = bi_y = NULL
  
  # check inputs
  if (missing(.data)) {
    stop("An object containing data must be specified for the '.data' argument.")
  }
  
  # check data
  #  if ("sf" %in% class(.data) == TRUE & "sf" %in% (.packages()) == FALSE){
  #    warning("The 'sf' package is not loaded, and the class 'sf' attribute of the given data set has been lost. Load 'sf' to retain the class when using 'bi_class'.")
  #  }
  
  # check inputs
  if (missing(x)) {
    stop("A variable must be given for the 'x' argument.")
  }
  
  if (missing(y)) {
    stop("A variable must be given for the 'y' argument.")
  }
  
  if (style %in% c("quantile", "equal", "fisher", "jenks") == FALSE){
    stop("The allowed styles are 'equal', 'fisher', 'jenks', or 'quantile'.")
  }
  
  if (is.numeric(dim) == FALSE){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (dim != 2 & dim != 3){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (is.logical(keep_factors) == FALSE){
    stop("A logical scalar must be supplied for 'keep_factors'. Please provide either 'TRUE' or 'FALSE'.")
  }
  
  # save parameters to list
  paramList <- as.list(match.call())
  
  # nse
  if (!is.character(paramList$x)) {
    xQ <- rlang::enquo(x)
  } else if (is.character(paramList$x)) {
    xQ <- rlang::quo(!! rlang::sym(x))
  }
  
  xQN <- rlang::quo_name(rlang::enquo(x))
  
  if (!is.character(paramList$y)) {
    yQ <- rlang::enquo(y)
  } else if (is.character(paramList$y)) {
    yQ <- rlang::quo(!! rlang::sym(y))
  }
  
  yQN <- rlang::quo_name(rlang::enquo(y))
  
  # check variables
  if (xQN %in% names(.data) == FALSE){
    stop(glue::glue("The given 'x' variable '{var}' is not found in the given data set.",
                    var = xQN))
  }
  
  if (yQN %in% names(.data) == FALSE){
    stop(glue::glue("The given 'y' variable '{var}' is not found in the given data set.",
                    var = yQN))
  }
  
  # create three bins for x and y
  bins_x <- dplyr::pull(.data, !!xQ)
  bins_y <- dplyr::pull(.data, !!yQ)
  
  # calculate breaks
  if (style == "quantile"){
    
    bins_x <- classInt::classIntervals(bins_x, n = dim, style = "quantile")
    bins_y <- classInt::classIntervals(bins_y, n = dim, style = "quantile")
    
  } else if (style == "equal"){
    
    bins_x <- classInt::classIntervals(bins_x, n = dim, style = "equal")
    bins_y <- classInt::classIntervals(bins_y, n = dim, style = "equal")
    
  } else if (style == "fisher"){
    
    bins_x <- classInt::classIntervals(bins_x, n = dim, style = "fisher")
    bins_y <- classInt::classIntervals(bins_y, n = dim, style = "fisher")
    
  } else if (style == "jenks"){
    
    bins_x <- classInt::classIntervals(bins_x, n = dim, style = "jenks")
    bins_y <- classInt::classIntervals(bins_y, n = dim, style = "jenks")
    
  }
  
  # convert to breaks
  if (action_level == TRUE){
    max_x <- max(dplyr::pull(.data, !!xQ), na.rm = T)
    bins_x <- c(0, action_vector, max_x)
    bins_y <- bins_y$brks
    
  } else {
    bins_x <- bins_x$brks
    bins_y <- bins_y$brks
  }
  
  
  # cut into groups defined above
  out <- dplyr::mutate(.data, bi_x = cut(!!xQ, breaks = bins_x, include.lowest = TRUE))
  out <- dplyr::mutate(out, bi_y = cut(!!yQ, breaks = bins_y, include.lowest = TRUE))
  out <- dplyr::mutate(out, bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
  
  # optionally remove factors
  if (keep_factors == FALSE){
    out <- dplyr::select(out, -c(bi_x, bi_y))
  }
  
  # return output
  return(out)
  
}
