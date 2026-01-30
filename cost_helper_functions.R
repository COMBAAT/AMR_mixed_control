

convert_array_to_named_vector <- function(this_array) {
  names <- colnames((this_array))
  this_vector <- as.vector(this_array)
  names(this_vector) <- names
  this_vector
}


set_days_per_year <- function() {
  days_per_year <- 365.25
  days_per_year
}



