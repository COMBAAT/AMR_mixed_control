library(codetools)

find_nearest_vector <- function(desired_vector, actual_vector) {
  selected_vector <- c()
  for (element in desired_vector) {
    diff <- abs(actual_vector - element)
    selected_element <- actual_vector[which.min(diff)]
    selected_vector <- c(selected_vector, selected_element)
  }
  selected_vector
}

examine_df <- function(df) {
  print(unique(df$treatment_type))
  print(paste0("length treat_prop = ", length(df$treat_prop)))
  print(paste0("length proph_ongoing = ", length(df$proph_ongoing)))
  print(unique(df$Vector_total_final))
}



findGlobals(fun = find_nearest_vector, merge = FALSE)$variables