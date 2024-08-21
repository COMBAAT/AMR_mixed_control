
# =========================================================
# Function Names: qual_check_no0, [additional functions if identified]
# Description: This script provides functions for performing quality checks on input data.
#              It includes checking for negative values and potentially other quality control measures
#              to ensure data integrity before processing.
#
# Parameters:
#   input - A vector or other data structure containing values to be checked.
#
# Returns:
#   Depending on the function, may return messages about the status of data or modify global environment settings.
#
# Example of use:
#   input_vector <- c(1, 2, -3, 4)
#   qual_check_no0(input_vector)
#
# Dependencies: Requires 'crayon' and 'codetools' packages for colorful messaging and code analysis functions.
#
# Author: Shaun Keegan & Louise Matthews
# Date Created: August 2024
# Last Modified: August 2024
# =========================================================
library(crayon)
library(codetools)

## ------------------------------------------------------ Check Inputs


qual_check_no0 <- function(input) {
  test <- c(1:length(input))
  test[] <- NA

  for (i in 1:length(input)) {
    if (input[i] < 0) {
      cat(red("WARNING: ", names(input[i]), "  LESS THAN 0\n"))
    }
  }

  for (j in 1:length(input)) {
    if (input[j] >= 0) {
      test[j] <- 1
    }
  }

  # if(sum(test[!is.na(test)]) == length(input)){cat(green("CHECK OK\n"))}
}

findGlobals(fun = qual_check_no0, merge = FALSE)$variables
