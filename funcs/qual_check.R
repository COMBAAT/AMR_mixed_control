
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

#-------------------------------------------------------------------------------
# Function Name: qual_check_no0
#
# Parameters:
#   input - A vector or other data structure containing values to be checked.
#
# Outputs:
#   Produces console messages warning if any values are less than zero.
#   The function is designed to identify and warn about negative values in the input data,
#   providing a straightforward audit of non-positive values within a given dataset.
#
# Dependencies:
#   crayon (for colored messaging)
#
#-------------------------------------------------------------------------------
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
  
  # This part of the code is commented out; if activated, it checks if all entries are non-negative
  # and prints a confirmation message. Uncomment the following line to enable this feature.
  # if(sum(test[!is.na(test)]) == length(input)){cat(green("CHECK OK\n"))}
}

# This code uses the 'findGlobals' function from the 'codetools' package to analyze and list
# global variables and other dependencies that are not explicitly passed as arguments to the function,
# ensuring all necessary components are declared and managed properly within the function scope.
findGlobals(fun = qual_check_no0, merge = FALSE)$variables
