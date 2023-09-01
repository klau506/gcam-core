### ========================================
# strings with brakets

# Initialize an empty character vector to store the strings
output <- character(0)

# Loop through i from 1 to 25
for (i in 1:25) {
  # Append the string to the output vector
  output <- c(output, paste0("\\code{food_flexitarian_", i, ".xml},"))

  # Check if every 4 i's have been processed
  if (i %% 4 == 0) {
    # Add a new line if it's every 4th iteration
    output <- c(output, "\n#' ")
  }
}

# Combine the output vector into a single string
result <- paste(output, collapse = "")

# Print the result
cat(result)




### ========================================
#no brakets strings

# Initialize an empty character vector to store the strings
output <- character(0)

# Loop through i from 1 to 25
for (i in 1:25) {
  # Append the string to the output vector
  output <- c(output, paste0("food_flexitarian_", i, ".xml,"))

  # Check if every 4 i's have been processed
  if (i %% 4 == 0) {
    # Add a new line if it's every 4th iteration
    output <- c(output, "\n")
  }
}

# Combine the output vector into a single string
result <- paste(output, collapse = "")

# Print the result
cat(result)
