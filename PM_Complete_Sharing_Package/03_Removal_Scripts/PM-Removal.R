# Import paragenetic mode dataset
data <- read.csv(file.choose(), check.names = FALSE)
# Identify paragenetic mode columns
pm_columns <- grep("^p[0-9]", names(data), value = TRUE)

# Display the paragenetic mode columns
print(pm_columns)

# Randomly select 3 paragenetic modes
removed_modes <- sample(pm_columns, 3)

# Display which modes were selected
print(removed_modes)

# Remove the 3 randomly selected paragenetic modes
data_modified <- data[, !names(data) %in% removed_modes]

# View the modified dataset
View(data_modified)

# Export the first iteration of random PM removal
# Set the folder for all PM removal iterations
output_folder <- "/Users/lindsaypodjasek/Documents/datathon/all mineral network/PM_Removal_Iterations"

# Export the first iteration
write.csv(
  data_modified,
  file.path(output_folder, "PM_Remove_001.csv"),
  row.names = FALSE
)

# Create an empty table to record removed modes and minerals lost
removal_log <- data.frame(
  Iteration = integer(),
  Removed_Mode_1 = character(),
  Removed_Mode_2 = character(),
  Removed_Mode_3 = character(),
  Minerals_Lost = integer()
)
# Run 100 independent random removal iterations
for (i in 1:100) {
  
  # Randomly select 3 paragenetic modes
  removed_modes <- sample(pm_columns, 3)
  
  # Remove those modes from the ORIGINAL dataset
  data_modified <- data[, !names(data) %in% removed_modes]
  
  # Identify the paragenetic modes that remain
  remaining_modes <- setdiff(pm_columns, removed_modes)
  
  # Find minerals with at least one remaining mode association
  has_mode <- rowSums(
    data_modified[, remaining_modes, drop = FALSE] == 1,
    na.rm = TRUE
  ) > 0
  
  # Exclude the two non-mineral summary rows
  is_mineral <- !(data_modified$PM %in% c("PM Description", "PM sum"))
  
  # Keep only minerals with remaining mode associations
  data_modified <- data_modified[has_mode & is_mineral, ]
  
  # Count minerals lost compared to the original network
  original_minerals <- sum(
    !(data$PM %in% c("PM Description", "PM sum"))
  )
  
  minerals_lost <- original_minerals - nrow(data_modified)
  
  # Create a unique filename
  filename <- sprintf("PM_Remove_%03d.csv", i)
  
  # Export the modified dataset
  write.csv(
    data_modified,
    file.path(output_folder, filename),
    row.names = FALSE
  )
 
  # Add count to removal log 
  removal_log[i, ] <- list(
    i,
    removed_modes[1],
    removed_modes[2],
    removed_modes[3],
    minerals_lost
  )
  
  # Display progress
  print(paste("Iteration", i, "saved as", filename))
  print(removed_modes)
}

# Export the removal log
write.csv(
  removal_log,
  file.path(output_folder, "PM_Removal_Log.csv"),
  row.names = FALSE
)

# View the removal log
View(removal_log)