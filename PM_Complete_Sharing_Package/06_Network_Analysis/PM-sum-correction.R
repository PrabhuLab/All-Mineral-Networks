
# TASK: Batch correction of PM sums across all removal experiments
# Count distinct parent paragenetic modes
# Preserve all original mineral attributes
# Preserve original CSV files


# STEP 1: Set the main project folder

project_folder <- "/Users/lindsaypodjasek/Documents/datathon/all mineral network"


# STEP 2: Identify folders containing modified datasets

# Experiment 1: 100 independent removals
folder_3mode <- file.path(
  project_folder,
  "Remove-3PM-100x",
  "PM_Removal_Iterations"
)

# Experiment 2: Original 3 progressive removal repetitions
folder_5percent <- file.path(
  project_folder,
  "Remove-PM-5-percent"
)

# Experiment 3: 10 repetitions stopping at 50%
folder_50percent <- file.path(
  folder_5percent,
  "Remove-PM-5-percent-50pct"
)


# STEP 3: Create a function to correct one CSV file

correct_pm_sum <- function(input_file) {
  
  # Import the dataset
  data <- read.csv(
    input_file,
    check.names = FALSE
  )
  
  # Verify the required column exists
  if (!"PM sum for mineral" %in% names(data)) {
    stop(paste("Missing PM sum column:", input_file))
  }
  
  # Identify remaining paragenetic mode columns
  pm_columns <- grep(
    "^p[0-9]+[a-z]*$",
    names(data),
    value = TRUE
  )
  
  # Identify distinct parent modes
  parent_modes <- sub("[a-z]+$", "", pm_columns)
  
  # Preserve the original PM sums
  if (!"Original_PM_Sum" %in% names(data)) {
    data$Original_PM_Sum <- data$`PM sum for mineral`
  }
  
  # Calculate updated sums
  if (nrow(data) == 0 || length(pm_columns) == 0) {
    
    updated_pm_sum <- integer(nrow(data))
    
  } else {
    
    mode_matrix <- as.matrix(
      data[, pm_columns, drop = FALSE]
    )
    
    mode_matrix <- matrix(
      as.numeric(mode_matrix),
      nrow = nrow(data),
      dimnames = dimnames(mode_matrix)
    )
    
    updated_pm_sum <- apply(
      mode_matrix,
      1,
      function(row) {
        
        occupied <- !is.na(row) & row == 1
        
        length(unique(parent_modes[occupied]))
      }
    )
  }
  
  # Compare against original sums
  original_sum <- suppressWarnings(
    as.numeric(as.character(data$Original_PM_Sum))
  )
  
  increased <- sum(
    updated_pm_sum > original_sum,
    na.rm = TRUE
  )
  
  decreased <- sum(
    updated_pm_sum < original_sum,
    na.rm = TRUE
  )
  
  unchanged <- sum(
    updated_pm_sum == original_sum,
    na.rm = TRUE
  )
  
  # Stop if any PM sums unexpectedly increase
  if (increased > 0) {
    stop(paste(
      "Unexpected PM sum increase in:",
      input_file
    ))
  }
  
  # Update the PM sum column
  data$`PM sum for mineral` <- updated_pm_sum
  
  # Generate corrected filename
  output_file <- sub(
    "[.]csv$",
    "_Corrected.csv",
    input_file,
    ignore.case = TRUE
  )
  
  # Never overwrite an existing corrected file
  if (file.exists(output_file)) {
    stop(paste(
      "Corrected file already exists:",
      output_file
    ))
  }
  
  # Export the corrected dataset
  write.csv(
    data,
    output_file,
    row.names = FALSE
  )
  
  # Return correction statistics
  data.frame(
    Input_File = input_file,
    Output_File = output_file,
    Minerals = nrow(data),
    Increased = increased,
    Decreased = decreased,
    Unchanged = unchanged,
    stringsAsFactors = FALSE
  )
}


# STEP 4: Collect all modified CSV files

# 100 independent-removal datasets
files_3mode <- file.path(
  folder_3mode,
  sprintf("PM_Remove_%03d.csv", 1:100)
)

# Original progressive removal repetition 01
files_rep01 <- file.path(
  folder_5percent,
  "Repetition-01",
  "Remove-PM-5-percent-OUTPUTS",
  sprintf("PM_5Percent_Step_%02d.csv", 1:44)
)

# Original progressive removal repetitions 02 and 03
files_rep02_03 <- unlist(
  lapply(2:3, function(run) {
    
    file.path(
      folder_5percent,
      sprintf("Repetition-%02d", run),
      sprintf("PM_5Percent_Step_%02d.csv", 1:44)
    )
  }),
  use.names = FALSE
)

# 10 repetitions stopping at 50%
files_50percent <- unlist(
  lapply(1:10, function(run) {
    
    file.path(
      folder_50percent,
      sprintf("Repetition-%02d", run),
      sprintf("PM_5Percent_Step_%02d.csv", 1:14)
    )
  }),
  use.names = FALSE
)

# Combine all input filenames
all_files <- c(
  files_3mode,
  files_rep01,
  files_rep02_03,
  files_50percent
)

# Verify all expected files exist
missing_files <- all_files[!file.exists(all_files)]

if (length(missing_files) > 0) {
  
  print(missing_files)
  
  stop("Some input files are missing. No corrections started.")
}

print(paste("Input files found:", length(all_files)))


# STEP 5: Check for existing corrected output files

corrected_paths <- sub(
  "[.]csv$",
  "_Corrected.csv",
  all_files,
  ignore.case = TRUE
)

existing_outputs <- corrected_paths[file.exists(corrected_paths)]

if (length(existing_outputs) > 0) {
  
  print(existing_outputs)
  
  stop(
    "Corrected files already exist. Move or rename them before running."
  )
}


# STEP 6: Correct every modified dataset

correction_results <- list()

for (i in seq_along(all_files)) {
  
  correction_results[[i]] <- correct_pm_sum(
    all_files[i]
  )
  
  print(paste(
    "Corrected",
    i,
    "of",
    length(all_files)
  ))
}


# STEP 7: Combine correction statistics

correction_log <- do.call(
  rbind,
  correction_results
)

# Verify that no PM sums increased
stopifnot(all(correction_log$Increased == 0))


# STEP 8: Export the master correction log

log_file <- file.path(
  project_folder,
  "PM_Sum_Correction_Log.csv"
)

if (file.exists(log_file)) {
  stop("Correction log already exists. Existing log was not overwritten.")
}

write.csv(
  correction_log,
  log_file,
  row.names = FALSE
)


# STEP 9: Display final results

print(paste(
  "Total datasets corrected:",
  nrow(correction_log)
))

print(paste(
  "Total decreased PM sums:",
  sum(correction_log$Decreased)
))

print(paste(
  "Total increased PM sums:",
  sum(correction_log$Increased)
))

print("Batch PM sum correction complete!")

View(correction_log)
