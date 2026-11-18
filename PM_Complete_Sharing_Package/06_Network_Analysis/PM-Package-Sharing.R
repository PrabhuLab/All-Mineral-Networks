
# ============================================================
# COMPLETE PM NETWORK SHARING PACKAGE
#
# Includes:
# - All 1500 final modified datasets
# - All 1500 network PNG visualizations
# - Corrected dataset copies
# - All 100 progressive topology logs
# - Final scripts, results, and project notes
#
# Excludes the older exploratory 5% experiments.
#
# Original project files are NOT modified.
# ============================================================


# STEP 1: Set folders

project <- paste0(
  "/Users/lindsaypodjasek/Documents/datathon/",
  "all mineral network"
)

package <- "/Volumes/JARROD BACK/LINDSAY-export/PM_Complete_Sharing_Package"

zip_file <- "/Volumes/JARROD BACK/LINDSAY-export/PM_Complete_Sharing_Package.zip"

# Do not overwrite a previous package
if (dir.exists(package) || file.exists(zip_file)) {
  stop("A sharing package already exists. Check it first.")
}

# Original experiment folders
task1 <- file.path(
  project,
  "Remove-3PM-100x"
)

task2 <- file.path(
  project,
  "Remove-PM-5-percent",
  "Remove-PM-5-percent-50pct"
)


# STEP 2: Check required source files

required <- character()

# Independent removal datasets and network images
for (rep in 1:100) {
  
  required <- c(
    required,
    file.path(
      task1, "PM_Removal_Iterations",
      sprintf("PM_Remove_%03d.csv", rep)
    ),
    file.path(
      task1, "PM_Removal_Iterations",
      sprintf("PM_Remove_%03d_Corrected.csv", rep)
    ),
    file.path(
      task1, "PM_Networks_3Mode",
      sprintf("PM_Network_%03d.png", rep)
    )
  )
}

# Progressive removal datasets, images, and logs
for (rep in 1:100) {
  
  rep_folder <- file.path(
    task2, sprintf("Repetition-%02d", rep)
  )
  
  required <- c(
    required,
    file.path(rep_folder, "PM_Removal_Topology_Log.csv")
  )
  
  for (step in 1:14) {
    
    required <- c(
      required,
      file.path(
        rep_folder,
        sprintf("PM_5Percent_Step_%02d.csv", step)
      ),
      file.path(
        rep_folder,
        "Network_Visualizations",
        sprintf("PM_Network_Step_%02d.png", step)
      )
    )
    
    # The first 10 repetitions have separate
    # corrected CSV copies.
    if (rep <= 10) {
      
      required <- c(
        required,
        file.path(
          rep_folder,
          sprintf(
            "PM_5Percent_Step_%02d_Corrected.csv",
            step
          )
        )
      )
    }
  }
}

# Important final results
required <- c(
  required,
  file.path(
    task2,
    "PM_Combined_Topology_Log.csv"
  ),
  file.path(
    task1,
    "PM_Removal_Iterations",
    "PM_Removal_Log.csv"
  ),
  file.path(
    project,
    "PM_Network_Random_Shortest_Path_Master.csv"
  ),
  file.path(
    project,
    "PM_Network_Attribute_Metrics_Master.csv"
  ),
  file.path(project, "PM_Task1_OUTPUT_Summary.csv"),
  file.path(project, "PM_Task2_OUTPUT_Summary.csv"),
  file.path(project, "PM_OUTPUT_Progressive_Plot.png")
)

missing <- required[!file.exists(required)]

cat("Required files checked:", length(required), "\n")
cat("Missing required files:", length(missing), "\n")

if (length(missing) > 0) {
  print(missing)
  stop("Missing files. Package creation has not started.")
}


# STEP 3: Create sharing package folders

folders <- c(
  "01_Notes",
  "02_Input_Data",
  "03_Removal_Scripts",
  "04_Independent_3PM/Datasets",
  "04_Independent_3PM/Network_Visualizations",
  "05_Progressive_5Percent",
  "06_Network_Analysis",
  "07_Summary_Results",
  "08_Logs"
)

dir.create(package)

for (folder in folders) {
  dir.create(
    file.path(package, folder),
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# STEP 4: Copy helper and file inventory

manifest <- data.frame(
  Source = character(),
  Package_Path = character(),
  Bytes = numeric(),
  stringsAsFactors = FALSE
)

copy_one <- function(source, relative_destination) {
  
  if (!file.exists(source)) {
    stop("Missing file: ", source)
  }
  
  destination <- file.path(
    package,
    relative_destination
  )
  
  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  success <- file.copy(
    source,
    destination,
    overwrite = FALSE
  )
  
  if (!success) {
    stop("Unable to copy: ", source)
  }
  
  manifest <<- rbind(
    manifest,
    data.frame(
      Source = source,
      Package_Path = relative_destination,
      Bytes = file.info(destination)$size
    )
  )
}


# STEP 5: Copy notes

note_files <- list.files(
  project,
  pattern = "^Notes\\.(docx|pdf)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(note_files) == 0) {
  warning("No Notes.docx or Notes.pdf found.")
}

for (f in note_files) {
  copy_one(
    f,
    file.path("01_Notes", basename(f))
  )
}


# STEP 6: Copy original input data

input_folder <- file.path(project, "inputs")

input_candidates <- c(
  file.path(input_folder, "ParageneticModes.csv"),
  file.path(project, "ParageneticModes.csv")
)

input_found <- input_candidates[
  file.exists(input_candidates)
]

if (length(input_found) == 0) {
  warning("Original ParageneticModes.csv not found.")
} else {
  copy_one(
    input_found[1],
    "02_Input_Data/ParageneticModes.csv"
  )
}

# Also include the original spreadsheet if present
original_xlsx <- list.files(
  input_folder,
  pattern = "ParageneticModes.*\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)

for (f in original_xlsx) {
  copy_one(
    f,
    file.path("02_Input_Data", basename(f))
  )
}


# STEP 7: Copy removal scripts

# Include the scripts inside the final experiment folders.
# Exclude scripts from the old exploratory 5% folders.

task1_scripts <- list.files(
  task1,
  pattern = "\\.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

for (f in task1_scripts) {
  copy_one(
    f,
    file.path("03_Removal_Scripts", basename(f))
  )
}

progressive_script <- file.path(
  task2,
  "PM-5percent-until-50percent-10reps.R"
)

copy_one(
  progressive_script,
  file.path(
    "03_Removal_Scripts",
    basename(progressive_script)
  )
)


# STEP 8: Copy independent removal experiment

cat("\nCopying independent 3PM experiment...\n")

task1_data <- file.path(
  task1,
  "PM_Removal_Iterations"
)

task1_images <- file.path(
  task1,
  "PM_Networks_3Mode"
)

for (rep in 1:100) {
  
  base <- sprintf("PM_Remove_%03d", rep)
  
  # Original dataset
  copy_one(
    file.path(task1_data, paste0(base, ".csv")),
    file.path(
      "04_Independent_3PM",
      "Datasets",
      paste0(base, ".csv")
    )
  )
  
  # Corrected dataset
  copy_one(
    file.path(
      task1_data,
      paste0(base, "_Corrected.csv")
    ),
    file.path(
      "04_Independent_3PM",
      "Datasets",
      paste0(base, "_Corrected.csv")
    )
  )
  
  # Network visualization
  image_name <- sprintf("PM_Network_%03d.png", rep)
  
  copy_one(
    file.path(task1_images, image_name),
    file.path(
      "04_Independent_3PM",
      "Network_Visualizations",
      image_name
    )
  )
}

cat("Task 1 complete: 100 datasets and 100 PNGs.\n")


# STEP 9: Copy progressive removal experiment

cat("\nCopying progressive 5% experiment...\n")

for (rep in 1:100) {
  
  rep_name <- sprintf("Repetition-%02d", rep)
  
  source_folder <- file.path(
    task2,
    rep_name
  )
  
  destination_folder <- file.path(
    "05_Progressive_5Percent",
    rep_name
  )
  
  for (step in 1:14) {
    
    base <- sprintf(
      "PM_5Percent_Step_%02d",
      step
    )
    
    # Modified dataset
    copy_one(
      file.path(
        source_folder,
        paste0(base, ".csv")
      ),
      file.path(
        destination_folder,
        paste0(base, ".csv")
      )
    )
    
    # Separate corrected copy for first 10 repetitions
    if (rep <= 10) {
      
      copy_one(
        file.path(
          source_folder,
          paste0(base, "_Corrected.csv")
        ),
        file.path(
          destination_folder,
          paste0(base, "_Corrected.csv")
        )
      )
    }
    
    # Network visualization
    image_name <- sprintf(
      "PM_Network_Step_%02d.png",
      step
    )
    
    copy_one(
      file.path(
        source_folder,
        "Network_Visualizations",
        image_name
      ),
      file.path(
        destination_folder,
        "Network_Visualizations",
        image_name
      )
    )
  }
  
  # Individual topology log
  copy_one(
    file.path(
      source_folder,
      "PM_Removal_Topology_Log.csv"
    ),
    file.path(
      destination_folder,
      "PM_Removal_Topology_Log.csv"
    )
  )
  
  if (rep %% 10 == 0) {
    cat("Completed", rep, "of 100 repetitions.\n")
  }
}

cat("Task 2 complete: 1400 datasets and 1400 PNGs.\n")


# STEP 10: Copy final network analysis results

analysis_files <- c(
  "PM_Network_Random_Shortest_Path_Master.csv",
  "PM_Network_Attribute_Metrics_Master.csv"
)

for (filename in analysis_files) {
  copy_one(
    file.path(project, filename),
    file.path("06_Network_Analysis", filename)
  )
}


# STEP 11: Copy final analysis R scripts

root_scripts <- list.files(
  project,
  pattern = "\\.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Final analysis scripts only.
# Exclude earlier all-pairs shortest-path analysis.
final_patterns <- c(
  "sum[-_]correction",
  "attribute.*batch",
  "random.*shortest.*path",
  "output[-_]summary",
  "package[-_]sharing"
)

for (f in root_scripts) {
  
  filename <- basename(f)
  
  matches <- any(vapply(
    final_patterns,
    function(p) grepl(
      p,
      filename,
      ignore.case = TRUE
    ),
    logical(1)
  ))
  
  if (matches) {
    
    copy_one(
      f,
      file.path(
        "06_Network_Analysis",
        filename
      )
    )
  }
}


# STEP 12: Copy summary results

summary_files <- c(
  "PM_Task1_OUTPUT_Summary.csv",
  "PM_Task2_OUTPUT_Summary.csv",
  "PM_OUTPUT_Progressive_Plot.png"
)

for (filename in summary_files) {
  
  copy_one(
    file.path(project, filename),
    file.path("07_Summary_Results", filename)
  )
}


# STEP 13: Copy logs

log_sources <- c(
  file.path(
    task1_data,
    "PM_Removal_Log.csv"
  ),
  file.path(
    task2,
    "PM_Combined_Topology_Log.csv"
  ),
  file.path(
    project,
    "PM_Sum_Correction_Log.csv"
  )
)

for (f in log_sources) {
  
  if (file.exists(f)) {
    copy_one(
      f,
      file.path("08_Logs", basename(f))
    )
  }
}


# STEP 14: Verify the package

cat("\nVerifying packaged datasets and images...\n")

count_matching <- function(folder, pattern) {
  
  length(list.files(
    file.path(package, folder),
    pattern = pattern,
    recursive = TRUE
  ))
}

independent_csv <- count_matching(
  "04_Independent_3PM/Datasets",
  "^PM_Remove_[0-9]{3}\\.csv$"
)

progressive_csv <- count_matching(
  "05_Progressive_5Percent",
  "^PM_5Percent_Step_[0-9]{2}\\.csv$"
)

independent_png <- count_matching(
  "04_Independent_3PM/Network_Visualizations",
  "^PM_Network_[0-9]{3}\\.png$"
)

progressive_png <- count_matching(
  "05_Progressive_5Percent",
  "^PM_Network_Step_[0-9]{2}\\.png$"
)

cat("Independent datasets:", independent_csv, "/ 100\n")
cat("Progressive datasets:", progressive_csv, "/ 1400\n")
cat("Independent PNGs:", independent_png, "/ 100\n")
cat("Progressive PNGs:", progressive_png, "/ 1400\n")

if (
  independent_csv != 100 ||
  progressive_csv != 1400 ||
  independent_png != 100 ||
  progressive_png != 1400
) {
  stop("Verification failed. ZIP not created.")
}

# Save complete inventory
write.csv(
  manifest,
  file.path(package, "Package_Manifest.csv"),
  row.names = FALSE
)

cat("\nAll 1500 datasets and 1500 PNGs verified!\n")


# STEP 15: Create ZIP archive

cat("\nCreating ZIP archive...\n")

if (!nzchar(Sys.which("zip"))) {
  stop(
    "The ZIP utility was not found. ",
    "The complete sharing folder is still available."
  )
}

# ZIP the complete package folder
old_directory <- getwd()

zip_status <- tryCatch({
  
  setwd(project)
  
  utils::zip(
    zipfile = zip_file,
    files = basename(package),
    flags = "-rq"
  )
  
}, finally = {
  
  setwd(old_directory)
  
})

if (!file.exists(zip_file) ||
    is.na(file.info(zip_file)$size) ||
    file.info(zip_file)$size == 0) {
  
  stop("ZIP creation could not be verified.")
}

cat("\n====================================\n")
cat("SHARING PACKAGE COMPLETE\n")
cat("====================================\n")

cat("Total primary datasets:",
    independent_csv + progressive_csv, "\n")

cat("Total network PNGs:",
    independent_png + progressive_png, "\n")

cat("Total copied files:",
    nrow(manifest), "\n")

cat("\nSharing folder:\n", package, "\n")
cat("\nZIP archive:\n", zip_file, "\n")
cat("\nDONE!\n")
