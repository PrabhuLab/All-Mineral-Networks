
# ============================================================
# MINERAL-PARAGENETIC MODE NETWORK ATTRIBUTE ANALYSIS
#
# Calculates three OUTPUT metrics:
#   1. Density range / network diameter
#   2. Hardness range / network diameter
#   3. Age range / network diameter
#
# Experiments:
#   Task 1: 100 independent three-mode removals
#   Task 2: 100 progressive repetitions x 14 steps
#
# Total expected networks: 1500
#
# Uses the diameter of the largest connected component.
# Saves results after every network.
# ============================================================


# STEP 1: Load igraph

library(igraph)


# STEP 2: Set project folder

project_folder <- paste0(
  "/Users/lindsaypodjasek/Documents/datathon/",
  "all mineral network"
)

output_file <- file.path(
  project_folder,
  "PM_Network_Attribute_Metrics_Master.csv"
)


# STEP 3: Build a list of all network CSV files

file_list <- data.frame(
  Experiment = character(),
  Repetition = integer(),
  Step = integer(),
  File = character(),
  stringsAsFactors = FALSE
)


# TASK 1: Independent three-mode removals

task1_folder <- file.path(
  project_folder,
  "Remove-3PM-100x",
  "PM_Removal_Iterations"
)

for (rep in 1:100) {
  
  filename <- sprintf(
    "PM_Remove_%03d_Corrected.csv",
    rep
  )
  
  input_file <- file.path(task1_folder, filename)
  
  file_list <- rbind(
    file_list,
    data.frame(
      Experiment = "Independent_3PM",
      Repetition = rep,
      Step = 1,
      File = input_file
    )
  )
}


# TASK 2: Progressive removal until 50% remain

task2_folder <- file.path(
  project_folder,
  "Remove-PM-5-percent",
  "Remove-PM-5-percent-50pct"
)

for (rep in 1:100) {
  
  repetition_folder <- file.path(
    task2_folder,
    sprintf("Repetition-%02d", rep)
  )
  
  for (step in 1:14) {
    
    base_name <- sprintf(
      "PM_5Percent_Step_%02d",
      step
    )
    
    corrected_file <- file.path(
      repetition_folder,
      paste0(base_name, "_Corrected.csv")
    )
    
    original_file <- file.path(
      repetition_folder,
      paste0(base_name, ".csv")
    )
    
    # Prefer corrected files when available.
    # Newer repetitions may already contain updated PM sums.
    if (file.exists(corrected_file)) {
      input_file <- corrected_file
    } else {
      input_file <- original_file
    }
    
    file_list <- rbind(
      file_list,
      data.frame(
        Experiment = "Progressive_5Percent",
        Repetition = rep,
        Step = step,
        File = input_file
      )
    )
  }
}


# STEP 4: Check all expected files BEFORE analysis

missing_files <- file_list$File[
  !file.exists(file_list$File)
]

cat("Expected networks:", nrow(file_list), "\n")
cat("Missing files:", length(missing_files), "\n")

if (length(missing_files) > 0) {
  
  print(head(missing_files, 20))
  
  stop(
    "Some network CSV files could not be found. ",
    "No analysis has started."
  )
}


# STEP 5: Function for an attribute's minimum,
# maximum, range, and OUTPUT metric

calculate_attribute <- function(
  data,
  mineral_ids,
  column_name,
  network_diameter,
  exclude_zero = FALSE
) {
  
  values <- suppressWarnings(
    as.numeric(
      as.character(data[mineral_ids, column_name])
    )
  )
  
  # Exclude missing and non-finite measurements
  values <- values[is.finite(values)]
  
  # Hardness: exclude zero/nonpositive values
  if (exclude_zero) {
    values <- values[values > 0]
  }
  
  n_valid <- length(values)
  
  # No data: return missing results
  if (n_valid == 0) {
    
    return(c(
      N = 0,
      Min = NA_real_,
      Max = NA_real_,
      Range = NA_real_,
      OUTPUT = NA_real_
    ))
  }
  
  minimum <- min(values)
  maximum <- max(values)
  attribute_range <- maximum - minimum
  
  if (
    is.finite(network_diameter) &&
    network_diameter > 0
  ) {
    
    output_metric <- attribute_range / network_diameter
    
  } else {
    
    output_metric <- NA_real_
  }
  
  return(c(
    N = n_valid,
    Min = minimum,
    Max = maximum,
    Range = attribute_range,
    OUTPUT = output_metric
  ))
}


# STEP 6: Analyze one mineral-mode network

analyze_network <- function(input_file) {
  
  data <- read.csv(
    input_file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  attributes <- c(
    "Density_Avg",
    "Hardness_Avg",
    "Oldest Known Age (Ma)"
  )
  
  missing_attributes <- setdiff(
    attributes,
    names(data)
  )
  
  if (length(missing_attributes) > 0) {
    stop(
      "Missing attributes in ",
      input_file,
      ": ",
      paste(missing_attributes, collapse = ", ")
    )
  }
  
  # Identify all remaining mode/submode columns
  pm_columns <- grep(
    "^p[0-9]+[a-z]*$",
    names(data),
    value = TRUE
  )
  
  # Construct association matrix
  if (length(pm_columns) > 0 && nrow(data) > 0) {
    
    incidence <- as.matrix(
      data[, pm_columns, drop = FALSE]
    )
    
    incidence <- matrix(
      suppressWarnings(as.numeric(incidence)),
      nrow = nrow(data),
      ncol = length(pm_columns)
    )
    
    incidence[is.na(incidence)] <- 0
    incidence <- (incidence == 1) * 1
    
  } else {
    
    incidence <- matrix(
      0,
      nrow = nrow(data),
      ncol = length(pm_columns)
    )
  }
  
  # Build bipartite graph
  graph <- graph_from_incidence_matrix(
    incidence,
    directed = FALSE
  )
  
  total_nodes <- vcount(graph)
  total_edges <- ecount(graph)
  
  network_components <- components(graph)
  
  # Find largest connected component
  if (total_nodes > 0) {
    
    largest_component_id <- which.max(
      network_components$csize
    )
    
    largest_nodes <- which(
      network_components$membership ==
        largest_component_id
    )
    
    largest_graph <- induced_subgraph(
      graph,
      vids = largest_nodes
    )
    
    # Mineral vertices have type FALSE.
    # This identifies mineral rows belonging to the LCC.
    mineral_ids <- largest_nodes[
      !V(graph)$type[largest_nodes]
    ]
    
    lcc_nodes <- vcount(largest_graph)
    
    # Exact unweighted network diameter
    network_diameter <- diameter(
      largest_graph,
      directed = FALSE,
      weights = NA
    )
    
  } else {
    
    mineral_ids <- integer(0)
    lcc_nodes <- 0
    network_diameter <- NA_real_
  }
  
  # Attribute calculations
  density <- calculate_attribute(
    data,
    mineral_ids,
    "Density_Avg",
    network_diameter
  )
  
  hardness <- calculate_attribute(
    data,
    mineral_ids,
    "Hardness_Avg",
    network_diameter,
    exclude_zero = TRUE
  )
  
  age <- calculate_attribute(
    data,
    mineral_ids,
    "Oldest Known Age (Ma)",
    network_diameter
  )
  
  # Return one row per network
  data.frame(
    Minerals_Total = nrow(data),
    Modes_Remaining = length(pm_columns),
    
    Nodes = total_nodes,
    Edges = total_edges,
    Components = network_components$no,
    LCC_Nodes = lcc_nodes,
    Diameter = network_diameter,
    
    Density_N = density["N"],
    Density_Min = density["Min"],
    Density_Max = density["Max"],
    Density_Range = density["Range"],
    Density_OUTPUT = density["OUTPUT"],
    
    Hardness_N = hardness["N"],
    Hardness_Min = hardness["Min"],
    Hardness_Max = hardness["Max"],
    Hardness_Range = hardness["Range"],
    Hardness_OUTPUT = hardness["OUTPUT"],
    
    Age_N = age["N"],
    Age_Min = age["Min"],
    Age_Max = age["Max"],
    Age_Range = age["Range"],
    Age_OUTPUT = age["OUTPUT"],
    
    row.names = NULL
  )
}


# STEP 7: Set up results and resume protection

if (file.exists(output_file)) {
  
  results <- read.csv(
    output_file,
    stringsAsFactors = FALSE
  )
  
  cat(
    "Existing output found. Resuming with",
    nrow(results),
    "completed networks.\n"
  )
  
} else {
  
  results <- NULL
}


# STEP 8: Process all networks

for (i in seq_len(nrow(file_list))) {
  
  experiment <- file_list$Experiment[i]
  rep <- file_list$Repetition[i]
  step <- file_list$Step[i]
  
  # Skip networks already saved to the master CSV
  if (!is.null(results)) {
    
    already_done <- any(
      results$Experiment == experiment &
        results$Repetition == rep &
        results$Step == step
    )
    
    if (already_done) {
      next
    }
  }
  
  cat(
    "\nProcessing", i, "of", nrow(file_list),
    "|", experiment,
    "| Repetition", rep,
    "| Step", step, "\n"
  )
  
  metrics <- analyze_network(file_list$File[i])
  
  new_row <- data.frame(
    Experiment = experiment,
    Repetition = rep,
    Step = step,
    metrics,
    stringsAsFactors = FALSE
  )
  
  results <- rbind(results, new_row)
  
  # Save after EVERY network
  write.csv(
    results,
    output_file,
    row.names = FALSE,
    na = ""
  )
}


# STEP 9: Final verification

cat("\n===================================\n")
cat("NETWORK ATTRIBUTE ANALYSIS FINISHED\n")
cat("===================================\n")

cat("Total networks analyzed:", nrow(results), "\n")

cat(
  "Independent 3PM networks:",
  sum(results$Experiment == "Independent_3PM"),
  "\n"
)

cat(
  "Progressive 5% networks:",
  sum(results$Experiment == "Progressive_5Percent"),
  "\n"
)

cat(
  "Missing/undefined diameters:",
  sum(is.na(results$Diameter) | results$Diameter == 0),
  "\n"
)

cat(
  "Missing density OUTPUT metrics:",
  sum(is.na(results$Density_OUTPUT)),
  "\n"
)

cat(
  "Missing hardness OUTPUT metrics:",
  sum(is.na(results$Hardness_OUTPUT)),
  "\n"
)

cat(
  "Missing age OUTPUT metrics:",
  sum(is.na(results$Age_OUTPUT)),
  "\n"
)

cat("\nResults saved to:\n")
cat(output_file, "\n")

# Preview the results in RStudio
View(results)
