
# ============================================================
# FINAL NETWORK OUTPUT METRIC ANALYSIS
#
# 100 independent 3-mode removal networks
# 1400 progressive 5% removal networks
#
# ATTRIBUTES:
# Density, Hardness, Geological Age
#
# METHOD:
# Randomly choose one mineral with the minimum value and one with the maximum value for each attribute.
# Calculate shortest path using igraph::shortest_paths().
# OUTPUT = shortest-path length / network diameter.
#
# Saves after every network and supports resuming.
# ============================================================

library(igraph)

# STEP 1: Project files

project_folder <- paste0(
  "/Users/lindsaypodjasek/Documents/datathon/",
  "all mineral network"
)

previous_metrics_file <- file.path(
  project_folder,
  "PM_Network_Attribute_Metrics_Master.csv"
)

output_file <- file.path(
  project_folder,
  "PM_Network_Random_Shortest_Path_Master.csv"
)

previous <- read.csv(
  previous_metrics_file,
  stringsAsFactors = FALSE
)

stopifnot(nrow(previous) == 1500)


# STEP 2: Locate the original network datasets

find_file <- function(experiment, rep, step) {
  
  if (experiment == "Independent_3PM") {
    
    return(file.path(
      project_folder,
      "Remove-3PM-100x",
      "PM_Removal_Iterations",
      sprintf("PM_Remove_%03d_Corrected.csv", rep)
    ))
  }
  
  if (experiment == "Progressive_5Percent") {
    
    folder <- file.path(
      project_folder,
      "Remove-PM-5-percent",
      "Remove-PM-5-percent-50pct",
      sprintf("Repetition-%02d", rep)
    )
    
    base <- sprintf("PM_5Percent_Step_%02d", step)
    
    corrected <- file.path(
      folder, paste0(base, "_Corrected.csv")
    )
    
    regular <- file.path(
      folder, paste0(base, ".csv")
    )
    
    if (file.exists(corrected)) {
      return(corrected)
    }
    
    return(regular)
  }
  
  stop("Unknown experiment: ", experiment)
}


files <- mapply(
  find_file,
  previous$Experiment,
  previous$Repetition,
  previous$Step,
  USE.NAMES = FALSE
)

cat("Expected networks:", length(files), "\n")
cat("Missing files:", sum(!file.exists(files)), "\n")

if (any(!file.exists(files))) {
  print(files[!file.exists(files)])
  stop("Missing datasets. Stopping.")
}


# STEP 3: Build the bipartite network

build_network <- function(data) {
  
  pm_columns <- grep(
    "^p[0-9]+[a-z]*$",
    names(data),
    value = TRUE
  )
  
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
  
  graph <- graph_from_incidence_matrix(
    incidence,
    directed = FALSE
  )
  
  # Preserve the CSV row number of every mineral
  V(graph)$name <- c(
    paste0("mineral_", seq_len(nrow(data))),
    paste0("mode_", pm_columns)
  )
  
  return(graph)
}


# STEP 4: Calculate one attribute's OUTPUT metric

calculate_output <- function(
  graph,
  data,
  attribute,
  graph_diameter,
  seed,
  exclude_zero = FALSE
) {
  
  set.seed(seed)
  
  # Mineral vertices only
  mineral_vertices <- which(!V(graph)$type)
  
  mineral_rows <- as.integer(
    sub(
      "^mineral_",
      "",
      V(graph)$name[mineral_vertices]
    )
  )
  
  values <- suppressWarnings(
    as.numeric(
      as.character(data[[attribute]][mineral_rows])
    )
  )
  
  valid <- which(is.finite(values))
  
  if (exclude_zero) {
    valid <- valid[values[valid] > 0]
  }
  
  if (length(valid) == 0) {
    
    return(list(
      Min_Value = NA_real_,
      Max_Value = NA_real_,
      Min_Mineral = NA_character_,
      Max_Mineral = NA_character_,
      Min_Row = NA_integer_,
      Max_Row = NA_integer_,
      N_Valid = 0,
      N_Min_Ties = 0,
      N_Max_Ties = 0,
      Path_Edges = NA_real_,
      OUTPUT = NA_real_
    ))
  }
  
  # Find attribute extremes
  minimum <- min(values[valid])
  maximum <- max(values[valid])
  
  min_candidates <- valid[
    values[valid] == minimum
  ]
  
  max_candidates <- valid[
    values[valid] == maximum
  ]
  
  # Randomly select ONE mineral at each extreme.
  # sample.int() safely handles a single candidate.
  min_choice <- min_candidates[
    sample.int(length(min_candidates), 1)
  ]
  
  max_choice <- max_candidates[
    sample.int(length(max_candidates), 1)
  ]
  
  min_vertex <- mineral_vertices[min_choice]
  max_vertex <- mineral_vertices[max_choice]
  
  min_row <- mineral_rows[min_choice]
  max_row <- mineral_rows[max_choice]
  
  # Calculate the actual shortest path
  path <- igraph::shortest_paths(
    graph,
    from = min_vertex,
    to = max_vertex,
    weights = NA,
    output = "vpath"
  )$vpath[[1]]
  
  if (length(path) == 0) {
    path_edges <- NA_real_
  } else {
    path_edges <- length(path) - 1
  }
  
  # Normalize using network diameter
  if (
    is.finite(path_edges) &&
    is.finite(graph_diameter) &&
    graph_diameter > 0
  ) {
    output_metric <- path_edges / graph_diameter
  } else {
    output_metric <- NA_real_
  }
  
  # Store selected mineral names
  min_name <- as.character(data$PM[min_row])
  max_name <- as.character(data$PM[max_row])
  
  return(list(
    Min_Value = minimum,
    Max_Value = maximum,
    Min_Mineral = min_name,
    Max_Mineral = max_name,
    Min_Row = min_row,
    Max_Row = max_row,
    N_Valid = length(valid),
    N_Min_Ties = length(min_candidates),
    N_Max_Ties = length(max_candidates),
    Path_Edges = path_edges,
    OUTPUT = output_metric
  ))
}


# STEP 5: Analyze an individual network

analyze_one <- function(input_file, expected_diameter, i) {
  
  data <- read.csv(
    input_file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  graph <- build_network(data)
  
  # Identify the largest connected component
  comp <- components(graph)
  
  largest_id <- which.max(comp$csize)
  
  largest_vertices <- which(
    comp$membership == largest_id
  )
  
  lcc <- induced_subgraph(
    graph,
    vids = largest_vertices
  )
  
  # Calculate network diameter
  d <- diameter(
    lcc,
    directed = FALSE,
    weights = NA
  )
  
  # Verify previous calculation
  if (d != expected_diameter) {
    stop("Diameter mismatch: ", input_file)
  }
  
  # Reproducible random selection for every
  # network and attribute
  base_seed <- 20261117 + i * 10
  
  density <- calculate_output(
    lcc, data, "Density_Avg",
    d, base_seed + 1
  )
  
  hardness <- calculate_output(
    lcc, data, "Hardness_Avg",
    d, base_seed + 2,
    exclude_zero = TRUE
  )
  
  age <- calculate_output(
    lcc, data, "Oldest Known Age (Ma)",
    d, base_seed + 3
  )
  
  names(density) <- paste0(
    "Density_", names(density)
  )
  
  names(hardness) <- paste0(
    "Hardness_", names(hardness)
  )
  
  names(age) <- paste0(
    "Age_", names(age)
  )
  
  as.data.frame(
    c(
      list(
        Diameter = d,
        LCC_Nodes = vcount(lcc)
      ),
      density,
      hardness,
      age
    ),
    stringsAsFactors = FALSE
  )
}


# STEP 6: Resume if interrupted

if (file.exists(output_file)) {
  
  results <- read.csv(
    output_file,
    stringsAsFactors = FALSE
  )
  
  cat(
    "Resuming from",
    nrow(results),
    "completed networks.\n"
  )
  
} else {
  
  results <- NULL
}


# STEP 7: Process all networks

for (i in seq_len(nrow(previous))) {
  
  experiment <- previous$Experiment[i]
  rep <- previous$Repetition[i]
  step <- previous$Step[i]
  
  # Skip networks already completed
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
    "Processing", i, "of", nrow(previous),
    "|", experiment,
    "| Repetition", rep,
    "| Step", step, "\n"
  )
  
  metrics <- analyze_one(
    files[i],
    previous$Diameter[i],
    i
  )
  
  new_row <- cbind(
    previous[i, c(
      "Experiment", "Repetition", "Step"
    )],
    metrics
  )
  
  results <- rbind(results, new_row)
  
  # Save after EACH completed network
  write.csv(
    results,
    output_file,
    row.names = FALSE,
    na = ""
  )
}


# STEP 8: Final checks

cat("\n===================================\n")
cat("FINAL SHORTEST-PATH ANALYSIS COMPLETE\n")
cat("===================================\n")

cat("Networks analyzed:", nrow(results), "\n")

cat(
  "Missing density OUTPUT:",
  sum(is.na(results$Density_OUTPUT)), "\n"
)

cat(
  "Missing hardness OUTPUT:",
  sum(is.na(results$Hardness_OUTPUT)), "\n"
)

cat(
  "Missing age OUTPUT:",
  sum(is.na(results$Age_OUTPUT)), "\n"
)

cat("\nSaved to:\n", output_file, "\n")

View(results)



# FINAL OUTPUT METRIC SUMMARY

# Load completed results
folder <- "/Users/lindsaypodjasek/Documents/datathon/all mineral network"

data <- read.csv(file.path(
  folder,
  "PM_Network_Random_Shortest_Path_Master.csv"
))

# TASK 1: Average across 100 independent networks
independent <- subset(data, Experiment == "Independent_3PM")

task1_summary <- data.frame(
  Attribute = c("Density", "Hardness", "Age"),
  Mean_OUTPUT = c(
    mean(independent$Density_OUTPUT, na.rm = TRUE),
    mean(independent$Hardness_OUTPUT, na.rm = TRUE),
    mean(independent$Age_OUTPUT, na.rm = TRUE)
  ),
  SD = c(
    sd(independent$Density_OUTPUT, na.rm = TRUE),
    sd(independent$Hardness_OUTPUT, na.rm = TRUE),
    sd(independent$Age_OUTPUT, na.rm = TRUE)
  )
)

print(task1_summary)

# TASK 2: Average across 100 repetitions at each step
progressive <- subset(data, Experiment == "Progressive_5Percent")

task2_summary <- aggregate(
  cbind(Density_OUTPUT, Hardness_OUTPUT, Age_OUTPUT, Diameter) ~ Step,
  data = progressive,
  FUN = mean
)

print(task2_summary)

# Save both summaries
write.csv(task1_summary,
          file.path(folder, "PM_Task1_OUTPUT_Summary.csv"),
          row.names = FALSE)

write.csv(task2_summary,
          file.path(folder, "PM_Task2_OUTPUT_Summary.csv"),
          row.names = FALSE)

# Plot the three attributes
matplot(
  task2_summary$Step,
  task2_summary[, c(
    "Density_OUTPUT",
    "Hardness_OUTPUT",
    "Age_OUTPUT"
  )],
  type = "b",
  pch = 19,
  lty = 1,
  xlab = "Progressive Removal Step",
  ylab = "Mean OUTPUT Metric",
  main = "Shortest-Path OUTPUT Metrics During PM Removal"
)

legend(
  "topright",
  legend = c("Density", "Hardness", "Age"),
  col = 1:3,
  pch = 19,
  lty = 1
)

# Save the graph
png(file.path(folder, "PM_OUTPUT_Progressive_Plot.png"),
    width = 1200, height = 800)

matplot(
  task2_summary$Step,
  task2_summary[, c(
    "Density_OUTPUT",
    "Hardness_OUTPUT",
    "Age_OUTPUT"
  )],
  type = "b",
  pch = 19,
  lty = 1,
  col = 1:3,
  xlab = "Progressive Removal Step",
  ylab = "Mean OUTPUT Metric",
  main = "Shortest-Path OUTPUT Metrics During PM Removal"
)

legend(
  "topright",
  legend = c("Density", "Hardness", "Age"),
  col = 1:3,
  pch = 19,
  lty = 1
)

dev.off()

cat("DONE! Summaries and graph saved.\n")
