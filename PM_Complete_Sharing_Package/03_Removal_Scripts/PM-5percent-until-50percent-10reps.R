
# TASK: Randomly remove 5% of remaining paragenetic modes
# Stop when 50% of the original modes remain
# Repeat the experiment 10 times
# Network analysis using igraph and Fruchterman-Reingold

library(igraph)


# STEP 1: Import the ORIGINAL mineral-PM dataset

data <- read.csv(file.choose(), check.names = FALSE)

# Identify paragenetic mode columns
pm_columns <- grep("^p[0-9]", names(data), value = TRUE)

# Verify the original dataset
stopifnot("PM" %in% names(data))
stopifnot(length(pm_columns) == 66)

print(paste("Original modes:", length(pm_columns)))


# STEP 2: Set the output folder

output_root <- "/Users/lindsaypodjasek/Documents/datathon/all mineral network/Remove-PM-5-percent/Remove-PM-5-percent-50pct"

stopifnot(dir.exists(output_root))


# STEP 3: Prepare the original network

# Exclude the two non-mineral summary rows
original_data <- data[
  !(data$PM %in% c("PM Description", "PM sum")),
  ,
  drop = FALSE
]

# Function to create a binary association matrix
make_matrix <- function(dataset, modes) {
  
  mat <- as.matrix(dataset[, modes, drop = FALSE])
  
  mat <- matrix(
    as.numeric(mat),
    nrow = nrow(dataset)
  )
  
  mat[is.na(mat)] <- 0
  
  return((mat == 1) * 1)
}

# Exclude minerals without any original mode associations
original_matrix <- make_matrix(original_data, pm_columns)

has_mode <- rowSums(original_matrix) > 0

initially_unassociated <- sum(!has_mode)

original_data <- original_data[
  has_mode,
  ,
  drop = FALSE
]

print(paste(
  "Initially unassociated minerals:",
  initially_unassociated
))


# STEP 4: Function to calculate network topology

analyze_network <- function(dataset, modes) {
  
  if (nrow(dataset) == 0 || length(modes) == 0) {
    
    return(list(
      graph = NULL,
      nodes = 0,
      edges = 0,
      components = 0,
      largest_component = 0,
      largest_fraction = 0
    ))
  }
  
  mat <- make_matrix(dataset, modes)
  
  g <- graph_from_incidence_matrix(
    mat,
    directed = FALSE
  )
  
  comp <- components(g)
  
  largest <- max(comp$csize)
  
  return(list(
    graph = g,
    nodes = vcount(g),
    edges = ecount(g),
    components = comp$no,
    largest_component = largest,
    largest_fraction = largest / vcount(g)
  ))
}


# STEP 5: Function to save Fruchterman-Reingold networks

save_network_plot <- function(g, output_file, title_text, seed) {
  
  png(
    filename = output_file,
    width = 2400,
    height = 1800,
    res = 300
  )
  
  if (is.null(g) || vcount(g) == 0) {
    
    plot.new()
    title(main = title_text)
    text(0.5, 0.5, "No Network Remaining")
    
  } else {
    
    # Color mineral nodes blue and mode nodes orange
    V(g)$color <- ifelse(
      V(g)$type,
      "orange",
      "steelblue"
    )
    
    # Make mode nodes larger
    V(g)$size <- ifelse(
      V(g)$type,
      7,
      2
    )
    
    # Calculate Fruchterman-Reingold layout
    set.seed(seed)
    
    fr_layout <- layout_with_fr(
      g,
      niter = 500
    )
    
    # Plot the network
    plot(
      g,
      layout = fr_layout,
      vertex.label = NA,
      vertex.frame.color = NA,
      edge.color = adjustcolor("gray60", alpha.f = 0.15),
      edge.width = 0.2,
      main = title_text
    )
  }
  
  dev.off()
}


# STEP 6: Calculate original network connectivity

baseline <- analyze_network(original_data, pm_columns)

print(paste(
  "Original largest component:",
  baseline$largest_component
))


# STEP 7: Run the removal experiment

# TEST: Start with one repetition
# Later we'll expand this to repetitions 2-10

repetitions_to_run <- 11:100

# Stop when exactly half of the 66 modes remain
target_modes <- length(pm_columns) / 2

for (run in repetitions_to_run) {
  
  # Set this repetition's output folder
  run_folder <- file.path(
    output_root,
    sprintf("Repetition-%02d", run)
  )
  
  # Prevent overwriting existing results
  if (dir.exists(run_folder) &&
      length(list.files(run_folder, all.files = TRUE,
                        no.. = TRUE)) > 0) {
    stop(paste("Output folder is not empty:", run_folder))
  }
  
  dir.create(
    run_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Create a folder for network images
  network_folder <- file.path(
    run_folder,
    "Network_Visualizations"
  )
  
  dir.create(network_folder, showWarnings = FALSE)
  
  # Start each repetition with the ORIGINAL network
  current_data <- original_data
  current_modes <- pm_columns
  
  # Set a reproducible random seed
  set.seed(100000 + run)
  
  # Create an empty results log
  run_log <- data.frame(
    Step = integer(),
    Modes_Removed = character(),
    Number_Removed = integer(),
    Modes_Remaining = integer(),
    Percent_Modes_Removed = numeric(),
    Minerals_Remaining = integer(),
    Minerals_Lost_This_Step = integer(),
    Nodes = integer(),
    Edges = integer(),
    Connected_Components = integer(),
    Largest_Component = integer(),
    LCC_Fraction_Remaining = numeric(),
    LCC_Fraction_Original = numeric()
  )
  
  i <- 0
  
  # Continue until 50% of modes remain
  while (length(current_modes) > target_modes) {
    
    i <- i + 1
    
    # Count minerals before removal
    minerals_before <- nrow(current_data)
    
    # Calculate 5% of the REMAINING modes
    number_to_remove <- max(
      1,
      round(length(current_modes) * 0.05)
    )
    
    # Stop exactly at 33 modes
    number_to_remove <- min(
      number_to_remove,
      length(current_modes) - target_modes
    )
    
    # Randomly select modes to remove
    removed_modes <- sample(
      current_modes,
      number_to_remove
    )
    
    # Remove the selected mode columns
    current_data <- current_data[
      , !names(current_data) %in% removed_modes,
      drop = FALSE
    ]
    
    # Update the list of remaining modes
    current_modes <- setdiff(
      current_modes,
      removed_modes
    )
    
    # Remove minerals without remaining associations
    if (nrow(current_data) > 0) {
      
      mat <- make_matrix(current_data, current_modes)
      
      has_mode <- rowSums(mat) > 0
      
      current_data <- current_data[
        has_mode,
        ,
        drop = FALSE
      ]
    }
    
    # Calculate mineral loss during this step
    minerals_lost <- minerals_before - nrow(current_data)
    
    # Recalculate PM sum using distinct parent modes
    
    remaining_pm_columns <- grep(
      "^p[0-9]+[a-z]*$",
      names(current_data),
      value = TRUE
    )
    
    parent_modes <- sub(
      "[a-z]+$",
      "",
      remaining_pm_columns
    )
    
    # Preserve the original PM sum
    if (!"Original_PM_Sum" %in% names(current_data)) {
      current_data$Original_PM_Sum <- current_data$`PM sum for mineral`
    }
    
    # Recalculate the updated PM sum
    if (nrow(current_data) > 0) {
      
      mode_matrix <- make_matrix(
        current_data,
        remaining_pm_columns
      )
      
      current_data$`PM sum for mineral` <- apply(
        mode_matrix,
        1,
        function(row) {
          length(unique(parent_modes[row == 1]))
        }
      )
      
    }
    
    # Calculate network connectivity statistics
    results <- analyze_network(
      current_data,
      current_modes
    )
    
    # Calculate percentage of original modes removed
    percent_removed <- 100 * (
      length(pm_columns) - length(current_modes)
    ) / length(pm_columns)
    
    # Save modified dataset
    csv_file <- file.path(
      run_folder,
      sprintf("PM_5Percent_Step_%02d.csv", i)
    )
    
    write.csv(
      current_data,
      csv_file,
      row.names = FALSE
    )
    
    # Save Fruchterman-Reingold visualization
    png_file <- file.path(
      network_folder,
      sprintf("PM_Network_Step_%02d.png", i)
    )
    
    # Use a separate seed for layout visualization
    save_network_plot(
      results$graph,
      png_file,
      paste("Repetition", run, "- Step", i),
      seed = 200000 + run * 100 + i
    )
    
    # Record topology and removal results
    run_log[i, ] <- list(
      i,
      paste(removed_modes, collapse = ", "),
      length(removed_modes),
      length(current_modes),
      percent_removed,
      nrow(current_data),
      minerals_lost,
      results$nodes,
      results$edges,
      results$components,
      results$largest_component,
      results$largest_fraction,
      results$largest_component / baseline$largest_component
    )
    
    # Display progress in Console
    print(paste(
      "Repetition", run,
      "| Step", i,
      "| Modes remaining:", length(current_modes),
      "| Minerals remaining:", nrow(current_data),
      "| Components:", results$components
    ))
  }
  
  # Save the removal and topology log
  write.csv(
    run_log,
    file.path(run_folder, "PM_Removal_Topology_Log.csv"),
    row.names = FALSE
  )
  
  print(paste(
    "Repetition", run,
    "completed with", nrow(run_log), "steps"
  ))
}

print("Requested repetitions completed successfully!")

# STEP 8: Combine topology logs from all 10 repetitions

# Create an empty list to hold the logs
all_logs <- list()

# Import each repetition's topology log
for (run in 1:10) {
  
  # Identify the repetition folder
  run_folder <- file.path(
    output_root,
    sprintf("Repetition-%02d", run)
  )
  
  # Identify the log file
  log_file <- file.path(
    run_folder,
    "PM_Removal_Topology_Log.csv"
  )
  
  # Import the log
  log_data <- read.csv(log_file)
  
  # Add a column identifying the repetition
  log_data$Repetition <- run
  
  # Store the log
  all_logs[[run]] <- log_data
}

# Combine all logs into one dataset
combined_log <- do.call(rbind, all_logs)

# Move the Repetition column to the beginning
combined_log <- combined_log[
  , c("Repetition",
      setdiff(names(combined_log), "Repetition"))
]

# Export the combined log
write.csv(
  combined_log,
  file.path(output_root, "PM_Combined_Topology_Log.csv"),
  row.names = FALSE
)

# View the combined results
View(combined_log)

# Verify the number of observations
print(dim(combined_log))