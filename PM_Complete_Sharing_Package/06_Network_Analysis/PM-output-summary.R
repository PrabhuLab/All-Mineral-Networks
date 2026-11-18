
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
