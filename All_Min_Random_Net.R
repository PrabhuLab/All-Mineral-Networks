#Generate Random Bipartite Networks

if(!require(igraph)) install.packages("igraph")
if(!require(visNetwork)) install.packages("visNetwork")

library(igraph)
library(visNetwork)

# 2. Define the network parameters
n1 <- 5855  # Number of nodes in Set 1
n2 <- 66   # Number of nodes in Set 2
ed <- 13275 # Total number of edges to place

# 3. Generate the random bipartite graph using igraph
# 'type = "gnm"' samples exactly the number of edges requested
g <- sample_bipartite(n1, n2, type = "gnm", m = ed, directed = FALSE)

#plot(g)

# PRE-COMPUTE COORDINATES (The Speed Fix)
# This calculates the X and Y positions instantly in igraph's C-backend
coords <- layout_nicely(g)

# Prepare nodes data frame with coordinates mapped to 'x' and 'y'
# visNetwork looks for 'x' and 'y' columns to hard-position nodes.
# Multiplied by 500 to spread the nodes out nicely along the grid.
nodes_df <- data.frame(
  id = 1:vcount(g),
  group = ifelse(V(g)$type, "Set 2", "Set 1"),
  label = paste("Node", 1:vcount(g)),
  x = coords[, 1] * 10,
  y = coords[, 2] * 10
)


# Extract edges
edge_matrix <- igraph::as_edgelist(g, names = FALSE)
edges_df <- data.frame(from = edge_matrix[,1], to = edge_matrix[,2])

# 5. Create the interactive visualization
# 'visIgraphLayout' automatically positions nodes into 2 distinct columns
visNetwork(nodes_df, edges_df, width = "100%", height = "600px") %>%
  visGroups(groupname = "Set 1", color = "#4f81bd") %>%
  visGroups(groupname = "Set 2", color = "#c0504d") %>%
  # IMPORTANT: Turn off the physics engine so visNetwork accepts preloaded coordinates
  visPhysics(stabilization = FALSE, enabled = FALSE) %>% 
  visNodes(fixed = TRUE) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE)
