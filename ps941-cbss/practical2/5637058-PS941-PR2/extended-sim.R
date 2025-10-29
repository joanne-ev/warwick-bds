library(tidyverse)
library(reshape2)
dev.new() # turn graphics device on

### FUNCTION TO GET SUM OF THRESHOLDS OF IMMEDIATE NEIGHBOURS ###
get_neighbour_sum <- function(x, y, grid) {
  
  ### Identifying active and inactive neighbours ###
  neighbours <- c(
    grid[x-1, y], grid[x+1, y], 
    grid[x, y-1], grid[x, y+1],
    grid[x-1, y-1], grid[x+1, y+1],
    grid[x-1, y+1], grid[x+1, y-1]
  )
  
  ### Adjust influence of agents ###
  # Normal activated agents
  lv_count <- sum(neighbours == 1) 
  lv_score <- lv_count * 1
  
  # High-value activate agents
  hv_count <- sum(neighbours == 2) 
  hv_score <- hv_count * 2
  
  ### Total activation score ###
  activation_score <- sum(lv_score, hv_score, na.rm = TRUE)
  
  return(activation_score)
}



### SIMULATION FUNCTION ###
agent_sim <- function(grid_size, cycle, hv_prop = 0.10, beta_dist_shape = 2) {
  activated_agents <- vector()
  dev.new()
  if (hv_prop == 0) {colour_palette <- c("black", "orange")} else if (hv_prop == 1) {colour_palette <- c("black", "green")} else {colour_palette <- c("black", "orange", "green")}
  
  ### INITIALISING ENVIRONMENT & AGENTS ###
  agent_matrix <- matrix(0, nrow = grid_size, ncol = grid_size) # creating environment 
  agent_pop <- agent_matrix %>% as.vector() %>% length() # population of agent matrix (i.e., grid_size^2)
  agent_prop <- 0.25 # proportion of agent population to be activated
  samp <- (agent_pop * agent_prop) %>% round() # number of agents sampled to be activated 
  loc <- sample(1:agent_pop, samp) # randomly sample individual agents within the population to be activated
  
  
  ### ACTIVATING AGENTS ###
  agent_matrix[loc] <- sample(1:2, samp, replace = TRUE, prob = c((1 - hv_prop), hv_prop)) # activate randomly sample individual agents (default is 10% high-value agents, 90% low-value agents)
  
  # Count the number of (in)active agents (0 = inactive, 1 = normal, 2 = high-value)
  hv_count <- sum(agent_matrix == 2) # high-value active
  lv_count <- sum(agent_matrix == 1) # no-value active
  ia_count <- sum(agent_matrix == 0) # inactive
  
  cat(hv_count, lv_count, ia_count)
  
  
  for (i in seq(cycle)) {
    
    # Identify location of a random agent (ignoring agents on the edge of the grid)
    agent_x <- sample(2:(grid_size-1), 1)
    agent_y <- sample(2:(grid_size-1), 1)
    
    # Original agent 
    original_agent <- agent_matrix[agent_x, agent_y] # check activation status (0, 1, 2)
    
    # Individual agent activation threshold (follows a beta distribution)
    thresholds <- matrix(rbeta(n = grid_size^2, shape1 = beta_dist_shape, shape2 = beta_dist_shape), nrow = grid_size, ncol = grid_size) 
    activation_threshold <- thresholds[agent_x, agent_y]
    
    # Calculate sum of thresholds of immediate neighbours
    activation_score <- get_neighbour_sum(agent_x, agent_y, agent_matrix) / 8
    
    
    ### Activate agent if the neighbour's mean activation score is greater than the agent's individual threshold ###
    # If a high-value agent gets activated
    if (original_agent == 2 & activation_score > activation_threshold) {
      status <- "HV influenced"
      agent_matrix[agent_x, agent_y] <- 2.5
      if (!"hotpink" %in% colour_palette) {colour_palette <- append(colour_palette, "hotpink", after = which(colour_palette == "green"))} # introduce a new colour to the colour palette to indicate whether an agent has been activated  
    } else if (original_agent == 2 & activation_score <= activation_threshold)  {status <- "HV not influenced"}
    
    # If a low-value agent gets activated
    if (original_agent == 1 & activation_score > activation_threshold) {
      status <- "LV influenced"
      agent_matrix[agent_x, agent_y] <- 1.5
      if (!"yellow" %in% colour_palette) {colour_palette <- append(colour_palette, "yellow", after = which(colour_palette == "orange"))} 
    } else if (original_agent == 1 & activation_score <= activation_threshold) {status <- "LV not influenced"}
    
    # If an inactive agent gets activated
    if (original_agent == 0 & activation_score > activation_threshold) {
      status <- "IA activated"
      agent_matrix[agent_x, agent_y] <- 0.5
      if (!"lightgrey" %in% colour_palette) {colour_palette <- append(colour_palette, "lightgrey", after = which(colour_palette == "black"))}
    } else if (original_agent == 0 & activation_score <= activation_threshold)  {status <- "IA not activated" } 
    
    activated_agents[i] <- sum(agent_matrix != 0)
    
    
    ### Creating simulation ###
    sim_img <- image(
      agent_matrix,
      axes = FALSE, # remove axes
      main = paste("Step:", i, "Activation:", status),
      col = colour_palette
    )
    mtext(
      paste0(
        hv_prop * 100, "% of high-value agents within the agent population", "\n",
        "Inactive agent (black n = ", ia_count, ") is activated (lightgrey n = ", sum(agent_matrix == 0.5), ").", "\n",
        "Low-value active agent (orange n = ", lv_count, ") is influenced (yellow n = ", sum(agent_matrix == 1.5), ").", "\n",
        "High-value active agent (green n = ", hv_count, ") is influenced (hotpink n = ", sum(agent_matrix == 2.5), ").", "\n"
      ), 
      side = 1, # side location (i.e., bottom)
      line = 4, # location within the side 
      cex = 0.9 # font size
      )
    
    if (i == cycle) {
      png(paste0("plot_", hv_prop, "_sim.png"), width = 2000, height = 1600, res = 300)
      sim_img
      dev.off()
    } else {sim_img}
    
    Sys.sleep(0.005)
    
  }
  
  ### Plot ###
  png(paste0("plot_", hv_prop, ".png"), width = 2000, height = 1600, res = 300)
  par(mgp = c(2, 0.7, 0))
  plot(
    activated_agents, type = "l",
    main = "Number of Active Agents Over Time",
    xlab = paste0("Time (", cycle, " cycles)"),
    ylab = paste0("Number of Active Agents (total n = ", grid_size^2, ")"),
    cex.main = 1, cex.lab = 0.8, cex.axis = 0.7 # change font size
  )
  mtext(
    text = paste0("Proportion of high-value agents = ", hv_prop * 100, "%; Beta distribution shape parameters = ", beta_dist_shape, "\n",
                  "Highest number of activated agents = ", max(activated_agents)),
    side = 1, # location within the plot (e.g., bottom)
    line = 3.5, # position within the location
    cex = 0.6
  )
  dev.off()

}


set.seed(1)
for (hv in seq(0, 1, by = 0.25)) {agent_sim(grid_size = 36, cycle = 2000, hv_prop = hv)}


