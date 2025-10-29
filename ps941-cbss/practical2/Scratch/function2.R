library(tidyverse)
library(reshape2)


### FUNCTION TO GET SUM OF THRESHOLDS OF IMMEDIATE NEIGHBOURS ###
get_neighbour_sum <- function(x, y, grid) {
  
  # Identifying active and inactive neighbours
  neighbours <- c(
    grid[x-1, y], grid[x+1, y], 
    grid[x, y-1], grid[x, y+1],
    grid[x-1, y-1], grid[x+1, y+1],
    grid[x-1, y+1], grid[x+1, y-1]
  )
  
  # Normal activated agents
  lv_count <- sum(neighbours == 1) 
  lv_score <- lv_count * 0.1
  
  # High-value activate agents
  hv_count <- sum(neighbours == 2) 
  hv_score <- hv_count * 0.9
  
  # Total activation score
  activation_score <- sum(lv_score, hv_score, na.rm = TRUE)
  
  return(activation_score)
}



### SIMULATION ###
agent_sim <- function(grid_size, cycle, agent_prop = 0.25) {
  activated_agents <- vector()
  
  ### INITIALISING ENVIRONMENT ###
  agent_matrix <- matrix(0, nrow = grid_size, ncol = grid_size) # creating environment # nolint: line_length_linter.
  agent_pop <- agent_matrix %>% as.vector() %>% length() # population of agent matrix (i.e., grid_size^2)
  agent_prop <- agent_prop # proportion of agent population to be activated
  samp <- (agent_pop * agent_prop) %>% round() # number of agents sampled to be activated 
  loc <- sample(1:agent_pop, samp) # identify individual agents within the population to be activated
  
  
  ### ACTIVATING AGENTS ###
  agent_matrix[loc] <- sample(1:2, samp, replace = TRUE, prob = c(0.75, 0.25)) # randomly activate agents 
  
  # Count the number of (in)active agents (0 = inactive, 1 = normal, 2 = high-value)
  hv_count <- sum(agent_matrix == 2) # high-value active
  lv_count <- sum(agent_matrix == 1) # no-value active
  ia_count <- sum(agent_matrix == 0) # inactive
  
  cat(hv_count, lv_count, ia_count)
  
  dev.new()
  
  colour_palette <- c("black", "orange", "green")
  
  for (i in seq(cycle)) {
    
    # Identify location of a random agent (ignoring agents on the edge of the grid)
    agent_x <- sample(2:(grid_size-1), 1)
    agent_y <- sample(2:(grid_size-1), 1)
    
    # Original agent 
    original_agent <- agent_matrix[agent_x, agent_y] # check activation status (0, 1, 2)
    
    # Individual agent activation threshold (follows a beta distribution)
    thresholds <- matrix(rbeta(n = grid_size^2, shape1 = 5, shape2 = 5), nrow = grid_size, ncol = grid_size) 
    activation_threshold <- original_agent + thresholds[agent_x, agent_y]
    
    # Calculate sum of thresholds of immediate neighbours
    activation_score <- get_neighbour_sum(agent_x, agent_y, agent_matrix)
    
    
    # Activate agent if the neighbour's total activation score is greater than the agent's individual threshold
    # When a high-value agent gets activated
    if (original_agent == 2 & activation_score > activation_threshold) {
      status <- "HV activated"
      agent_matrix[agent_x, agent_y] <- 2
      colour_palette <- c(colour_palette, 'blue')
    } else if (original_agent == 2 & activation_score <= activation_threshold)  {status <- "HV not activated"}
    
    # When a low-value agent gets activated
    if (original_agent == 1 & activation_score > activation_threshold) {
      status <- "LV activated"
      agent_matrix[agent_x, agent_y] <- 1
    } else if (original_agent == 1 & activation_score <= activation_threshold) {status <- "LV not activated"}
    
    # When an inactive agent gets activated
    if (original_agent == 0 & activation_score > activation_threshold) {
      status <- "IA activated"
      agent_matrix[agent_x, agent_y] <- 1
    } else if (original_agent == 0 & activation_score <= activation_threshold)  {status <- "IA not activated" } 
    
    activated_agents[i] <- sum(agent_matrix != 0)
    
    
    ### Creating simulation ###
    image(
      agent_matrix,
      main = paste("Step:", i, "Activation:", status),
      sub = paste("Black = Inactive; Orange = No-value active", "\n", "Green = High-value active; Blue: High-value activated"),
      col = colour_palette
    )
    Sys.sleep(0.5)
    
  }
  
  ### Plot ###
  dev.new()
  plot(
    activated_agents, type = "l",
    main = "Number of Active Agents Over Time",
    xlab = "Time",
    ylab = "Number of Active Agents"
  )
  
}


agent_sim(15, 1000)
