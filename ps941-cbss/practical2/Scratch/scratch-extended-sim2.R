library(tidyverse)
library(reshape2)


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
agent_sim <- function(grid_size, cycle, hv_prop = 0.10, beta_dist_shape = 5) {
	activated_agents <- vector()
	dev.new()
	colour_palette <- c("black", "orange", "green")
	
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
				agent_matrix[agent_x, agent_y] <- 2
			} else if (original_agent == 2 & activation_score <= activation_threshold)  {status <- "HV not activated"}
		
		# If a low-value agent gets activated
		if (original_agent == 1 & activation_score > activation_threshold) {
				status <- "LV influenced"
				agent_matrix[agent_x, agent_y] <- 1
			} else if (original_agent == 1 & activation_score <= activation_threshold) {status <- "LV not activated"}
		
		# If an inactive agent gets activated
		if (original_agent == 0 & activation_score > activation_threshold) {
				status <- "IA activated"
				agent_matrix[agent_x, agent_y] <- 1
			} else if (original_agent == 0 & activation_score <= activation_threshold)  {status <- "IA not activated" } 
		
		activated_agents[i] <- sum(agent_matrix != 0)
		
		
		### Creating simulation ###
		image(
			agent_matrix,
			main = paste("Step:", i, "Activation:", status),
			sub = paste0("Black: Inactive (n = ", ia_count, ")",
			             "\n", "Orange: Low-value active (n = ", lv_count, ")",
			             "\n", "Green: High-value active (n = ", hv_count, ")"),
			col = colour_palette
		)
		
		Sys.sleep(0.05)
		
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



# agent_sim(grid_size = 36, cycle = 5000, hv_prop = 0.25)




# Measure the time it takes to run the simulation
time_taken <- list()

for (hv in seq(0, 1, by = 0.2)) {
  time_taken[[as.character(hv)]] <- system.time({agent_sim(grid_size = 36, cycle = 1000, hv_prop = 0.25)})
}

time_taken
