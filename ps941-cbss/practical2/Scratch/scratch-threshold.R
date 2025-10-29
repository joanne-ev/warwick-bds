# Setup the number of cycles in the simulation
cycle <- 10000

# Define the grid size
grid_size <- 32

# Function to get the sum of thresholds of immediate neighbours.
get_neighbour_sum <- function(x, y, grid) {
  neighbours <- c(
    grid[x-1, y], grid[x+1, y],
    grid[x, y-1], grid[x, y+1],
    grid[x-1,y-1], grid[x+1,y+1],
    grid[x-1,y+1], grid[x+1,y-1]
  )
  return(sum(neighbours, na.rm = TRUE))
}


threshold_sim <- function(grid_size, cycle = 10000, alpha1, beta2) {
  
  # Create a matrix of agents
  activity <- matrix(0, nrow = grid_size, ncol = grid_size)
  
  # Randomly allocate some agents to be active from the start
  activity[sample(1:(grid_size^2), 100)] <- 1
  
  # Store number of activated agents
  activated_agents <- vector()
  
  # Threshold distribution
  distribution <- matrix(rbeta(n = grid_size^2, shape1 = alpha1, shape2 = beta2),
                         nrow = grid_size, ncol = grid_size)
  
  ### Simulation ###
  for (i in seq(cycle)) {
    # Identify random agent
    agent_x <- sample(2:(grid_size-1), 1)
    agent_y <- sample(2:(grid_size-1), 1)
    
    # Calculate sum of thresholds of immediate neighbours
    neighbour_sum <- get_neighbour_sum(agent_x, agent_y, activity)
    
    # Activate agent if their threshold is below the proportion of surrounding active agents.
    if (distribution[agent_x, agent_y] < (neighbour_sum / 8)) {
      activity[agent_x, agent_y] <- 1
    }
    
    # Store number of activated agents for each cycle
    activated_agents[i] <- sum(activity)
  }
  
  # Plot the results
  plot(
    activated_agents, type = "l",
    main = "Number of Active Agents Over Time",
    sub = paste("Randomly selected values from the Beta distribution of Alpha", alpha1, "and Beta", beta2),
    xlab = "Time",
    ylab = "Number of Active Agents"
  )
}

set.seed(1)
threshold_sim(grid_size, cycle, alpha1 = 5, beta2 = 5)
