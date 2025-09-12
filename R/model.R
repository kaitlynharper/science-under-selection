##############################################################################
# Model
#
# Description: This file establishes the run_simulation function
##############################################################################

# Simulation function
run_simulation <- function(
  n_agents,
  n_timesteps,
  n_timesteps_per_career_step,
  mean_papers_per_agent_per_timestep
) {
  #### Initialize model ####
  #Initialize matrix of papers
  # Calculate total number of papers
  total_papers <- n_agents * n_timesteps * (mean_papers_per_agent_per_timestep+1)
  papers <- matrix(0, nrow = total_papers, ncol = 8)
  # Set column names
  colnames(papers) <- c(
    "paper_ID",
    "timestep",
    "author_ID",
    "hypothesis_ID",
    "estimated_m",
    "estimated_se",
    "verisimilitude_gain",
    "information_gain"
  )
  # Track current paper (ID and index should be the same for papers throughout)
  next_paper_ID <- 1

  #Initialize matrix of agents
  # Calculate total number of agents
  total_agents <- n_agents *
    (floor(n_timesteps / n_timesteps_per_career_step) + 1)
  agents <- matrix(0, nrow = total_agents, ncol = 5)
  # Set column names
  colnames(agents) <- c(
    "researcher_ID",
    "timestep",
    "prob_replicate",
    "career_level",
    "timesteps_in_career_level"
  )
  # Track current agent
  next_agent_ID <- 1
  next_agent_index <- 1

  # Generate initial population of agents
  # Set up initial current_agents
  current_agents <- matrix(0, nrow = n_agents, ncol = 6)
  # Set column names
  colnames(current_agents) <- c(
    "researcher_ID",
    "timestep",
    "prob_replicate",
    "career_level",
    "timesteps_in_career_level",
    "total_papers"
  )
  # Generate IDs
  researcher_IDs <- next_agent_ID:(next_agent_ID + n_agents - 1)
  # Set timestep
  timesteps <- rep(0, n_agents)
  # Generate prob_replicate (between 0 and 1)
  prob_replicates <- pmax(0, pmin(1, rnorm(n_agents, 0.5, 0.5)))
  # Assign career level
  career_levels <- rep(1:5, length.out = n_agents)
  # Assign timesteps agents have been in current career level
  timesteps_in_career_levels <- sample(1:10, n_agents, replace = TRUE)

  # Fill agents matrix with these values
  # Add to matrix
  current_agents[
    next_agent_index:(next_agent_index +
      (length(researcher_IDs)) -
      1),
  ] <-
    cbind(
      researcher_IDs,
      timesteps,
      prob_replicates,
      career_levels,
      timesteps_in_career_levels
    )
  # Update next agent ID
  next_agent_ID <- next_agent_ID + length(researcher_IDs)

  #### Timestep loop ####
  for (timestep in 0:n_timesteps) {
    # Update timestep infor for current_agents
    current_agents[, "timestep"] <- timestep
    current_agents[, "timesteps_in_career_level"] <- current_agents[, "timesteps_in_career_level"] + 1

    # Save current agent info
    current_n_agents <- nrow(current_agents)
    current_researcher_IDs <- current_agents[, "researcher_ID"]

    # Decide how many papers for each agent using Poisson distribution
    papers_per_agent <- rpois(current_n_agents, mean_papers_per_agent_per_timestep)
    total_papers_this_timestep <- sum(papers_per_agent)

    # Generate list of author IDs
    author_IDs <- rep(current_researcher_IDs, papers_per_agent)

    # Generate timestep list
    timesteps <- rep(timestep, total_papers_this_timestep)
    
    # Generate paper IDs
    paper_IDs <- next_paper_ID:(next_paper_ID + total_papers_this_timestep - 1)

    # Generate hypothesis IDs
    hypothesis_IDs <- sample(
      1:100000,
      total_papers_this_timestep,
      replace = FALSE
    )

    # Generate effect size values
    estimated_m <- rnorm(
      total_papers_this_timestep,
      0.5,
      0.2
    )

    # Generate SE values
    estimated_se <- abs(rnorm(
      total_papers_this_timestep,
      0.1,
      0.05
    ))

    # Generate verisimilitude gain
    verisimilitude_gain <- rnorm(
      total_papers_this_timestep,
      0.2,
      0.1
    )

    # Generate information gain
    information_gain <- rnorm(
      total_papers_this_timestep,
      0.3,
      0.1
    )

    # Add to matrix
    papers[
      next_paper_ID:(next_paper_ID + total_papers_this_timestep - 1),
    ] <-
      cbind(
        paper_IDs,
        timesteps,
        author_IDs,
        hypothesis_IDs,
        estimated_m,
        estimated_se,
        verisimilitude_gain,
        information_gain
      )
    # Update next paper ID
    next_paper_ID <- next_paper_ID + total_papers_this_timestep
  }

  # Return list of results with papers
  return(list(
    papers = papers,
    agents = agents
  ))
}
