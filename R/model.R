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
  papers_per_agent_per_timestep
) {
  #### Initialize model ####
  #Initialize matrix of papers
  # Calculate total number of papers
  total_papers <- n_agents * n_timesteps * papers_per_agent_per_timestep
  papers <- matrix(0, nrow = total_papers, ncol = 6)
  # Set column names
  colnames(papers) <- c(
    "paper_ID",
    "hypothesis_ID",
    "estimated_m",
    "estimated_se",
    "verisimilitude_gain",
    "information_gain"
  )
  # Track current paper (ID and index are the same)
  latest_paper_ID <- 1

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
  latest_agent_ID <- 1
  latest_agent_index <- 1

  # Generate initial population of agents
  # Generate IDs
  researcher_IDs <- latest_agent_ID:(latest_agent_ID + n_agents - 1)

  # Set timestep
  timesteps <- rep(1, n_agents)

  # Generate prob_replicate (between 0 and 1)
  prob_replicates <- pmax(0, pmin(1, rnorm(n_agents, 0.5, 1)))

  # Assign career level
  career_levels <- rep(1:5, length.out = n_agents)

  # Assign timesteps agents have been in current career level
  timesteps_in_career_levels <- sample(1:10, n_agents, replace = TRUE)

  # Fill agents matrix with these values
  # Add to matrix
  agents[
    latest_agent_index:(latest_agent_index +
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
  # Update latest agent ID and index
  latest_agent_index <- latest_agent_index +
    (length(researcher_IDs) - 1)
  latest_agent_ID <- latest_agent_ID +
    (length(researcher_IDs) - 1)

  #### Timestep loop ####
  for (timestep in 1:n_timesteps) {
    # Generate paper IDs for this timestep
    paper_ids <- latest_paper_ID:(latest_paper_ID +
      (papers_per_agent_per_timestep * n_agents) -
      1)

    # Generate hypothesis IDs for this timestep
    hypothesis_ids <- sample(
      1:100000,
      (papers_per_agent_per_timestep * n_agents),
      replace = FALSE
    )

    # Generate effect sizes for this timestep
    estimated_m <- rnorm(
      (papers_per_agent_per_timestep * n_agents),
      0.5,
      0.2
    )

    # Generate SE for this timestep
    estimated_se <- abs(rnorm(
      (papers_per_agent_per_timestep * n_agents),
      0.1,
      0.05
    ))

    # Generate verisimilitude gain for this timestep
    verisimilitude_gain <- rnorm(
      (papers_per_agent_per_timestep * n_agents),
      0.2,
      0.1
    )

    # Generate information gain for this timestep
    information_gain <- rnorm(
      (papers_per_agent_per_timestep * n_agents),
      0.3,
      0.1
    )

    # Add to matrix
    papers[
      latest_paper_ID:(latest_paper_ID +
        (papers_per_agent_per_timestep * n_agents) -
        1),
    ] <-
      cbind(
        paper_ids,
        hypothesis_ids,
        estimated_m,
        estimated_se,
        verisimilitude_gain,
        information_gain
      )
    # Update latest paper ID
    latest_paper_ID <- latest_paper_ID +
      (papers_per_agent_per_timestep * n_agents)
  }

  # Return list of results with papers
  return(list(
    papers = papers,
    agents = agents
  ))
}
