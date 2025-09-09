##############################################################################
# Model
#
# Description: This file establishes the run_simulation function
##############################################################################

# Simulation function
run_simulation <- function(
  n_agents,
  n_timesteps,
  papers_per_agent_per_timestep
) {
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
  latest_paper_index <- 1

  #Initialize matrix of agents

  # Each timestep
  for (timestep in 1:n_timesteps) {
    # Generate paper IDs for this timestep
    paper_ids <- latest_paper_index:(latest_paper_index +
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
      latest_paper_index:(latest_paper_index +
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

    latest_paper_index <- latest_paper_index +
      (papers_per_agent_per_timestep * n_agents)
  }

  # Return database
  return(papers)
}
