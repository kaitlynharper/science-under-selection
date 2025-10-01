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

  # Initialize empty papers matrix
  papers <- initialize_papers_matrix(
    n_agents,
    n_timesteps,
    mean_papers_per_agent_per_timestep
  )

  # Initialize empty agents matrix
  agents <- initialize_agents_matrix(
    n_agents,
    n_timesteps,
    n_timesteps_per_career_step
  )

  # Generate initial population of agents
  current_agents <- generate_initial_agents(
    n_agents
  )

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

    # Career dynamics
    if (timestep %% n_timesteps_per_career_step == 0) {
      # Count papers by agent
      if (next_paper_ID > 1) {
        # Extract author IDs from filled rows
        author_ids <- papers[1:(next_paper_ID - 1), "author_ID"]
        # Count papers per author using tabulate
        all_counts <- tabulate(author_ids)
        # Map counts to current agents (0 for agents with no papers)
        current_agents[, "total_papers"] <- all_counts[current_agents[, "researcher_ID"]]
        current_agents[is.na(current_agents[, "total_papers"]), "total_papers"] <- 0
      } else {
        # No papers yet, all counts are 0
        current_agents[, "total_papers"] <- 0
      }
      # Save current agents to the agents matrix
      agents[
        next_agent_index:(next_agent_index + current_n_agents - 1),
      ] <- current_agents
      # Update agents matrix index tracker 
      next_agent_index <- next_agent_index + current_n_agents
      # Retire some agents
      # Fill each career level by choosing agents beneath based on metrics
      # Generate new agents to fill lowest level
      # ^ Remember to update next_agent_ID when doing this
    }
  }

  # Return list of results with papers
  return(list(
    papers = papers,
    agents = agents
  ))
}
