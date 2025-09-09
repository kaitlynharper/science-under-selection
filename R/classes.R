##############################################################################
# Agent-Based Model: Class Definitions
#
# This script contains the R6 class definitions for an agent-based model.
# The model includes four main classes:
#   - agents_list: Holds a collection of agents
#   - papers_list: Holds a collection of papers
#   - effects_list: Holds a collection of effects
#   - model: Holds all components of the model
#
##############################################################################

# Load required libraries
library(R6)
library(polars)

#### agents_list Class ####
# Holds a collection of agents in the model
agents_list <- R6Class("agents_list",
  public = list(
    # Constructor
    initialize = function() {
      # Initialize empty polars DataFrame with specified columns
      private$.agents_df <- pl$DataFrame(
        timestep = as.integer(integer(0)),
        agent_ID = as.integer(integer(0)),
        prob_replicate = as.numeric(numeric(0)),
        
        
        career_level = as.integer(integer(0)),
        time_in_career_level = as.integer(integer(0))
      )
    },

    # Get the agents DataFrame
    get_agents = function() {
      return(private$.agents_df)
    },

    # Fill with initial population of agents (generate new filled dataframe)
    fill_initial_agents = function(n_agents, timestep = 1, prob_replicate_values) {
      # Create DataFrame with all agents with specified traits/values
      private$.agents_df <- pl$DataFrame(
        timestep = as.integer(rep(timestep, n_agents)),
        agent_ID = as.integer(1:n_agents),
        prob_replicate = as.numeric(prob_replicate_values),
        career_level = as.integer(rep(0, n_agents)),
        time_in_career_level = as.integer(rep(0, n_agents))
      )
    },

    # Add a new agent
    add_agent = function(timestep, prob_replicate, career_level, time_in_career_level) {
      # Create new agent DataFrame
      new_agent <- pl$DataFrame(
        timestep = as.integer(timestep),
        agent_ID = as.integer(self$get_next_agent_id()),
        prob_replicate = as.numeric(prob_replicate),
        career_level = as.integer(career_level),
        time_in_career_level = as.integer(time_in_career_level)
      )

      if (private$.agents_df$height == 0) {
        private$.agents_df <- new_agent
      } else {
        # Use private helper method to handle concatenation to agent_list df
        private$.append_agent(new_agent)
      }
    },

    # Get the next available agent ID
    get_next_agent_id = function() {
      if (private$.agents_df$height == 0) return(1L)
      # Convert to R data.frame first, then get max
      # TODO: avoid the conversion here 
      r_df <- as.data.frame(private$.agents_df)
      max_id <- max(r_df$agent_ID)
      return(as.integer(max_id) + 1L)
    }

  ),

  private = list(
    # Private field to store the agents DataFrame
    .agents_df = NULL,

    # Private helper method to append agent data
    .append_agent = function(new_agent) {
      private$.agents_df <- pl$concat(private$.agents_df, new_agent, how = "vertical")
    }
  )
)

#### papers_list Class ####
# Holds a collection of papers in the model
papers_list <- R6Class("papers_list",
  public = list(
    # Constructor
    initialize = function() {
      # TODO: Initialize papers collection
    }

    # TODO: Add methods here
  ),

  private = list(
    # TODO: Add private fields and methods here
  )
)

#### effects_list Class ####
# Holds a collection of effects in the model
effects_list <- R6Class("effects_list",
  public = list(
    # Constructor
    initialize = function() {
      # TODO: Initialize effects collection
    }

    # TODO: Add methods here
  ),

  private = list(
    # TODO: Add private fields and methods here
  )
)

#### model Class ####
# Holds all components of the model
model <- R6Class("model",
  public = list(
    # Constructor
    initialize = function() {
      # TODO: Initialize model components? Call the other classes' methods here?
    }

    # TODO: Add methods here
  ),

  private = list(
    # TODO: Add private fields and methods here
  )
)
