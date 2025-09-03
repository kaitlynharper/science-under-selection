##############################################################################
# Toy Model: Bayesian Inference Simulator (Rshiny)
# Description: Interactive app to explore Bayesian updating with
# normal prior and likelihood. Computes the posterior and KL-based
# metrics (verisimilitude change, information gain), and renders
# distribution plots.
#
# Inputs (UI):
#   - True Effect Mean/SD, Prior Mean/SD, Study Mean/SE
# Outputs:
#   - Text summary (KL metrics) and plot
##############################################################################

# Load required packages
library(shiny) #Rshiny app
library(shinyWidgets) #Rshiny formatting for inputs 
library(philentropy) #KL divergence

# Define the User Interface (UI)
ui <- fluidPage(
  titlePanel("Toy Model: Bayesian Inference Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "scenario",
        label = "Scenario:",
        choices = c(
          "Select..." = "",
          "First study of true effect",
          "First study of null effect",
          "Replication with same result (accurate)",
          "Replication with same result (inaccurate)"
        ),
        selected = ""
      ),

      h3("Input Parameters"),
      # True effect parameters
      h4("True Effect Distribution"),
      autonumericInput(
        inputId = "true_effect_mean", label = "True Effect Mean:", value = 0.0,
        decimalCharacter = ".", digitGroupSeparator = ",", decimalPlaces = 2,
        align = "left"
      ),
      autonumericInput(
        inputId = "true_effect_sd", label = "True Effect SD:", value = 0.01,
        decimalCharacter = ".", digitGroupSeparator = ",", decimalPlaces = 2,
        align = "left"
      ),
      
      # Prior parameters
      h4("Prior Distribution"),
      autonumericInput(
        inputId = "prior_mean", label = "Prior Mean:", value = 0.0,
        decimalCharacter = ".", digitGroupSeparator = ",", decimalPlaces = 2,
        align = "left"
      ),
      autonumericInput(
        inputId = "prior_sd", label = "Prior SD:", value = 1.0,
        decimalCharacter = ".", digitGroupSeparator = ",", decimalPlaces = 2,
        align = "left"
      ),
      
      # Study parameters
      h4("Study Results"),
      autonumericInput(
        inputId = "study_effect", label = "Study Mean:", value = 0.5,
        decimalCharacter = ".", digitGroupSeparator = ",", decimalPlaces = 2,
        align = "left"
      ),
      autonumericInput(
        inputId = "study_se", label = "Study SE:", value = 0.2,
        decimalCharacter = ".", digitGroupSeparator = ",", decimalPlaces = 2,
        align = "left"
      )
    ),
    
    mainPanel(
      h3("Results"),
      verbatimTextOutput("results"),
      plotOutput("dist_plot")
    )
  )
)

# Define the Server logic
server <- function(input, output, session) {
  
  # Preset scenarios: users can still change inputs after selection
  scenario_defaults <- list(
    "First study of true effect" = list(
      true_effect_mean = 1.0, true_effect_sd = 0.01,
      prior_mean = 0.0, prior_sd = 1.0,
      study_effect = 1.5, study_se = 0.5
    ),
    "First study of null effect" = list(
      true_effect_mean = 0.0, true_effect_sd = 0.01,
      prior_mean = 0.0, prior_sd = 1.0,
      study_effect = 1.5, study_se = 0.5
    ),
    "Replication with same result (accurate)" = list(
      true_effect_mean = 0.5, true_effect_sd = 0.01,
      prior_mean = 0.5, prior_sd = 0.4,
      study_effect = 0.5, study_se = 0.5
    ),
    "Replication with same result (inaccurate)" = list(
      true_effect_mean = 1.0, true_effect_sd = 0.01,
      prior_mean = 0.5, prior_sd = 0.4,
      study_effect = 0.5, study_se = 0.5
    )
  )

  # Handle scenario selection: when user picks a scenario from dropdown,
  # automatically populate the input fields with preset values for that scenario
  observeEvent(input$scenario, {
    req(input$scenario != "")  # Only proceed if a scenario is actually selected
    defaults <- scenario_defaults[[input$scenario]]  # Get the preset values for this scenario
    if (is.null(defaults)) return(NULL)  # Safety check: exit if scenario not found
    
    # Update numeric fields; users can still change them manually afterwards
    shinyWidgets::updateAutonumericInput(session, "true_effect_mean", value = defaults$true_effect_mean)
    shinyWidgets::updateAutonumericInput(session, "true_effect_sd", value = defaults$true_effect_sd)
    shinyWidgets::updateAutonumericInput(session, "prior_mean", value = defaults$prior_mean)
    shinyWidgets::updateAutonumericInput(session, "prior_sd", value = defaults$prior_sd)
    shinyWidgets::updateAutonumericInput(session, "study_effect", value = defaults$study_effect)
    shinyWidgets::updateAutonumericInput(session, "study_se", value = defaults$study_se)
  })

  # This function runs every time the inputs change
  output$results <- renderText({
    # Extract input values
    true_effect_mean <- input$true_effect_mean
    true_effect_sd <- input$true_effect_sd
    prior_mean <- input$prior_mean
    prior_sd <- input$prior_sd
    study_effect <- input$study_effect
    study_se <- input$study_se
    
    # Calculate posterior
    likelihood_mean <- study_effect
    likelihood_sd <- study_se
    prior_var <- prior_sd^2
    likelihood_var <- likelihood_sd^2
    posterior_var <- 1 / (1/prior_var + 1/likelihood_var)
    posterior_mean <- (prior_mean/prior_var + likelihood_mean/likelihood_var) * posterior_var
    posterior_sd <- sqrt(posterior_var)
    
    # Calculate KL-divergence
    # Calculation explanation: https://stats.stackexchange.com/questions/7440/kl-divergence-between-two-univariate-gaussians?newreg=3562e6f5a18544b8bf7b629d2d952da2
    kl_norm <- function(mu0, sd0, mu1, sd1) {
      log(sd1 / sd0) + (sd0^2 + (mu0 - mu1)^2) / (2 * sd1^2) - 0.5
    }
    kl_prior_to_true <- kl_norm(true_effect_mean, true_effect_sd, prior_mean, prior_sd)
    kl_posterior_to_true <- kl_norm(true_effect_mean, true_effect_sd, posterior_mean, posterior_sd)
    verisimilitude_change <- kl_prior_to_true - kl_posterior_to_true
    information_gain <- kl_norm(posterior_mean, posterior_sd, prior_mean, prior_sd)
    
    # Format results with all metrics
    sprintf(
      "%s\n\n%s\n\n%s\n\n%s",
      paste0("Prior verisimilitude: ", round(kl_prior_to_true, 3)),
      paste0("Posterior verisimilitude: ", round(kl_posterior_to_true, 3)),
      paste0("True contribution (verisimilitude gain): ", round(verisimilitude_change, 3)),
      paste0("Perceived contribution (information gain): ", round(information_gain, 3))
    )
  })
  
  # Combined plot: true effect (black), study likelihood (gray), prior (blue), posterior (green)
  output$dist_plot <- renderPlot({
    # Extract input values for plotting
    true_effect_mean <- input$true_effect_mean
    true_effect_sd <- input$true_effect_sd
    prior_mean <- input$prior_mean
    prior_sd <- input$prior_sd
    study_effect <- input$study_effect
    study_se <- input$study_se
    
    # Calculate posterior for plotting
    likelihood_mean <- study_effect
    likelihood_sd <- study_se
    
    prior_var <- prior_sd^2
    likelihood_var <- likelihood_sd^2
    
    posterior_var <- 1 / (1/prior_var + 1/likelihood_var)
    posterior_mean <- (prior_mean/prior_var + likelihood_mean/likelihood_var) * posterior_var
    posterior_sd <- sqrt(posterior_var)
    
    # Create x-axis for plotting
    x_plot <- seq(-3, 3, length.out = 1000)
    
    # Precompute densities
    y_true <- dnorm(x_plot, mean = true_effect_mean, sd = true_effect_sd)
    y_prior <- dnorm(x_plot, mean = prior_mean, sd = prior_sd)
    y_likelihood <- dnorm(x_plot, mean = likelihood_mean, sd = likelihood_sd)
    y_posterior <- dnorm(x_plot, mean = posterior_mean, sd = posterior_sd)

    # Calculate the maximum density for y-axis, ignoring the true effect (often very peaked)
    max_density <- max(y_prior, y_likelihood, y_posterior)
    
    # Create the plot with dynamic y-axis and draw curves
    plot(x_plot, y_true, 
         type = "l", col = "black", lwd = 2, 
         xlab = "Effect Size", ylab = "Density",
         main = "True Effect, Study Estimate, Prior, and Posterior Distributions",
         ylim = c(0, max_density * 1.1))

    # Shade under curves with 20% opacity
    polygon(c(x_plot, rev(x_plot)), c(rep(0, length(y_true)), rev(y_true)),
            col = adjustcolor("black", alpha.f = 0.2), border = NA)
    polygon(c(x_plot, rev(x_plot)), c(rep(0, length(y_prior)), rev(y_prior)),
            col = adjustcolor("blue", alpha.f = 0.2), border = NA)
    polygon(c(x_plot, rev(x_plot)), c(rep(0, length(y_likelihood)), rev(y_likelihood)),
            col = adjustcolor("gray", alpha.f = 0.2), border = NA)
    polygon(c(x_plot, rev(x_plot)), c(rep(0, length(y_posterior)), rev(y_posterior)),
            col = adjustcolor("green", alpha.f = 0.2), border = NA)

    # Re-draw lines on top for clarity
    lines(x_plot, y_prior, col = "blue", lwd = 2)
    lines(x_plot, y_likelihood, col = "gray", lwd = 2)
    lines(x_plot, y_posterior, col = "green", lwd = 2)
    lines(x_plot, y_true, col = "black", lwd = 2)
    
    # Add legend
    legend("topleft", 
           legend = c("True Effect", "Study Estimate", "Prior", "Posterior"),
           col = c("black", "gray", "blue", "green"), 
           lwd = 2, 
           bty = "n")
    
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

#To open app in R terminal - setwd and run app
#setwd("/Users/kaitlynharper/Documents/Toy_model")
#shiny::runApp()
