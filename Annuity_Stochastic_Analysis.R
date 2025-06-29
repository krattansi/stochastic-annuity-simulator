library(shiny)
library(ggplot2)

# Simulate interest rates using Vasicek model
# TODO: maybe add other models later (CIR, Hull-White?)
vasicek_sim <- function(r_init, mean_revert_speed, long_term_rate, vol, years, time_step, num_paths) {
  steps <- years / time_step
  rate_matrix <- matrix(NA, nrow = num_paths, ncol = steps + 1)
  rate_matrix[, 1] <- r_init
  
  for (i in 2:(steps + 1)) {
    random_shocks <- rnorm(num_paths)
    # Vasicek SDE discretization
    rate_matrix[, i] <- rate_matrix[, i-1] + 
      mean_revert_speed * (long_term_rate - rate_matrix[, i-1]) * time_step + 
      vol * sqrt(time_step) * random_shocks
  }
  
  return(rate_matrix)
}

# Calculate present value of cash flows
pv_calculation <- function(interest_rates, dt, payments) {
  num_sims <- nrow(interest_rates)
  num_periods <- length(payments)
  present_values <- rep(0, num_sims)
  
  for (sim in 1:num_sims) {
    # Get discount factors from cumulative rates
    disc_factors <- exp(-cumsum(interest_rates[sim, 2:(num_periods + 1)]) * dt)
    present_values[sim] <- sum(payments * disc_factors)
  }
  
  return(present_values)
}

# Setup cash flows based on annuity type
setup_cashflows <- function(annuity_type, pmt, term, defer_years = 0, growth_rate = 0) {
  if (annuity_type == "Level") {
    cashflows <- rep(pmt, term)
  } else if (annuity_type == "Deferred") {
    # No payments during deferral period
    cashflows <- c(rep(0, defer_years), rep(pmt, term - defer_years))
  } else if (annuity_type == "Growing") {
    # Geometric growth
    cashflows <- pmt * (1 + growth_rate)^(0:(term - 1))
  } else {
    stop("Invalid annuity type specified")
  }
  
  return(cashflows)
}

# UI Layout
ui <- fluidPage(
  titlePanel("Annuity Valuation with Stochastic Interest Rates"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Annuity Parameters"),
      selectInput("annuity_type", "Type:", 
                  choices = c("Level", "Deferred", "Growing")),
      
      # Show deferral input only for deferred annuities
      conditionalPanel(
        condition = "input.annuity_type == 'Deferred'",
        numericInput("deferral", "Deferral Period:", 
                     value = 5, min = 1, max = 25)
      ),
      
      # Growth rate for growing annuities
      conditionalPanel(
        condition = "input.annuity_type == 'Growing'",
        numericInput("growth", "Growth Rate (%):", 
                     value = 3, min = 0, max = 20, step = 0.5)
      ),
      
      numericInput("payment", "Payment Amount:", value = 1000, min = 100),
      sliderInput("T", "Term (years):", min = 5, max = 30, value = 10),
      
      br(),
      h4("Vasicek Model Parameters"),
      sliderInput("r0", "Initial Rate:", min = 0.01, max = 0.10, 
                  value = 0.03, step = 0.005),
      sliderInput("a", "Mean Reversion Speed:", min = 0.01, max = 0.5, 
                  value = 0.1, step = 0.01),
      sliderInput("b", "Long-term Rate:", min = 0.01, max = 0.10, 
                  value = 0.04, step = 0.005),
      sliderInput("sigma", "Volatility:", min = 0.001, max = 0.05, 
                  value = 0.01, step = 0.001),
      
      br(),
      sliderInput("n_sims", "# Simulations:", min = 500, max = 10000, 
                   value = 2000, step = 500),
      
      actionButton("go", "Run Analysis", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Interest Rate Paths", plotOutput("ratePaths", height = "500px")),
        tabPanel("PV Distribution", plotOutput("pvHist", height = "500px")),
        tabPanel("Risk Metrics", verbatimTextOutput("summaryStats")),
        tabPanel("vs. Deterministic", verbatimTextOutput("compareDeterministic")),
        tabPanel("Cash Flows", plotOutput("cashFlowChart", height = "400px"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive values to store simulation results
  sim_results <- reactiveValues(
    rates = NULL,
    pv_values = NULL,
    cash_flows = NULL
  )
  
  observeEvent(input$go, {
    # Get parameters with some validation
    dt <- 1.0  # annual time step
    growth_pct <- ifelse(is.null(input$growth), 0, input$growth / 100)
    defer_period <- ifelse(is.null(input$deferral), 0, input$deferral)
    
    # Generate cash flows
    cf <- setup_cashflows(input$annuity_type, input$payment, input$T, 
                          defer_period, growth_pct)
    
    # Run Vasicek simulation
    rate_paths <- vasicek_sim(input$r0, input$a, input$b, input$sigma, 
                              input$T, dt, input$n_sims)
    
    # Calculate PVs
    pv_results <- pv_calculation(rate_paths, dt, cf)
    
    # Store results
    sim_results$rates <- rate_paths
    sim_results$pv_values <- pv_results
    sim_results$cash_flows <- cf
  })
  
  # Plot sample rate paths
  output$ratePaths <- renderPlot({
    if (is.null(sim_results$rates)) return(NULL)
    
    # Show first 20 paths max to avoid clutter
    paths_to_show <- min(20, input$n_sims)
    years <- 0:input$T
    
    # Use base R plotting for simplicity
    matplot(years, t(sim_results$rates[1:paths_to_show, ]), 
            type = "l", lty = 1, lwd = 1.5,
            main = paste("Sample Interest Rate Paths (", paths_to_show, "of", input$n_sims, ")"),
            xlab = "Year", ylab = "Interest Rate",
            col = rainbow(paths_to_show, alpha = 0.7))
    
    # Add mean path
    mean_path <- colMeans(sim_results$rates)
    lines(years, mean_path, col = "black", lwd = 3, lty = 2)
    legend("topright", legend = "Mean Path", col = "black", lwd = 3, lty = 2)
  })
  
  # PV histogram
  output$pvHist <- renderPlot({
    if (is.null(sim_results$pv_values)) return(NULL)
    
    mean_pv <- mean(sim_results$pv_values)
    
    ggplot(data.frame(pv = sim_results$pv_values), aes(x = pv)) +
      geom_histogram(bins = 40, fill = "lightblue", color = "darkblue", alpha = 0.7) +
      geom_vline(xintercept = mean_pv, color = "red", size = 1.2, linetype = "dashed") +
      annotate("text", x = mean_pv, y = Inf, vjust = 1.5, 
               label = paste("Mean =", round(mean_pv, 0)), color = "red") +
      labs(title = "Distribution of Present Values",
           x = "Present Value ($)", y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # Summary statistics
  output$summaryStats <- renderPrint({
    if (is.null(sim_results$pv_values)) {
      cat("Run simulation first!")
      return()
    }
    
    pv <- sim_results$pv_values
    
    cat("=== PRESENT VALUE STATISTICS ===\n")
    cat("Mean PV:        $", format(round(mean(pv), 2), big.mark = ","), "\n")
    cat("Std Dev:        $", format(round(sd(pv), 2), big.mark = ","), "\n")
    cat("Min PV:         $", format(round(min(pv), 2), big.mark = ","), "\n")
    cat("Max PV:         $", format(round(max(pv), 2), big.mark = ","), "\n")
    cat("\n=== RISK METRICS ===\n")
    cat("95% VaR:        $", format(round(quantile(pv, 0.05), 2), big.mark = ","), "\n")
    cat("95% CVaR:       $", format(round(mean(pv[pv <= quantile(pv, 0.05)]), 2), big.mark = ","), "\n")
    cat("99% VaR:        $", format(round(quantile(pv, 0.01), 2), big.mark = ","), "\n")
    
    # Additional percentiles
    cat("\n=== PERCENTILES ===\n")
    percentiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
    for (p in percentiles) {
      cat(sprintf("%2.0f%% percentile: $%s\n", p * 100, 
                  format(round(quantile(pv, p), 2), big.mark = ",")))
    }
  })
  
  # Compare with deterministic calculation
  output$compareDeterministic <- renderPrint({
    if (is.null(sim_results$pv_values)) {
      cat("Run simulation first!")
      return()
    }
    
    # Simple deterministic calc using initial rate
    cf <- sim_results$cash_flows
    fixed_pv <- sum(cf / (1 + input$r0)^(1:length(cf)))
    stoch_pv <- mean(sim_results$pv_values)
    
    cat("=== DETERMINISTIC vs STOCHASTIC COMPARISON ===\n\n")
    cat("Fixed Rate PV:     $", format(round(fixed_pv, 2), big.mark = ","), "\n")
    cat("Stochastic Mean:   $", format(round(stoch_pv, 2), big.mark = ","), "\n")
    cat("Difference:        $", format(round(stoch_pv - fixed_pv, 2), big.mark = ","), "\n")
    cat("Relative Diff:     ", round((stoch_pv - fixed_pv) / fixed_pv * 100, 2), "%\n")
    
    if (stoch_pv > fixed_pv) {
      cat("\n→ Stochastic valuation is HIGHER than fixed-rate\n")
    } else {
      cat("\n→ Stochastic valuation is LOWER than fixed-rate\n")
    }
  })
  
  # Cash flow visualization
  output$cashFlowChart <- renderPlot({
    if (is.null(sim_results$cash_flows)) return(NULL)
    
    cf_data <- data.frame(
      Year = 1:length(sim_results$cash_flows),
      Payment = sim_results$cash_flows
    )
    
    ggplot(cf_data, aes(x = Year, y = Payment)) +
      geom_col(fill = "steelblue", width = 0.7) +
      geom_text(aes(label = paste0("$", format(round(Payment, 0), big.mark = ","))), 
                vjust = -0.5, size = 3) +
      labs(title = paste(input$annuity_type, "Annuity - Payment Schedule"),
           x = "Year", y = "Payment Amount ($)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      scale_y_continuous(labels = scales::dollar_format())
  })
}

# Run the app
shinyApp(ui = ui, server = server)
