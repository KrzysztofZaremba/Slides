library(shiny)
library(plotly)


z_score <- function(alpha) {
  qnorm(1 - alpha)
}

# Function to calculate power of the test
calculate_power <- function(mean_null, mean_alt, sigma, n, alpha) {
  z_alpha <- qnorm(1 - alpha)
  z_beta <- (mean_alt - mean_null) / (sigma / sqrt(n)) - z_alpha
  power <- 1 - pnorm(z_beta)
  return(power)
}

# Define UI
ui <- navbarPage("Statistical Test Visualizations",  
                 tabPanel("Type 1 & 2 Errors",
                 fluidPage(
  titlePanel("Visualizing Type 1 and Type 2 Errors with Plotly"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mean_alt",
                  "Mean of Alternative Distribution",
                  min = 0, max = 10, value = 3),
      sliderInput("alpha",
                  "Significance Level (Alpha)",
                  min = 0.001, max = 0.5, value = 0.05, step = 0.001),
      checkboxInput("show_null", "Show Null Distribution", TRUE),
      checkboxInput("show_alt", "Show Alternative Distribution", TRUE),
      actionButton("sample_null", "Generate from Null"),
      verbatimTextOutput("sample_stat_null"), # Display null sample result
      actionButton("sample_alt", "Generate from Alternative"),
      verbatimTextOutput("sample_stat_alt"), # Display alternative sample result
      actionButton("erase_results", "Erase Results")
    ),
    mainPanel(
      textOutput("critical_value_text"),
      plotlyOutput("distPlot")
    )
  )
                 )
                 ),
  tabPanel("Test Power",
           sidebarLayout(
             sidebarPanel(
               sliderInput("mean_alt_power", "Mean of Alternative Distribution:", min = 0, max = 10, value = 3),
               sliderInput("alpha_power", "Significance Level (Alpha):", min = 0.001, max = 0.5, value = 0.05, step = 0.001),
               sliderInput("n_power", "Sample Size (n):", min = 5, max = 100, value = 10),
               sliderInput("sigma_power", "Standard Deviation (σ):", min = 0.1, max = 10, value = 3),
               checkboxInput("show_null_power", "Show Null Distribution", TRUE),
               checkboxInput("show_alt_power", "Show Alternative Distribution", TRUE)
             ),
             mainPanel(
               plotlyOutput("powerPlot"),
               htmlOutput("formulaOutput") # Changed to htmlOutput for potential HTML representation
             )
           )
  )
)


# Define server logic
server <- function(input, output, session) {
  mu0 <- 0
  sigma <- 1
  sample_size <- 30
  sample_stat <- reactiveValues(null_stat = NULL, alt_stat = NULL)
  
  observeEvent(input$sample_null, {
    sample <- rnorm(1, mean = mu0, sd = sigma)
    sample_stat$null_stat <- mean(sample)
  })
  
  observeEvent(input$sample_alt, {
    mu1 <- input$mean_alt
    sample <- rnorm(1, mean = mu1, sd = sigma)
    sample_stat$alt_stat <- mean(sample)
  })
  
  observeEvent(input$erase_results, {
    sample_stat$null_stat <- NULL
    sample_stat$alt_stat <- NULL
  })
  
  output$sample_stat_null <- renderText({
    if (!is.null(sample_stat$null_stat)) {
      paste("Sample statistic from null: ", round(sample_stat$null_stat, 3))
    }
  })
  
  output$sample_stat_alt <- renderText({
    if (!is.null(sample_stat$alt_stat)) {
      paste("Sample statistic from alternative: ", round(sample_stat$alt_stat, 3))
    }
  })
  
  output$critical_value_text <- renderText({
    alpha <- input$alpha
    z_alpha <- qnorm(1 - alpha)
    critical_value <- mu0 + z_alpha * sigma
    paste("I reject if the test statistic is larger than", round(critical_value, 2))
  })



  output$distPlot <- renderPlotly({
    # Parameters
    mu0 <- 0
    mu1 <- input$mean_alt
    sigma <- 1
    alpha <- input$alpha
    z_alpha <- qnorm(1 - alpha)
    critical_value <- z_alpha 
    x_range <- seq(-4, 10, length.out = 1000)
    

    # Generate data for both distributions across the entire range
    y_null_full <- dnorm(x_range, mean = mu0, sd = sigma)
    y_alt_full <- dnorm(x_range, mean = mu1, sd = sigma)
    
    # Create a more detailed segment for Type 1 and Type 2 error areas
    x_type1 <- seq(critical_value, max(x_range), length.out = 500)
    y_type1 <- dnorm(x_type1, mean = mu0, sd = sigma)
    
    x_type2 <- seq(min(x_range), critical_value, length.out = 500)
    y_type2 <- dnorm(x_type2, mean = mu1, sd = sigma)
    
    plot <- plot_ly() %>%
      layout(title = "Distribution Under Null vs. Alternative Hypothesis in a One-Sided Test",
             xaxis = list(title = "Value", range = c(-4, 10)),
             yaxis = list(title = "Density", range = c(0, max(c(y_null_full, y_alt_full))*1.2)),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3), # Adjust legend position
             margin = list(b = 100)) # Reserve space at the bottom for the legend
    
    # Null distribution
    if(input$show_null) {
      plot <- plot %>%
        add_trace(x = x_range, y = y_null_full, type = 'scatter', mode = 'lines', name = "Null Distribution", line = list(color = 'blue')) %>%
        add_trace(x = x_type1, y = y_type1, type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = 'rgba(0, 0, 255, 0.2)', name = "Type 1 Error Area")
    }
    
    # Alternative distribution
    if(input$show_alt) {
      plot <- plot %>%
        add_trace(x = x_range, y = y_alt_full, type = 'scatter', mode = 'lines', name = "Alternative Distribution", line = list(color = 'red')) %>%
        add_trace(x = x_type2, y = y_type2, type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = 'rgba(255, 0, 0, 0.2)', name = "Type 2 Error Area")
    }
    
    # Critical value
    plot <- plot %>%
      add_trace(x = c(critical_value, critical_value), y = c(0, max(c(y_null_full, y_alt_full))*1.2), type = 'scatter', mode = 'lines', name = "Critical Value", line = list(color = 'black', dash = 'dash'))
    
    
    # Adding horizontal lines for sample statistics
    if (!is.null(sample_stat$null_stat)) {
      plot <- plot %>% add_lines(x = c(sample_stat$null_stat, sample_stat$null_stat), y = c(0, max(c(y_null_full, y_alt_full))*1.2), name = "Sample from Null", line = list(color = 'purple', dash = 'dot'))
    }
    if (!is.null(sample_stat$alt_stat)) {
      plot <- plot %>% add_lines(x = c(sample_stat$alt_stat, sample_stat$alt_stat), y = c(0, max(c(y_null_full, y_alt_full))*1.2), name = "Sample from Alternative", line = list(color = 'orange', dash = 'dot'))
    }
    
    
    plot
  })
  
  output$powerPlot <- renderPlotly({
    mu0 <- 0  # Mean of the null distribution
    sigma <- input$sigma_power  # Standard deviation of the distributions
    mu1 <- input$mean_alt_power  # Mean of the alternative distribution, controlled by the user
    alpha <- input$alpha_power  # Significance level, controlled by the user
    z_alpha <- qnorm(1 - alpha)  # Z-value corresponding to the alpha level
    n=input$n_power
    critical_value <- mu0 + z_alpha * sigma/sqrt(n)  # Critical value for the one-sided test

    x_range <- seq(-4, 10, length.out = 1000)
    y_null <- dnorm(x_range, mean = mu0, sd = sigma/sqrt(n))
    y_alt <- dnorm(x_range, mean = mu1, sd = sigma/sqrt(n))
    
    plot <- plot_ly() %>%
      layout(title = "Power of the Test",
             xaxis = list(title = "Value"),
             yaxis = list(title = "Density"),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3),
             margin = list(b = 100))
    
    if (input$show_null_power) {
      plot <- plot %>% add_lines(x = x_range, y = y_null, name = "Null Distribution", line = list(color = 'blue'))
    }
    
    if (input$show_alt_power) {
      plot <- plot %>% add_lines(x = x_range, y = y_alt, name = "Alternative Distribution", line = list(color = 'red'))
    }
    
    plot <- plot %>% add_lines(x = c(critical_value, critical_value), y = c(0, max(y_null, y_alt)), name = "Critical Value", line = list(color = 'black', dash = 'dash'))
    
    # Highlight the power area
    if (input$show_alt_power) {
      power_area_x <- x_range[x_range > critical_value]
      power_area_y <- dnorm(power_area_x, mean = mu1, sd = sigma/sqrt(n))
      plot <- plot %>% add_ribbons(x = power_area_x, y = power_area_y, ymin = 0, ymax = power_area_y, name = "Power", fillcolor = 'rgba(255,0,0,0.5)', line = list(color = 'rgba(255,0,0,0)'))
    }
    
    plot
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
