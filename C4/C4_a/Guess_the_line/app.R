library(shiny)
library(plotly)

# Define the UI
ui <- fluidPage(
  titlePanel("Guess the Line"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("slope_guess", "Guess Slope (Beta1):", value = 0, min = -5, max = 5, step = 0.1),
      numericInput("intercept_guess", "Guess Intercept (Beta0):", value = 0, min = -10, max = 10, step = 0.1)
    ),
    mainPanel(
      plotlyOutput("regressionPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Fixed data generation
  set.seed(123) # For reproducibility
  x <- seq(-10, 10, by = 1)
  e <- rnorm(length(x), mean = 0, sd = 5)
  y_actual <- 4 + 1.65 * x + e
  
  data <- data.frame(x, y_actual)
  
  output$regressionPlot <- renderPlotly({
    # Create the plot for the actual data
    p <- plot_ly(data, x = ~x, y = ~y_actual, type = "scatter", mode = "markers", marker = list(size = 8, opacity = 0.8)) %>%
      layout(
        title = "Data Points and Guessed Line",
        xaxis = list(title = "X", range = c(-25, 25)),
        yaxis = list(title = "Y", range = c(-25, 25))
      )
    
    # Guessing the line
    slope_guess <- input$slope_guess
    intercept_guess <- input$intercept_guess
    
    # Calculate the line based on guessed slope and intercept
    y_guessed <- intercept_guess + slope_guess * x
    
    # Add the guessed line to the plot
    p <- p %>% add_trace(x = x, y = y_guessed, type = "scatter", mode = "lines", line = list(color = "blue", opacity = 0.7, showlegend = FALSE))
    
    # Calculate and add rectangles representing squared deviations
    squared_deviations <- (y_actual - y_guessed)^2
    max_squared_deviation <- max(squared_deviations)
    
    deviations <- lapply(1:length(x), function(i) {
      deviation <- list(
        type = "line",
        x0 = x[i],
        x1 = x[i],
        y0 = y_actual[i],
        y1 = y_guessed[i],
        color = paste("#FF0000", sprintf("%02X", 128), sep = ""),  # Adjust alpha (transparency) here (e.g., 128 for 50% transparency)
        line = list(width = 1)
      )
      deviation
    })
    
    p <- p %>% layout(shapes = deviations)
    
    # Calculate SSE
    sse <- sum(squared_deviations)
    
    # Add SSE below the plot
    p <- p %>% add_text(
      text = paste("SSE:", round(sse, 2)),
      x = -10,
      y = -20,  # Adjust the y-coordinate to position the text below the plot
      showarrow = FALSE,
      font = list(color = "black", size = 14),  # You can customize the font and size
      showlegend = FALSE,  # Set showlegend to FALSE to hide the legend entry
      marker = list(opacity = 0)  # Set marker opacity to 0 to make the dot completely transparent
      # Set showlegend to FALSE to hide the legend entry
    )
    
    
    p
  })

  
  output$deviationsText <- renderPrint({
    # Guessing the line
    slope_guess <- input$slope_guess
    intercept_guess <- input$intercept_guess
    
    # Calculate the line based on guessed slope and intercept
    y_guessed <- intercept_guess + slope_guess * x
    
    # Calculate squared deviations
    squared_deviations <- (y_actual - y_guessed)^2
    
    cat("Squared Deviations for Each Point:\n")
    cat(paste(squared_deviations, collapse = ", "))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

df <- 8

# Define the t-value
t_value <- 1.625

# Calculate the probability
probability <- 1 - pt(t_value, df)

