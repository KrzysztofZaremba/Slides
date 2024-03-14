library(shiny)
library(plotly)

# Define the UI
ui <- fluidPage(
  titlePanel("SST and SSE Interactive Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("slope", "Slope:", min = 0, max = 1, value = 0.125, step = 0.025),
      sliderInput("variance", "Variance:", min = 0, max = 5, value = 3, step = 0.5),
      
      # Description of how the app works
      htmlOutput("app_description"),
      textOutput("rSquaredDetailText")
    ),
    mainPanel(
      plotlyOutput("regressionPlot"),
      plotlyOutput("sstPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  data <- reactive({
    x <- runif(20, -10, 10)  # Replace with your desired data generation logic
    y <- 3 + x * input$slope + input$variance * rnorm(length(x))
    data.frame(x, y)
  })
  
  # Description content
  output$app_description <- renderUI({
    HTML("<p>This Shiny app allows you to interactively visualize the concepts of SST (Total Sum of Squares) and SSE (Error Sum of Squares) in a simple linear regression context.</p>
          <p>Adjust the slope and variance sliders to see how they affect the regression plot and SST/SSE calculations.</p>
          <p>The app provides two interactive plots:</p>
          <ul>
            <li>Regression Plot: Shows the scatter plot of data points with the fitted regression line. The red dashed lines represent the deviations between data points and the regression line, and the blue line represents the regression line.</li>
            <li>SST Plot: Displays the intercept-only model plot (SST) with a green line. Similar to the regression plot, it also shows deviations and the SST value.</li>
          </ul>
          <p>Observe how changing the slope and variance values impacts the explained and unexplained variances in the data.</p>")
  })
  
  output$regressionPlot <- renderPlotly({
    
    
    fit <- lm(y ~ x, data=data())  # Find the least squares line
    data=data()  
    # Calculate axis limits
    max_range <- max(range(data$x, data$y))
    x_limits <- c(-10, 10)
    y_limits <- c(-15, 20)
    aspect_ratio <- diff(y_limits) / diff(x_limits)
    
    data=data()  
    # Create the plot for SSE
    p <- plot_ly(data(), x = ~x, y = ~y, type = "scatter", mode = "markers", marker = list(size = 8, opacity = 0.8)) %>%
      add_trace(x = data$x, y = predict(fit), type = "scatter", mode = "lines", line = list(color = "blue")) %>%
      layout(
        title = "Linear Regression Plot",
        xaxis = list(title = "X", range = x_limits, scaleanchor = "y", scaleratio = aspect_ratio),
        yaxis = list(title = "Y", range = y_limits)
      )
    
    # Add squares for unexplained variance
    squares <- lapply(1:length(fit$residuals), function(i) {
      x_val <- data$x[i]
      y_val <- data$y[i]
      square <- list(
        type = "rect",
        x0 = x_val - fit$residuals[i],
        x1 = x_val,
        y0 = y_val - fit$residuals[i],
        y1 = y_val,
        fillcolor = "rgba(255, 0, 0, 0.1)",
        line = list(width = 0)
      )
      square
    })
    
    # Add dashed vertical lines representing deviations
    deviations <- lapply(1:length(fit$residuals), function(i) {
      x_val <- data$x[i]
      y_val <- data$y[i]
      deviation <- list(
        type = "line",
        x0 = x_val,
        x1 = x_val,
        y0 = y_val,
        y1 = predict(fit)[i],
        line = list(color = "red", width = 1, dash = "dash")
      )
      deviation
    })
    
    # Calculate and add the sum of squares to the plot
    sse <- sum(fit$residuals^2)
    p <- p %>% layout(shapes = c(squares, deviations)) %>%
      add_text(
        text = paste("SSE:", round(sse, 2)),
        x = -8,
        y = 18,
        showarrow = FALSE,
        font = list(color = "white")
      )
    
    p
    
    

  })
  
  output$sstPlot <- renderPlotly({
    fit <- lm(y ~ 1, data=data())  # Model with only the intercept
    
    data=data()  
    # Calculate axis limits
    max_range <- max(range(data$x, data$y))
    x_limits <- c(-10, 10)
    y_limits <- c(-15, 20)
    aspect_ratio <- diff(y_limits) / diff(x_limits)
    
    # Create the plot for SST (intercept-only model)
    p <- plot_ly(data, x = ~x, y = ~y, type = "scatter", mode = "markers", marker = list(size = 8, opacity = 0.8)) %>%
      add_trace(x = data$x, y = predict(fit), type = "scatter", mode = "lines", line = list(color = "Green")) %>%
      layout(
        title = "Intercept-Only Model Plot (SST)",
        xaxis = list(title = "X", range = x_limits, scaleanchor = "y", scaleratio = aspect_ratio),
        yaxis = list(title = "Y", range = y_limits)
      )
    
    # Add squares for unexplained variance (same as regression plot)
    squares <- lapply(1:length(fit$residuals), function(i) {
      x_val <- data$x[i]
      y_val <- data$y[i]
      square <- list(
        type = "rect",
        x0 = x_val - fit$residuals[i],
        x1 = x_val,
        y0 = y_val - fit$residuals[i],
        y1 = y_val,
        fillcolor = "rgba(0, 200, 100, 0.1)",
        line = list(width = 0)
      )
      square
    })
    
    # Add dashed vertical lines representing deviations (same as regression plot)
    deviations <- lapply(1:length(fit$residuals), function(i) {
      x_val <- data$x[i]
      y_val <- data$y[i]
      deviation <- list(
        type = "line",
        x0 = x_val,
        x1 = x_val,
        y0 = y_val,
        y1 = predict(fit)[i],
        line = list(color = "red", width = 1, dash = "dash")
      )
      deviation
    })
    
    # Calculate and add the sum of squares to the plot
    sst <- sum((data$y - mean(data$y))^2)
    p <- p %>% layout(shapes = c(squares, deviations)) %>%
      add_text(
        text = paste("SST:", round(sst, 2)),
        x = -8,
        y = 18,
        showarrow = FALSE,
        font = list(color = "white")
      )
    
    p
  })
  
  rSquaredDetail <- reactive({
    data <- data()  # Assuming this retrieves your reactive dataset
    fit <- lm(y ~ x, data = data)
    sse <- sum(fit$residuals^2)
    sst <- sum((data$y - mean(data$y))^2)
    rsq <- 1 - sse / sst
    
    # Creating a detailed string that shows the calculation steps
    detail <- sprintf("R-squared (R^2) = 1 - SSE/SST = 1 - %.4f/%.4f = %.4f", sse, sst, rsq)
    detail  # This returns the detailed calculation
  })
  
  output$rSquaredDetailText <- renderText({
    rSquaredDetail()  # Use the detailed reactive expression
  })
}

?add_text()

# Run the Shiny app
shinyApp(ui = ui, server = server)
