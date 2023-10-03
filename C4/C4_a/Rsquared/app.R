library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("SST and SSE Interactive Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("slope", "Slope:", min = 0, max = 2, value = 0.125, step = 0.025),
      sliderInput("variance", "Variance:", min = 0, max = 20, value = 8, step = 1),
      helpText("This app demonstrates the concepts of SST (Total Sum of Squares) and SSE (Error Sum of Squares) in the context of linear regression."),
      helpText("Use the sliders to adjust the slope and variance of the data points. Observe how SST and SSE change as you modify the parameters.")
    ),
    mainPanel(
      plotOutput("regressionPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  output$regressionPlot <- renderPlot({
    set.seed(17)
    x <- (1:24) * 2
    y <- 24 + x * input$slope + input$variance * rnorm(length(x))
    
    data <- data.frame(x, y)
    
    # Create a scatterplot with a linear regression line
    plot <- ggplot(data, aes(x, y)) +
      geom_point(pch = 19, cex = 0.8) +
      geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
      labs(title = "Linear Regression Plot",
           x = "X",
           y = "Y") +
      theme_minimal()
    
    # Calculate the least squares line (regression line)
    fit <- lm(y ~ x)
    
    # Calculate the Total Sum of Squares (SST)
    y_mean <- mean(data$y)
    sst <- sum((data$y - y_mean)^2)
    
    # Calculate the Error Sum of Squares (SSE)
    sse <- sum(fit$residuals^2)
    
    # Add rectangles to visualize SSE
    rectangles <- mapply(function(x, y, r) rect(x, y, x - r, y - r, col = hsv(1, alpha = 0.1), border = NA),
                         x, y, fit$residuals)
    
    # Display SST and SSE
    plot <- plot + annotate("text", x = 6, y = max(data$y), label = paste("SST:", round(sst, 2))) +
      annotate("text", x = 6, y = max(data$y) - 10, label = paste("SSE:", round(sse, 2)))
    
    # Show rectangles representing errors
    plot <- plot + rectangles
    
    plot
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)