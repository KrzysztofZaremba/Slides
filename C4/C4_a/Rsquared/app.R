library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("SST and SSE Interactive Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("slope", "Slope:", min = 0, max = 2, value = 0.125, step = 0.025),
      sliderInput("variance", "Variance:", min = 0, max = 20, value = 8, step = 1)
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
    
    ggplot(data, aes(x, y)) +
      geom_point(pch = 19, cex = 0.8) +
      geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
      labs(title = "Linear Regression Plot",
           x = "X",
           y = "Y") +
      theme_minimal()
    
    fit <- lm(y ~ x)  # Find the least squares line
    u <- mapply(function(x, y, r) rect(x, y, x - r, y - r, col = hsv(1, alpha = 0.1), border = NA),
                x, y, fit$residuals)  # Plot the squares
    u
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)