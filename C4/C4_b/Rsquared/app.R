library(shiny)
library(ggplot2)
library(ggrepel)

ui <- fluidPage(
  titlePanel("R-Squared Interactive Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("slope", "Slope:", min = 0, max = 2, value = 0.5, step = 0.05),
      sliderInput("variance", "Variance:", min = 0, max = 20, value = 8, step = 1),
      sliderInput("n_obs", "Number of Observations:", min = 10, max = 200, value = 50, step = 10)
    ),
    mainPanel(
      plotOutput("regressionPlot")
    )
  )
)

server <- function(input, output) {
  
  output$regressionPlot <- renderPlot({
    set.seed(17)
    x <- seq(0, 100, length.out = input$n_obs)
    y <- 50 + x * input$slope + input$variance * rnorm(length(x))
    
    data <- data.frame(x, y)
    
    lm_fit <- lm(y ~ x, data = data)
    rsquared <- summary(lm_fit)$r.squared
    
    ggplot(data, aes(x, y)) +
      geom_point(pch = 19, cex = 0.8) +
      geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
      geom_text_repel(aes(label = paste("R² =", round(rsquared, 2))), x = 30, y = 90, size = 4) +
      labs(title = "Linear Regression Plot",
           x = "X",
           y = "Y") +
      theme_minimal() +
      xlim(c(0, 100)) +  # Fix the x-axis limits
      ylim(c(0, 200))   # Fix the y-axis limits
  })
}

shinyApp(ui = ui, server = server)