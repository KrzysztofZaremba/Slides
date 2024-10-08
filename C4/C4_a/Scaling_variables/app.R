#scaling variales

#Load required libraries
library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  titlePanel("Linear Regression Parameter Exploration"),
  
  sidebarLayout(
    
    sidebarPanel(
      HTML("<h3>App Description</h3>"),
      HTML("<p>This app allows you to explore the changes in slope and intercept of a linear regression when we rescale/add a constant to a variable. Pick your transformation using windows, click on UPDATE PLOT below and observe how the data and parameter change. The gray points represent the original data and the black points represent transformed data. Dashed line is the original regression line and the solid black one represents regression fitted to the transformed data.</p>"),
      uiOutput("formula1"),
      HTML("<p>Transformed Regression (y' on z)</p>"),
      uiOutput("formula2"),
      HTML("<p>Where:</p>"),
      uiOutput("formula3"),
      uiOutput("formula4"),
      numericInput("x_scale", "scale a in z=ax+c", value = 1, step = 0.1),
      numericInput("constant_x", "constant c in z=ax+c:", value = 0, step = 1),
      numericInput("y_scale", "scale b in y'= by+d", value = 1, step = 0.1),
      numericInput("constant_y", "constant d in y'= by+d", value = 0, step = 1),
      actionButton("update", "Update Plot"),
      uiOutput("formula5"),
      uiOutput("formula6")
    ),
    
    mainPanel(
      plotOutput("regression_plot")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  output$formula1 <- renderUI({
    withMathJax(paste0("$$y=\\beta^x_0+\\beta^x_1x_i+\\epsilon_i$$"))
  })
  
  output$formula2 <- renderUI({
    withMathJax(paste0("$$y'=\\beta^z_0+\\beta^z_1z_i+u_i$$"))
  })
  
  output$formula3 <- renderUI({
    withMathJax(paste0("$$y'=by+d$$"))
  })
  
  output$formula4 <- renderUI({
    withMathJax(paste0("$$z=ax+c$$"))
  })
  
  output$formula6 <- renderUI({
    withMathJax(paste0("$$\\beta^z_0=b\\beta^0_x+d-\\frac{b}{a}\\beta^x_1c=",input$y_scale,"\\beta^0_x+",input$constant_y ,"-\\frac{",input$y_scale,"}{",input$x_scale ,"}\\beta^x_1(",input$constant_x ,")$$"))
  })
  
  output$formula5 <- renderUI({
    withMathJax(paste0("$$\\beta^z_1=\\frac{b}{a}\\beta^x_1=\\frac{",input$y_scale,"}{",input$x_scale ,"}\\beta^x_1$$"))
  })
  
  
  
  observeEvent(input$update, {
    # Generate random data
    set.seed(123)
    x <- rnorm(100)
    y <- 2 * x + rnorm(100)
    
    # Apply user-specified transformations
    x_transformed <- (x + input$constant_x) * input$x_scale
    y_transformed <- (y + input$constant_y) * input$y_scale
    
    # Fit regression models
    model_original <- lm(y ~ x)
    model_transformed <- lm(y_transformed ~ x_transformed)
    
    # Extract coefficients for original and transformed models
    beta0_original <- coef(model_original)[1]
    beta1_original <- coef(model_original)[2]
    
    beta0_transformed <- coef(model_transformed)[1]
    beta1_transformed <- coef(model_transformed)[2]
    
    # Create a scatterplot with both regression lines
    plot_data <- data.frame(x, y, x_transformed, y_transformed)
    p <- ggplot(plot_data) +
      geom_point(aes(x, y), color = "gray") +
      geom_point(aes(x_transformed, y_transformed), color = "black") +
      geom_abline(intercept = beta0_original, slope = beta1_original, color = "red", linetype = "dashed") +
      geom_abline(intercept = beta0_transformed, slope = beta1_transformed, color = "blue") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      labs(
        title = "Linear Regression Comparison",
        x = "X",
        y = "Y"
      )+
      xlim(-10,10)+
      ylim(-10,10)+
      theme_minimal()
    
    # Show the plot
    output$regression_plot <- renderPlot({
      p
    })
    
    # Calculate the relationship between beta_x and beta_z
    beta_x <- beta1_original
    beta_z <- beta1_transformed / input$x_scale
    
    message <- paste("Original Regression (Y on X): Y = ", round(beta0_original, 2), " + ", round(beta1_original, 2), "X\n")
    message <- paste(message, "Transformed Regression (Y' on Z): Y' = ", round(beta0_transformed, 2), " + ", round(beta1_transformed, 2), "Z\n")
    message <- paste(message, "Beta_x (Original) =", round(beta_x, 2), "\n")
    message <- paste(message, "Beta_z (Transformed) =", round(beta_z, 2))
    
    cat(message)
  })
  
  
}

# Run the Shiny app
shinyApp(ui, server)