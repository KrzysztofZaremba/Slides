library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Linear Regression with Error Terms"),
  tabsetPanel(
    tabPanel("Uncorrelated Errors",
             sidebarLayout(
               sidebarPanel(
                 actionButton("add_errors", "Add 100 Error Terms"),
                 actionButton("reset_errors", "Reset"),
                 textOutput("regression_formula")
               ),
               mainPanel(
                 plotOutput("regression_plot"),
                 plotOutput("error_histogram"),
                 textOutput("avg_errors")
               )
             )
    ),
    tabPanel("Uncorrelated Errors - non - 0 expectations",
             sidebarLayout(
               sidebarPanel(
                 actionButton("add_errors2", "Add 100 Error Terms "),
                 actionButton("reset_errors2", "Reset"),
                 textOutput("regression_formula2")
               ),
               mainPanel(
                 plotOutput("regression_plot2"),
                 plotOutput("error_histogram2"),
                 textOutput("avg_errors2")
               )
             )
    ),
    tabPanel("Correlated Errors",
             sidebarLayout(
               sidebarPanel(
                 actionButton("add_correlated_errors", "Add 100 Error Terms"),
                 actionButton("reset_correlated_errors", "Reset"),
                 textOutput("correlated_regression_formula")
               ),
               mainPanel(
                 plotOutput("correlated_regression_plot"),
                 plotOutput("correlated_error_histogram"),
                 textOutput("correlated_avg_errors")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # Initialize the data frame with the regression line
  data_df <- data.frame(x = seq(-120, 120), y = 0.5 * seq(-120, 120))
  
  # Initialize reactiveValues for uncorrelated errors
  rv_uncor <- reactiveValues(error_terms = data.frame(x = numeric(0), y = numeric(0), error = numeric(0)),
                             )
  
  rv_uncor2 <- reactiveValues(error_terms = data.frame(x = numeric(0), y = numeric(0), error = numeric(0)),
  )
  
  # Initialize reactiveValues for correlated errors
  rv_cor <- reactiveValues(error_terms = data.frame(x = numeric(0), y = numeric(0), error = numeric(0)),
                          )
  
  # Function to calculate regression formula
  calculate_regression_formula <- function(data_df, errors) {
    lm_fit <- lm(y ~ x, data = data.frame(x = data_df$x, y = data_df$y + errors))
    coef(lm_fit)
  }
  
  observeEvent(input$add_errors, {
    # Generate 100 uncorrelated error terms with a normal distribution (mean = 0, sd = 10)
    new_errors <- data.frame(x = rnorm(241, mean = 0, sd = 10), y = rnorm(241, mean = 0, sd = 10))

    # Calculate the error as y - 0.5x
    new_errors$error <- new_errors$y 

    # Add the error terms to the regression line to create new x-values
    new_x <- data_df$x + new_errors$x
    new_data <- data.frame(x = new_x, y = data_df$y + new_errors$error, error = new_errors$error)
   
    # Combine new_derrorta with existing error_terms
    rv_uncor$error_terms <- rbind(rv_uncor$error_terms, new_data)
    
    avg_error <- mean(rv_uncor$error_terms$error)
    # Calculate and accumulate the average of errors

    # Update the scatterplot
    output$regression_plot <- renderPlot({
      plot <- ggplot() +
        geom_line(data = data_df, aes(x = x, y = y), color = "blue") +
        geom_point(data = rv_uncor$error_terms, aes(x = x, y = y), color = "red", alpha = 0.1, shape = 19, size = 3, stroke=NA) +
        labs(title = "Uncorrelated Errors: Linear Regression") +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, hjust = 0.5)) +
        scale_x_continuous(limits = c(-120, 120)) +
        scale_y_continuous(limits = c(-100, 100))  # Adjusted y-axis limits
      
      print(plot)
    })
    
    # Update the histogram of errors
    output$error_histogram <- renderPlot({
      error_histogram <- ggplot(data = rv_uncor$error_terms, aes(x = error)) +
        geom_histogram(binwidth = 5, fill = "blue", color = "black") +
        labs(title = "Histogram of Uncorrelated Errors") +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, hjust = 0.5))
      
      print(error_histogram)
    })
    
    # Calculate and display the average of errors
    avg_error <- mean(rv_uncor$error_terms$error)
    output$avg_errors <- renderText({
      paste("Average Error:", round(avg_error, 3))
    })
    
    # Display regression formula
    output$regression_formula <- renderText({
      coef <- calculate_regression_formula(data_df, rv_uncor$error_terms$error)
      paste("Regression Formula: y =b0+b1*x + e")
    })
  })
  
  observeEvent(input$add_errors2, {
    # Generate 100 uncorrelated error terms with a normal distribution (mean = 0, sd = 10)
    new_errors <- data.frame(x = rnorm(241, mean = 0, sd = 10), y = rnorm(241, mean = -15, sd = 10))
    
    # Calculate the error as y - 0.5x
    new_errors$error <- new_errors$y 
    
    # Add the error terms to the regression line to create new x-values
    new_x <- data_df$x + new_errors$x
    new_data <- data.frame(x = new_x, y = data_df$y + new_errors$error, error = new_errors$error)
    
    # Combine new_derrorta with existing error_terms
    rv_uncor2$error_terms <- rbind(rv_uncor2$error_terms, new_data)
    
    avg_error <- mean(rv_uncor2$error_terms$error)
    # Calculate and accumulate the average of errors
    
    # Update the scatterplot
    output$regression_plot2 <- renderPlot({
      plot <- ggplot() +
        geom_line(data = data_df, aes(x = x, y = y), color = "blue") +
        geom_point(data = rv_uncor2$error_terms, aes(x = x, y = y), color = "red", alpha = 0.1, shape = 19, size = 3, stroke=NA) +
        labs(title = "Uncorrelated Errors: Linear Regression") +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, hjust = 0.5)) +
        scale_x_continuous(limits = c(-120, 120)) +
        scale_y_continuous(limits = c(-100, 100))  # Adjusted y-axis limits
      
      print(plot)
    })
    
    # Update the histogram of errors
    output$error_histogram2 <- renderPlot({
      error_histogram <- ggplot(data = rv_uncor2$error_terms, aes(x = error)) +
        geom_histogram(binwidth = 5, fill = "blue", color = "black") +
        labs(title = "Histogram of Uncorrelated Errors") +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, hjust = 0.5))
      
      print(error_histogram)
    })
    
    # Calculate and display the average of errors
    avg_error <- mean(rv_uncor2$error_terms$error)
    output$avg_errors2 <- renderText({
      paste("Average Error:", round(avg_error, 3))
    })
    
    # Display regression formula
    output$regression_formula2 <- renderText({
      coef <- calculate_regression_formula(data_df, rv_uncor2$error_terms$error)
      paste("Regression Formula: y =b0+b1*x + e")
    })
  })
  
  observeEvent(input$reset_errors2, {
    # Reset uncorrelated errors
    rv_uncor2$error_terms <- data.frame(x = numeric(0), y = numeric(0), error = numeric(0))
    rv_uncor2$avg_errors <- numeric(0)
  })
  
  observeEvent(input$add_correlated_errors, {
    # Generate 100 correlated error terms with a positive correlation with x
    x_values <- data_df$x
    new_errors <- data.frame(x = rnorm(241, mean = 0, sd = 10), y = rnorm(241, mean = 0.2 * x_values, sd = 10))
    
    # Calculate the error as y - 0.5x
    new_errors$error <- new_errors$y
    
    # Add the error terms to the regression line to create new x-values
    new_x <- data_df$x + new_errors$x
    new_data <- data.frame(x = new_x, y = data_df$y + new_errors$error, error = new_errors$error)
    
    # Combine new_data with existing error_terms
    rv_cor$error_terms <- rbind(rv_cor$error_terms, new_data)
    
    # Calculate and accumulate the average of errors
    avg_error <- mean(rv_cor$error_terms$error)
    
    # Update the scatterplot
    output$correlated_regression_plot <- renderPlot({
      plot <- ggplot() +
        geom_line(data = data_df, aes(x = x, y = y), color = "blue") +
        geom_point(data = rv_cor$error_terms, aes(x = x, y = y), color = "red", alpha = 0.1, shape = 19, size = 3, stroke=NA) +
        labs(title = "Correlated Errors: Linear Regression") +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, hjust = 0.5)) +
        scale_x_continuous(limits = c(-120, 120)) +
        scale_y_continuous(limits = c(-100, 100))  # Adjusted y-axis limits
      
      print(plot)
    })
    
    # Update the histogram of errors (show only error values)
    output$correlated_error_histogram <- renderPlot({
      error_histogram <- ggplot(data = rv_cor$error_terms, aes(x = error)) +
        geom_histogram(binwidth = 5, fill = "blue", color = "black") +
        labs(title = "Histogram of Correlated Errors") +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, hjust = 0.5))
      
      print(error_histogram)
    })
    
    # Calculate and display the average of errors
   
    output$correlated_avg_errors <- renderText({
      paste("Average Error:", round(avg_error, 3))
    })
    
    # Display regression formula
    output$correlated_regression_formula <- renderText({
      coef <- calculate_regression_formula(data_df, rv_cor$error_terms$error)
      paste("Regression Formula: y =b0+b1* x + e")
    })
  })
  
  observeEvent(input$reset_correlated_errors, {
    # Reset correlated errors
    rv_cor$error_terms <- data.frame(x = numeric(0), y = numeric(0), error = numeric(0))
    rv_cor$avg_errors <- numeric(0)
  })
}

shinyApp(ui, server)

