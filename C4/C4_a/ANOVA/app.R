library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("ANOVA Shiny App"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_points", "Number of Data Points", value = 50, min = 10, max = 100),
      numericInput("beta0", "Intercept (beta0)", value = 0, min = -10, max = 10),
      numericInput("beta1", "Slope (beta1)", value = 1, min = -10, max = 10),
      numericInput("error_sd", "Standard Deviation of Errors", value = 5, min = 0.1, max = 10),
      actionButton("generate_data", "Generate Data"),
      
      # Description of how the app works
      htmlOutput("app_description")
    ),
    mainPanel(
      fluidRow(
        column(width = 4, plotOutput("mean_and_deviations")),
        column(width = 4, plotOutput("fitted_regression")),
        column(width = 4, plotOutput("unexplained_deviations"))
      ),
      verbatimTextOutput("anova_table"),
      verbatimTextOutput("f_test_result"),
      plotOutput("f_density")
    )
  )
)

# Define server
server <- function(input, output) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$generate_data, {
    x <- runif(n = input$n_points,-10, 10)
    true_beta0 <- input$beta0 # Use input value
    true_beta1 <- input$beta1 # Use input value
    error_sd <- input$error_sd
    y <- true_beta0 + true_beta1 * x + rnorm(input$n_points, sd = error_sd)
    data(data.frame(x, y))
  })
  
  # Description content
  output$app_description <- renderUI({
    HTML("<p>This Shiny app allows you to explore ANOVA (Analysis of Variance) by generating and visualizing data for a simple linear regression model.</p>
          <p>Adjust the number of data points, intercept (beta0), slope (beta1), and standard deviation of errors to see how these parameters affect the regression analysis.</p>
          <p>Click the 'Generate Data' button to create a dataset based on your inputs, and observe the following visualizations:</p>
          <ul>
            <li>Total Deviations: Shows the total deviations of the data points from the mean.</li>
            <li>Explained Deviations: Illustrates the deviations explained by the linear regression model.</li>
            <li>Unexplained Deviations: Depicts the unexplained deviations (residuals) after applying the regression model.</li>
            <li>ANOVA Table: Displays the analysis of variance table.</li>
            <li>F-Test Result: Shows the F-statistic and critical value for the F-test.</li>
            <li>F-Distribution Density: Visualizes the F-distribution density with the F-statistic marked for reference.</li>
          </ul>")
  })
  
  output$mean_and_deviations <- renderPlot({
    data_df <- data()
    mean_y <- mean(data_df$y, na.rm = TRUE)
    deviations <- data_df$y - mean_y
    SST <- sum((data_df$y - mean_y)^2, na.rm = TRUE)
    
    ggplot(data_df, aes(x, y)) +
      geom_point() +
      geom_hline(yintercept = mean_y, linetype = "dashed", color = "red") +
      geom_segment(aes(xend = x, yend = mean_y), linetype = "dashed", alpha = 0.2) +
      labs(title = "Total Deviations", x = "X", y = "Y") +
      annotate("text", x = -2, y = max(data_df$y) - 2, label = paste("SST =", round(SST, 2)), color = "red", size=6) +
      theme_minimal()
  })
  
  output$fitted_regression <- renderPlot({
    data_df <- data()
    lm_model <- lm(y ~ x, data = data_df)
    mean_y <- mean(data_df$y, na.rm = TRUE)
    fitted_values <- predict(lm_model, newdata = data_df)
    deviations_from_regression <- data_df$y - fitted_values
    SS_model <- sum((fitted_values - mean(data_df$y))^2, na.rm = TRUE)
    
    ggplot(data_df, aes(x, y)) +
      geom_point() +
      geom_hline(yintercept = mean_y, linetype = "dashed", color = "red") +
      geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], linetype = "solid", color = "blue") +
      geom_segment(aes(x=x, xend = x, y=mean_y, yend = fitted_values), linetype = "dashed", alpha = 0.2) +
      labs(title = "Explained Deviations", x = "X", y = "Y") +
      annotate("text", x = -2, y = max(data_df$y) - 2, label = paste("SSM =", round(SS_model, 2)), color = "red", size=6) +
      theme_minimal()
  })
  
  output$unexplained_deviations <- renderPlot({
    data_df <- data()
    mean_y <- mean(data_df$y, na.rm = TRUE)
    lm_model <- lm(y ~ x, data = data_df)
    fitted_values <- predict(lm_model, newdata = data_df)
    unexplained_deviations <- data_df$y - fitted_values
    SS_residuals <- sum(unexplained_deviations^2, na.rm = TRUE)
    
    ggplot(data_df, aes(x, y)) +
      geom_point() +
      geom_hline(yintercept = mean_y, linetype = "dashed", color = "red") +
      geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], linetype = "solid", color = "blue") +
      geom_segment(aes(x=x, xend = x, y=fitted_values, yend = y), linetype = "dashed", alpha = 0.2) +
      labs(title = "Unexplained Deviations", x = "X", y = "Y") +
      annotate("text", x = -2, y = max(data_df$y) - 2, label = paste("SSR =", round(SS_residuals, 2)), color = "red", size=6) +
      theme_minimal()
  })
  
  output$anova_table <- renderPrint({
    data_df <- data()
    lm_model <- lm(y ~ x, data = data_df)
    
    # Perform ANOVA using R's anova function
    anova_result <- anova(lm_model)
    
    # Print the ANOVA result
    print(anova_result)
  })
  
  
  
  output$f_test_result <- renderText({
    data_df <- data()
    lm_model <- lm(y ~ x, data = data_df)
    SS_model <- sum((predict(lm_model, newdata = data_df) - mean(data_df$y))^2, na.rm = TRUE)
    SS_residuals <- sum(resid(lm_model)^2, na.rm = TRUE)
    df1 <- 1
    df2 <- length(data_df$x) - 2
    F_statistic <- SS_model / (SS_residuals/(input$n_points-2))
    paste("F-statistic =SSR/SSE=", round(F_statistic, 2))
    
    f_formula  <- paste("F-statistic =", round(F_statistic, 2), "\n", "DF_Model = 1", "\n", "DF_Residuals =", df2)
    
    f_formula
    
  })
  
  output$f_density <- renderPlot({
    data_df <- data()
    lm_model <- lm(y ~ x, data = data_df)
    SS_model <- sum((predict(lm_model, newdata = data_df) - mean(data_df$y))^2, na.rm = TRUE)
    SS_residuals <- sum(resid(lm_model)^2, na.rm = TRUE)
    
    df1 <- 1
    df2 <- length(data_df$x) - 2
    f_statistic <-  SS_model / (SS_residuals/(input$n_points-2))
    f_values <- df(seq(0, 5, by = 0.01), df1, df2)
    critical_value <- qf(0.95, df1, df2)
    
    ggplot(data.frame(x=seq(0, 5, by = 0.01), y = f_values), aes(x,y)) +
      geom_area(aes(y = y, x = x), alpha=0.5 ,fill="lightblue",
                stat="identity")+
      geom_vline(xintercept = f_statistic, linetype = "dashed", color = "red") +
      labs(title = "F-Distribution Density", x = "F", y = "Density") +
      annotate("text", x = f_statistic, y = 0.5, label = paste("F-statistic =", round(f_statistic, 2)), color = "red", size=6) +
      annotate("text", x = 4.5, y = 2, label = paste("5% critical value"), color = "blue", size=6) +
      geom_vline(xintercept = critical_value, linetype = "dotted", color = "blue") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)
