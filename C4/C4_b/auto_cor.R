# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 100

# Generate data with correlated residuals
correlation_values <- c(0.8, 0.6, 0.4, 0.2, 0)  # Decreasing correlation values
scatterplot_list <- list()

# Initialize a list to store autocorrelation coefficients
autocorrelation_coefficients <- list()
ggplot_list <-vector("list", length = 5)
lag=2
# Create scatterplots for residual vs. lagged residuals
for (lag in 1:5) {  # Lag from 1 to 5
  # Create correlated residuals with decreasing correlation
  correlation <- correlation_values[lag]
  cov_matrix <- matrix(c(1, correlation, correlation, 1), ncol = 2)
  correlated_residuals <- mvrnorm(n, c(0, 0), cov_matrix)
  
  # Create a data frame for correlated residuals
  data1 <- data.frame(Residual_i = correlated_residuals[, 1])
  data1[[paste0("Residual_i-", lag)]] <- correlated_residuals[, 2]
  
  # Create a scatter plot for residual vs. lagged residuals
  message(lag)
  
  ggplot_list[[lag]]<-local({
  lag<-lag
   gg <- ggplot(data=data1, aes(x = Residual_i, y = data1[[paste0("Residual_i-", lag)]])) +
    geom_point(color = "blue") +
    labs(x = "Residual", y = paste0("Residual_i-", lag)) +
    theme_minimal() +
    ggtitle(paste("Lag:", lag))
   print(gg)
  })

  # Calculate and store autocorrelation coefficient
  autocorr_coeff <- cor(data1$Residual_i, data1[[paste0("Residual_i-", lag)]])
  autocorrelation_coefficients[[lag]] <- autocorr_coeff
}


ggplot_list[[4]]
grid.arrange(grobs = ggplot_list, ncol = 5)
