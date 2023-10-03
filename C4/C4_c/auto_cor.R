
# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 100

# Generate data with correlated residuals
correlation_values <- c(0.8, 0.6, 0.4, 0.2, 0)  # Decreasing correlation values
scatterplot_list <- list()

# Initialize a list to store autocorrelation coefficients
autocorrelation_coefficients <- list()
ggplot_list <- vector("list", length = 5)

# Create scatterplots for residual vs. lagged residuals
# Generate data with correlated residuals
correlation_values <- c(0.8, 0.6, 0.4, 0.2, 0)  # Decreasing correlation values
scatterplot_list <- list()

# Initialize a list to store autocorrelation coefficients
autocorrelation_coefficients <- list()

autocor = function (lag) {  # Lag from 1 to 5
  # Create correlated residuals with decreasing correlation
  correlation <- correlation_values[lag]
  cov_matrix <- matrix(c(1, correlation, correlation, 1), ncol = 2)
  correlated_residuals <- mvrnorm(n, c(0, 0), cov_matrix)
  
  # Create a data frame for correlated residuals
  data1 <- data.frame(Residual_i = correlated_residuals[, 1])
  data1[[paste0("Residual_i-", lag)]] <- correlated_residuals[, 2]
  
  autocorr_coeff <- cor(data1$Residual_i, data1[[paste0("Residual_i-", lag)]])
}

# Create scatterplots and collect autocorrelation coefficients
autocorrelation_coefficients=lapply(1:5, autocor)

# Create a bar graph of autocorrelation coefficients
bar_data <- data.frame(Lag = 1:5, Autocorrelation = unlist(autocorrelation_coefficients))
bar_plot <- ggplot(bar_data, aes(x = factor(Lag), y = Autocorrelation)) +
  geom_bar(stat = "identity") +
  labs(x = "Lag", y = "Autocorrelation") +
  theme_xaringan()

ggplot_list <- lapply(1:5, plot_data_column)
# Arrange the scatterplots and bar graph side by side
grid.arrange(
  arrangeGrob(grobs = ggplot_list , ncol = 5),
  bar_plot,
  ncol = 1,
  heights = c(3, 3)  # Adjust the height ratio as needed
)
