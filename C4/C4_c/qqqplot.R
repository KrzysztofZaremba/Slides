



###


histogram of residuals from normal

histogram of residuals from uniform 

histogram of residuals from student t with 2





###plot 1,2,3

different quantiles of normal distribution 


### plot 3

Simulate data from normal distribution,

Standardize it?
  tive normal distribution will plot as a straight line. Let t[1] < t[2] < . . . < t[n] be the
externally studentized residuals ranked in increasing order. If we plot t[i] against the
cumulative probability Pi = − ( ) i n 1
2 / , i = 1, 2, . . . , n , on t

table with both - they should be more or less the same, (start with like 3 observations)

table with both - all observations together - normal, quantile, sample

which graphically means they should be on the same line

determine the quantiles and put them ona  graph 

###let's see what r does on qqplot and let follow it

### plot 4

- show residuals from uniform distribution

- show histogram of these residuals

- build a table1


library(ggplot2)
library(gridExtra)

# Set the seed for reproducibility
set.seed(123)

# Generate uniformly distributed residuals
residuals <- runif(100, min = -1, max = 1)

# Create a data frame with the residuals
data_df <- data.frame(Residuals = residuals)

# Create x values (original variable)
data_df$x <- rnorm(100)

# Create y values using a linear equation
data_df$y <- 2 + 3 * data_df$x + residuals

# Fit a linear regression model
model <- lm(y ~ x, data_df)

# Create a fitted regression plot
regression_plot <- ggplot(data_df, aes(x = x, y = y)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "X", y = "Y") +
  theme_minimal()

# Create a histogram of standardized residuals
histogram_plot <- ggplot(data_df, aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, aes(y = after_stat(density)), fill = "green", color = "black", alpha = 0.7) +
  geom_density(color = "blue") +
  labs(x = "Standardized Residuals", y = "Density") +
  theme_minimal()

# Create a QQ plot
qq_plot <- ggplot(data_df) +
  geom_qq(aes(sample = residuals)) +
  geom_abline(color = "red") +
  coord_fixed() +
  theme_minimal()+
  xlim(-3,3)+
  ylim(-3,3)

# Arrange the three plots side by side
grid.arrange(regression_plot, histogram_plot, qq_plot, ncol = 3)



####

exercice some other distributions, maybe right skeewd