# Builds Data/deliveries.Rda for the W06 live deck.
# Invented data: a sample of 36 logged deliveries for a last-mile courier in
# Mexico City, whose contract promises a mean delivery time of 100 minutes.

set.seed(2026)

n <- 36
x <- rgamma(n, shape = 26, rate = 26 / 108.4)          # right-skewed, as delivery times are
x <- round((x - mean(x)) * (21 / sd(x)) + 108.4, 1)    # tune centre and spread

deliveries <- data.frame(
  delivery_id = 1:n,
  minutes     = x
)

save(deliveries, file = "deliveries.Rda")

# what the slides quote
cat("n:", nrow(deliveries), " mean:", round(mean(deliveries$minutes), 2),
    " sd:", round(sd(deliveries$minutes), 2), "\n")
