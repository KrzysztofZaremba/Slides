# Builds Data/branches.Rda for the W07 live deck.
# Invented data: 120 branches of a Mexican retail chain, one row per branch.
#   ad_spend     - local advertising spend last month, thousands of pesos
#   sales        - sales last month, thousands of pesos
#   staff        - number of employees

set.seed(1207)

n <- 120
ad_spend <- round(rgamma(n, shape = 9, rate = 9 / 42), 1)          # ~42k, right-skewed
noise    <- rnorm(n, 0, 210)
sales    <- round(520 + 4.1 * ad_spend + noise, 1)
staff    <- pmax(3, round(6 + 0.09 * ad_spend + rnorm(n, 0, 2)))

branches <- data.frame(
  branch_id = 1:n,
  ad_spend  = ad_spend,
  sales     = sales,
  staff     = staff
)

save(branches, file = "branches.Rda")

r <- cor(branches$ad_spend, branches$sales)
t <- r * sqrt((n - 2) / (1 - r^2))
cat("n", n, "\n")
cat("ad_spend mean", round(mean(ad_spend), 1), " range", round(range(ad_spend), 1), "\n")
cat("sales    mean", round(mean(sales), 1), " range", round(range(sales), 1), "\n")
cat("r", round(r, 4), " r^2", round(r^2, 3), " t", round(t, 3), "\n")
cat("qt(0.95, n-2)", round(qt(0.95, n - 2), 3),
    " one-sided p", signif(1 - pt(t, n - 2), 3), "\n")
