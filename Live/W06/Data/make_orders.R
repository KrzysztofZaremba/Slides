# Builds Data/orders.Rda for the W07 live deck.
# Invented data: a small audit of orders on a Mexico City food-delivery app.
#   channel - "app" or "web", how the order was placed
#   basket  - order value in pesos
#
# Deliberately SMALL samples: with n = 14 and m = 11 the normal approximation is
# not safe, so Student's t is the right reference — and the statistic lands
# between the normal cut-off (1.645) and the t cut-off, so the choice decides
# the verdict.

set.seed(4021)

n_app <- 14
n_web <- 11

repeat {
  a <- rnorm(n_app, 292, 45)
  w <- rnorm(n_web, 260, 45)
  SE <- sqrt(var(a) / n_app + var(w) / n_web)
  tt <- (mean(a) - mean(w)) / SE
  if (tt > 1.68 && tt < 1.79) break        # between qnorm(.95) and qt(.95, 10)
}

orders <- data.frame(
  order_id = 1:(n_app + n_web),
  channel  = c(rep("app", n_app), rep("web", n_web)),
  basket   = round(c(a, w), 0)
)
orders <- orders[sample(nrow(orders)), ]
orders$order_id <- 1:nrow(orders)
rownames(orders) <- NULL

save(orders, file = "orders.Rda")

app <- orders$basket[orders$channel == "app"]
web <- orders$basket[orders$channel == "web"]
SE  <- sqrt(var(app) / n_app + var(web) / n_web)
tt  <- (mean(app) - mean(web)) / SE
v   <- min(n_app - 1, n_web - 1)
cat("n app", n_app, " n web", n_web, "\n")
cat("mean app", round(mean(app), 1), " mean web", round(mean(web), 1),
    " gap", round(mean(app) - mean(web), 1), "\n")
cat("sd app", round(sd(app), 1), " sd web", round(sd(web), 1), " SE", round(SE, 2), "\n")
cat("t", round(tt, 3), " | shortcut df", v,
    " t cut", round(qt(0.95, v), 3), " normal cut", round(qnorm(0.95), 3), "\n")
cat("p (t)", round(1 - pt(tt, v), 4), " | p (normal)", round(1 - pnorm(tt), 4), "\n")
