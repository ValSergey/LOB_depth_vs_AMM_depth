library(data.table)
library(ggplot2)
library(openxlsx)

# Load order book
ob <- fread("btc_best.csv")
colnames(ob) <- c("timestamp", "bid_price", "bid_size", "ask_price", "ask_size")

colnames(ob) <- c("timestamp", "bid_price", "bid_size", "ask_price", "ask_size")
ob[, timestamp := as.POSIXct(timestamp)]
ob[, mid := (bid_price + ask_price) / 2]
ob[, total_size := bid_size + ask_size]

# Liquidity scaling factor
alpha <- 50

# LOB‑calibrated AMM reserves
ob[, x_reserve := alpha * total_size]
ob[, y_reserve := x_reserve * mid]
ob[, k := x_reserve * y_reserve]

# Pick a row (example: row 100)
row <- ob[100]

x0 <- row$x_reserve
y0 <- row$y_reserve
k  <- row$k

# AMM invariant curve R(x) vs R(y)
amm_xy <- function(x0, y0, n = 400) {
  x_vals <- seq(x0 * 0.2, x0 * 2, length.out = n)  # avoid x=0
  y_vals <- (x0 * y0) / x_vals
  data.table(x = x_vals, y = y_vals)
}

curve <- amm_xy(x0, y0)

# Plot R(x) vs R(y)
ggplot(curve, aes(x = x, y = y)) +
  geom_line(color = "steelblue", linewidth = 1.3) +
  labs(
    title = "LOB‑Calibrated AMM Invariant Curve (x·y = k)",
    x = "R(x) — Base Reserve (BTC)",
    y = "R(y) — Quote Reserve (USD)"
  ) +
  theme_minimal()
write.xlsx(curve, "amm_curve.xlsx", overwrite = TRUE)
