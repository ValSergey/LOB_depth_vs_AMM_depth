library(tidyverse)
library(ggplot2)

lob <- read_csv("btc_depth100.csv")



bids <- lob %>%
  select(price = bids.price, size = bids.quantity) %>%
  filter(size > 0) %>%
  arrange(desc(price)) %>%
  mutate(cum_size = -cumsum(size), side = "bid")

asks <- lob %>%
  select(price = asks.price, size = asks.quantity) %>%
  filter(size > 0) %>%
  arrange(price) %>%
  mutate(cum_size = cumsum(size), side = "ask")

depth <- bind_rows(bids, asks)

#---------------------------------------------------------------------------------#
fit_data <- depth %>%
  arrange(cum_size)

#Compute mid‑price for good starting values
mid <- (max(bids$price) + min(asks$price)) / 2
#
start_Rx <- max(abs(fit_data$cum_size)) * 2
start_Ry <- mid * start_Rx

fit <- nls(
  price ~ Ry / (Rx - cum_size),
  data = fit_data,
  start = list(Rx = start_Rx, Ry = start_Ry),
  algorithm = "port",
  lower = c(Rx = max(fit_data$cum_size) * 1.01, Ry = 0),
  upper = c(Inf, Inf),
  control = nls.control(maxiter = 200, warnOnly = TRUE)
)

coef(fit)

Rx_hat <- coef(fit)[["Rx"]]
Ry_hat <- coef(fit)[["Ry"]]

amm <- tibble(
  cum_size = seq(min(depth$cum_size), max(depth$cum_size), length.out = 400)
) %>%
  mutate(
    price = Ry_hat / (Rx_hat - cum_size),
    side = ifelse(cum_size < 0, "bid", "ask")
  )



ggplot() +
  geom_line(data = depth, aes(x = cum_size, y = price, color = side), size = 1.3) +
  geom_line(data = amm, aes(x = cum_size, y = price),
            color = "purple", size = 1.1, linetype = "dashed") +
  labs(
    title = "AMM Fit to Both Sides of LOB: P(q) = Ry / (Rx - q)",
    x = "Cumulative size q",
    y = "Price"
  ) +
  theme_minimal()
#------------------------------------------------------------------------#
library(writexl)

exportToExcel <- data.frame(
  Lob = depth,
  Amm = amm
)

write_xlsx(exportToExcel, "LOBvsAMM.xlsx")

write.table(
  exportToExcel,
  file = "LOBvsAMM.csv",
  sep = ",",
  append = TRUE,
  col.names = !file.exists("LOBvsAMM.csv"),
  row.names = FALSE
)
#------------------------------------------------------------------------#
# Reference price
P0 <- mid

lob_slippage <- depth %>%
  mutate(
    slippage = price / P0 - 1
  )

amm_slippage <- tibble(
  cum_size = seq(min(depth$cum_size), max(depth$cum_size), length.out = 400)
) %>%
  mutate(
    price = Ry_hat / (Rx_hat - cum_size),
    slippage = price / P0 - 1
  )


ggplot() +
  geom_line(data = lob_slippage,
            aes(x = cum_size, y = slippage, color = side),
            size = 1.3) +
  geom_line(data = amm_slippage,
            aes(x = cum_size, y = slippage),
            color = "purple", size = 1.1, linetype = "dashed") +
  labs(
    title = "Marginal Slippage: LOB vs AMM",
    x = "Trade size q (cumulative)",
    y = "Marginal slippage (relative to mid)"
  ) +
  theme_minimal()

#---------------------------------------------------------------------------------#
fit_data <- depth %>%
  filter(cum_size > 0) %>%
  arrange(cum_size)
mid <- (max(bids$price) + min(asks$price)) / 2

start_Rx <- max(fit_data$cum_size) * 2
start_Ry <- mid * start_Rx

fit <- nls(
  price ~ Ry / (Rx - cum_size),
  data = fit_data,
  start = list(Rx = start_Rx, Ry = start_Ry),
  algorithm = "port",
  lower = c(Rx = max(fit_data$cum_size) * 1.01, Ry = 0),
  upper = c(Inf, Inf),
  control = nls.control(maxiter = 200, warnOnly = TRUE)
)

coef(fit)
Rx_hat <- coef(fit)[["Rx"]]
Ry_hat <- coef(fit)[["Ry"]]

amm <- tibble(
  cum_size = seq(min(depth$cum_size), max(depth$cum_size), length.out = 400)
) %>%
  mutate(
    price = Ry_hat / (Rx_hat - cum_size),
    side = ifelse(cum_size < 0, "bid", "ask")
  )


ggplot() +
  geom_line(data = depth, aes(x = cum_size, y = price, color = side), size = 1.3) +
  geom_line(data = amm, aes(x = cum_size, y = price),
            color = "purple", size = 1.1, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "AMM fit: P(q) = Ry / (Rx - q)",
    x = "Cumulative size q",
    y = "Price"
  )


