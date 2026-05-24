# Install binancer if not already installed
if (!requireNamespace("binancer", quietly = TRUE)) {
  install.packages("binancer")
}

library(binancer)

# --- PARAMETERS ---
symbol <- "BTCUSDT"  # Trading pair
limit  <- 100          # Number of levels to retrieve (valid: 5, 10, 20, 50, 100, 500, 1000)

# --- GET ORDER BOOK SNAPSHOT ---
# binance_depth returns a list with bids and asks
order_book <- tryCatch({
  binance_depth(symbol = symbol, limit = limit)
}, error = function(e) {
  message("Error retrieving order book: ", e$message)
  NULL
})

# --- DISPLAY RESULT ---
if (!is.null(order_book)) {
  cat("Order Book Snapshot for", symbol, "\n")
  cat("Bids (price, quantity):\n")
  print(order_book$bids)
  cat("\nAsks (price, quantity):\n")
  print(order_book$asks)
}
csv_path <- "btc_depth100.csv"
write.table(
  order_book,
  file = csv_path,
  sep = ",",
  append = TRUE,
  col.names = !file.exists(csv_path),
  row.names = FALSE
)

