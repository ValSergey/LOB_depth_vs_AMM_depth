if (!requireNamespace("websocket", quietly = TRUE)) {
  install.packages("websocket")
}

library(websocket)
library(jsonlite)

csv_path <- "btc_best.csv"

# Create CSV header if file does not exist
if (!file.exists(csv_path)) {
  header <-  c("timestamp", "bid_price", "bid_size", "ask_price", "ask_size")
  write.table(t(header), csv_path, sep=",", col.names=FALSE, row.names=FALSE)
}

# Binance WebSocket endpoint for BTC/USDT best bid/ask
# The stream "@bookTicker" gives real-time top-of-book updates
ws_url <- "wss://stream.binance.com:9443/ws/btcusdt@bookTicker"

# Create WebSocket connection
ws <- WebSocket$new(ws_url, autoConnect = FALSE)

# On connection open
ws$onOpen(function(event) {
  cat("Connected to Binance WebSocket for BTC/USDT best bid/ask\n")
})

# On receiving a message
ws$onMessage(function(event) {
  # Parse JSON message
  data <- fromJSON(event$data)
  
  # Extract best bid and ask
  #bid_price <- as.numeric(data$`b`)
  #bid_qty   <- as.numeric(data$`B`)
  #ask_price <- as.numeric(data$`a`)
  #ask_qty   <- as.numeric(data$`A`)
  
  ts <- Sys.time()
  
  row <- data.frame(
    timestamp = ts,
    bid_price = as.numeric(data$`b`),
    bid_size  = as.numeric(data$`B`),
    ask_price = as.numeric(data$`a`),
    ask_size  =  as.numeric(data$`A`)
  )
  
  write.table(
    row,
    file = csv_path,
    sep = ",",
    append = TRUE,
    col.names = !file.exists(csv_path),
    row.names = FALSE
  )
  
  # Print live prices
  #cat(sprintf("Best Bid: %.2f (%f BTC) | Best Ask: %.2f (%f BTC)\n",
  #            bid_price, bid_qty, ask_price, ask_qty))
})

# On error
ws$onError(function(event) {
  cat("Error:", event$message, "\n")
})

# On close
ws$onClose(function(event) {
  cat("WebSocket closed\n")
})

# Connect
ws$connect()

# Keep R running to receive messages
while (TRUE) {
  later::run_now(timeout = 1)
}
ws$close()
