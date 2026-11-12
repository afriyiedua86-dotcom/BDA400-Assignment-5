stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  if (length(valid_rsi) < k_period + d_period - 1)
    stop("Not enough RSI values for requested smoothing periods")
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  if (max_rsi == min_rsi) {
    normalized <- rep(0, length(valid_rsi))
  } else {
    normalized <- (valid_rsi - min_rsi) / (max_rsi - min_rsi)
  }
  k_line <- sma(normalized, k_period)
  d_line <- sma(k_line, d_period)
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
