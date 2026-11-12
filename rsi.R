rsi <- function(data, period) {
  if (period < 1 || period != as.integer(period)) stop("period must be a positive integer")
  if (length(data) <= period) stop("data must contain more than period observations")
  diff_values <- diff(data)
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, length(data))
  calc_rsi <- function(g, l) {
    if (l == 0 && g == 0) return(50)
    if (l == 0) return(100)
    100 - 100 / (1 + g / l)
  }
  rsi_values[period + 1] <- calc_rsi(avg_gain, avg_loss)
  if (length(data) > period + 1) {
    for (i in (period + 2):length(data)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      rsi_values[i] <- calc_rsi(avg_gain, avg_loss)
    }
  }
  return(rsi_values)
}
