# Run from the directory containing the nine R files.
for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
            "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")) source(f)

stopifnot(identical(as.numeric(sma(c(1,2,3,4), 2)), c(1.5,2.5,3.5)))
stopifnot(isTRUE(all.equal(ema(c(10,12,14), 3), c(10,11,12.5))))
stopifnot(isTRUE(all.equal(stdev(c(1,2,3)), sqrt(2/3))))
stopifnot(isTRUE(all.equal(linreg(c(2,4,6,8), 4, 0)$slope, 2)))
stopifnot(identical(crossover(c(1,3,1), c(2,2,2)), c("None","Up","Down")))
stopifnot(identical(crossunder(c(3,1,3), c(2,2,2)), c("None","True","False")))
stopifnot(length(rsi(seq(1,30), 14)) == 30)
stopifnot(length(macd(seq(1,30), 3, 5, 2)$histogram) == 30)
stopifnot(length(stoch_rsi(sin(seq(1,60)) + seq(1,60)/10, 14, 3, 3)$d_line) > 0)
cat("All sample checks passed.\n")
