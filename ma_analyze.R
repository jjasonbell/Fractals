sp_data <- read.csv('s_and_p_tickers.csv', stringsAsFactors = F, header = T)
sp_tickers <- sp_data$Ticker.symbol

out <- matrix(0, 0, 3)
tick_succ <- rep(0, 0)
ratio <- rep(0, 0)
for (l in 1:length(sp_tickers)) {
    TICKER <- sp_tickers[l]
    tryCatch({
        source('price_series.R')
        row <- output[which.max(output[, 3]), ]
        out <- rbind(out, row)
        tick_succ <- c(tick_succ, TICKER)
        ratio <- c(ratio, row[3] / (open[length(open)] - open[1]))
    }, error = function(e){cat(" ERROR :", conditionMessage(e), "\n")})
}
ma_data <- data.frame(ratio, ticker = tick_succ, out)
