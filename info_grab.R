sp_data <- read.csv('s_and_p_tickers.csv', stringsAsFactors = F, header = T)
sp_tickers <- sp_data$Ticker.symbol


for (l in 1:length(sp_tickers)) {
    TICKER <- sp_tickers[l]
    tryCatch({
        source('opt_price.R')
    }, error = function(e){cat(" ERROR :", conditionMessage(e), "\n")})
}