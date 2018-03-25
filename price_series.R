library(QuantTools)
library(anytime)
library(gtools)
library(Quandl)
TICKER <- "AAP"
start_date <- '2014-01-01'
QUANDL_KEY = 'HLnciyx6xt-2xHqGzWoR'
Quandl.api_key(QUANDL_KEY)
quandl_get <- function(sym, start_date = "2015-01-01") {
    tryCatch(
        Quandl(
            c(paste0("WIKI/", sym, ".8"),  #  Adj. Open
              paste0("WIKI/", sym, ".11"), # Adj. Close
              paste0("WIKI/", sym, ".12") ), # Volume 
            start_date = start_date, 
            type = "zoo"
        )
    )
}
a <- quandl_get(TICKER, start_date = start_date)
open <- as.numeric(a[, 1])
close <- as.numeric(a[, 2])
chg <- close - open
chg_l <- chg[1:(length(chg) - 1)]
mod <- lm(chg[2:length(chg)] ~ chg_l)
summary(mod)
acf(chg)
ma10 <- sma(open, 10)
prof_calc <- function(sma_n, ema_n) {
    a <- sma(open, sma_n)
    b <- ema(open, ema_n)
    begin <- max(sma_n, ema_n)
    stopifnot(length(a) == length(b))
    a <- a[begin:length(a)]
    b <- b[begin:length(b)]
    ab_diff <- a - b
    stopifnot(sum(is.na(ab_diff)) == 0)
    open2 <- open[begin:length(open)]
    S <- length(open2)
    cross <- rep(0, 0)
    for (i in 2:S) {
        if (sign(ab_diff[i - 1]) != sign(ab_diff[i])) {
            cross <- c(cross, i)
        }
    }
    profit <- rep(0, length(cross))
    for (j in 1:(length(cross) - 1)) {
        buy_ind <- ab_diff[cross[j]] > 0
        if (buy_ind) {
            profit[j] <- open2[cross[j + 1]] - open2[cross[j]]
        } else {
            profit[j] <- open2[cross[j]] - open2[cross[j + 1]]
        }
    }
    return(profit)
}
prof_calc(10, 30)
ema_n_vec <- 30:50
sma_n_vec <- 5:20
output <- matrix(0, 0, 3)
out_counter <- 0
for (i in 1:length(ema_n_vec)) {
    for (j in 1:length(sma_n_vec)) {
        to_add <- c(sma_n_vec[j], ema_n_vec[i], 
                    sum(prof_calc(sma_n_vec[j], ema_n_vec[i])))
        output <- rbind(output, to_add)
    }
}
print(output[which.max(output[, 3]), ])
print(open[length(open)] - open[1])
