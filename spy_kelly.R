library(QuantTools)
library(anytime)
library(gtools)
library(Quandl)
setwd('~/Documents/Fractals')
df <- read.csv('HistoricalQuotes.csv')
open <- df$open[length(df$open):1]
close <- df$close[length(df$open):1]
chg <- close - open

plot(chg, type = 'l')
T <- length(open)
gaps <- open[2:T] - close[1:(T - 1)]

kelly_bet <- function(returns) {
    wins <- returns > 0
    p <- sum(wins) / length(returns)
    R <- mean(returns[wins]) / abs(mean(returns[!wins]))
    q <- 1 - p
    f_star <- max(0, p - q / R)
    f_star <- min(f_star, 1)
    return(list(f_star=f_star, p=p, R=R))
}

kelly_bet(gaps)
kelly_bet(chg)
kelly_bet(close[2:T] - close[1:(T - 1)])
kelly_bet(open[2:T] - open[1:(T - 1)])
bets <- rep(0, 100)
for (i in 1:1000) {
    print(i)
    start_1 <- 1 + i
    end_2 <- T - i
    a <- kelly_bet(close[start_1:T] - close[1:end_2])
    a
    bets[i] <- a$f_star 
}

simulator <- function(R, series) {
    ctc <- close[2:T] - close[1:(T - 1)]
    returns <- ctc / close[1:(T - 1)]
    end_state <- returns[T - 1]
    spot <- which.min(end_state - returns[-(T - 1)])
    sim_returns <- rep(0, R)
    sim_prices <- rep(0, R)
    index <- 1:length(returns)
    last_price <- close[T]
    for (r in 1:R) {
        ind_dist <- abs(spot - index)
        dens_vals <- 1/ind_dist[-spot]
        weights <- dens_vals / sum(dens_vals)
        returns_temp <- returns[-spot]
        index_temp <- index[-spot]
        spot <- sample(index_temp, size=1, prob=weights)
        sim_returns[r] <- returns_temp[which(index_temp == spot)]
        last_price <- last_price + last_price * sim_returns[r]
        sim_prices[r] <- last_price
        stopifnot(!is.na(sum(sim_prices[r])))
    }
    return(sim_prices)
}  
R <- 14
S <- 1000
mat <- matrix(0, S, R)
for (s in 1:S) {
    mat[s, ] <- simulator(R, close)
    print(s)
}
y_min <- min(c(mat))
y_max <- max(c(mat))
plot(c(1, R + 1), c(y_min, y_max), type = 'n')
for (s in 1:S) lines(2:(R + 1), mat[s, ], col = col_blue)
abline(h = close[T], col = 'white', lwd = 1)
sum(mat[, R] > 250 + 10.35) / S
