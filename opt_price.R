library(jsonlite)
library(anytime)
library(Quandl)
source('opt_price_funs.R')
#TICKER <- "BHGE"
start_date <- '2014-01-01'
trading_days <- read.csv('trading_days.csv')
trading_days$date <- as.Date(trading_days$date)
days_left <- function(unix_exp) {
    current_date <- Sys.Date()
    today <- which(trading_days$date == current_date)
    expire_date <- which(anydate(unix_exp) == trading_days$date)
    return(sum(trading_days$trading[today:expire_date]))
}
days_left <- Vectorize(days_left)
exp_url <- paste0('https://query2.finance.yahoo.com/v7/finance/options/',
                  TICKER)
main_json <- fromJSON(url(exp_url))
quote <- main_json[[1]]$result$quote
expirations <- main_json$optionChain$result$expirationDates[[1]]
ref_date <- get_date(expirations, 10)
target_date <- get_date(expirations, 100)

#---------------------------------------------#
#  Retrieve Options Chain for Target Date     #
#---------------------------------------------#
target_opts <- get_options(TICKER, target_date)
ref_opts <- get_options(TICKER, ref_date)
target_calls <- target_opts$calls
ref_calls <- ref_opts$calls

#---------------------------------------------#
#  Estimate Model using Quandl Price Data     #
#---------------------------------------------#
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
price_series <- a[, 2]
gamma1 <- 1
kbar <- 7
est <- MSM_CF(price_series, rawret=0, kbar, gamma1)
m0 <- est$parvec[1]
b <- est$parvec[2]
sigma <- est$sigma


#---------------------------------------------#
#  Simulate Model and Compute Price           #
#---------------------------------------------#
sim_no <- 200
S0 <- price_series[length(price_series)]
NumDays <- days_left(target_date)
path_mat <- matrix(NA, sim_no, NumDays)
for (i in 1:sim_no) {
    sim_mat <- MSMsimulation(S0, kbar, b, m0, gamma1, sigma, NumDays)
    path_mat[i, ] <- sim_mat[, ncol(sim_mat)]
}
plot(c(0, 100), c(min(path_mat), max(path_mat)), type = 'n')
for (i in 1:nrow(path_mat)) {
    lines(path_mat[i, ], type = 'l', col='grey')
}
abline(h = S0, col = 'white', lty = 2)

final_prices <- path_mat[, ncol(path_mat)]
call_pricer <- function(strike, final_prices) {
    price_vec <- rep(NA, length(strike))
    for (i in 1:length(strike)) {
        end_vals <- ifelse(final_prices > strike[i], 
                           final_prices - strike[i], 0)
        mean <- mean(end_vals)
        price_vec[i] <- mean
    }
    return(price_vec)
}

last_closing_price <- quote$regularMarketPrice
qtiles <- seq(0.01, 1, by = 1/200)
mads <- rep(NA, length(qtiles))
for (i in 1:length(qtiles)) {
    temp_final_prices <- final_prices[final_prices < 
            quantile(final_prices, qtiles[i])]
    actual_price <- target_calls$lastPrice[1]
    pred_price <- call_pricer(target_calls$strike[1], temp_final_prices)
    mads[i] <- abs(actual_price - pred_price)
}
best_spot <- which.min(mads)
threshold <- quantile(final_prices, qtiles[best_spot])
final_prices <- final_prices[final_prices < threshold]

call_table <- matrix(NA, nrow(target_calls), 2)
call_table[, 1] <- target_calls$lastPrice
prices <- call_pricer(target_calls$strike, final_prices)
call_table[, 2] <- round(prices, 2)
colnames(call_table) <- c("Actual Price", "Fractal Price")
call_table
ratios <- call_table[, 1] / call_table[, 2]
ratios

kelly_bet <- function(opt_price, strike, budget) {
    opt_price <- opt_price  + 12/100
    ITM <- ifelse(final_prices > strike, TRUE, FALSE)
    made_money <- ifelse(final_prices - strike > opt_price, TRUE, FALSE)
    OTM <- ifelse(final_prices <= strike, TRUE, FALSE)
    gains <- ifelse(made_money, final_prices - strike - opt_price, NA)
    small_losses <- ifelse(ITM & !made_money, 
        final_prices - strike - opt_price, 0)
    bigger_losses <- ifelse(OTM, -opt_price, 0)
    losses <- small_losses + bigger_losses
    losses <- ifelse(losses == 0, NA, losses)
    p <- mean(made_money)
    q <- 1 - p
    mean_gain <- mean(gains, na.rm = T)
    mean_loss <- abs(mean(losses, na.rm = T))
    R <- mean_gain / mean_loss
    f_star <- max(0, p - q / R)
    f_star <- min(f_star, 1)
    return(list(bet = f_star * budget, p = p))
}

table <- matrix(0, nrow(target_calls), 3)
table[, 3] <- target_calls$lastPrice * 100
for(i in 1:nrow(target_calls)) {
    kb <- kelly_bet(target_calls$lastPrice[i], target_calls$strike[i], 1000)
    table[i, 2] <- round(kb$bet)
    table[i, 1] <- round(kb$p, 2)
}
table <- cbind(table, target_calls$strike)
colnames(table) <- c("Win Prob", "Kelly Bet", "Actual Price", "Strike")
call_table
table
ratios
which.min(ratios) == length(ratios)
last_closing_price
anydate(target_date)
table[which.min(ratios), 4]
save.image(paste0('~/Documents/Fractals/Data/', TICKER, '_opt_data.RData'))
