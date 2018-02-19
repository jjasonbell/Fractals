setwd('/Users/jasonbell/Desktop/Fractals')
# create trading day data frame for use in options pricing
holidays_2018_2020 <- as.Date(c('2018-01-01',                 # 2018
    '2018-01-15', '2018-02-19', '2018-03-30', '2018-05-28', 
    '2018-07-04', '2018-09-03', '2018-11-22', '2018-12-25',
    '2019-01-01',                                             # 2019
    '2019-01-21', '2019-02-18', '2019-04-19', '2019-05-27', 
    '2019-07-04', '2019-09-02', '2019-11-28', '2019-12-25',
    '2020-01-01',                                             # 2020
    '2020-01-20', '2020-02-17', '2020-04-10', '2020-05-25', 
    '2020-07-03', '2020-09-07', '2020-11-26', '2020-12-25'
))
dates <- seq.Date(from = as.Date("2018-02-16"), 
    to = as.Date("2020-12-31"), by = 1)
trading <- rep(1, length(dates))
trading <- ifelse(weekdays(dates) == 'Sunday' | weekdays(dates) == 'Saturday',
    0, trading)
trading <- ifelse(dates %in% holidays_2018_2020, 0, trading)
trading_days <- data.frame(date = dates, trading = trading)
write.csv(trading_days, 'trading_days.csv', row.names = FALSE)