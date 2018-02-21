library(anytime)
setwd('~/Documents/Fractals')
files <- list.files('~/Documents/Fractals/Data')
files2 <- paste0('Data/', files)
call_list <- list()
kelly_list <- list()
ratio_list <- list()
info_table <- matrix(NA, nrow = length(files), ncol = 2)
rownames(info_table) <- files
dates <- rep(NA, length(files))
for (j in 1:length(files2)) {
    load(files2[j])
    file_ticker <- strsplit(files[j], "_")[[1]][1]
    stopifnot(file_ticker == TICKER)
    kelly_list[[j]] <- table
    call_list[[j]] <- call_table
    ratio_list[[j]] <- ratios
    info_table[j, 1] <- last_closing_price
    info_table[j, 2] <- table[which.min(ratios), 4]
    rownames(info_table)[j] <- TICKER
    dates[j] <- anydate(target_date)
    print(TICKER)
}
dates <- as.Date(dates)
my_index <- list()
avg_index <- rep(NA, length(files))
for (j in 1:length(files)) {
    my_index[[j]] <- kelly_list[[j]][, 1] / ratio_list[[j]]
    avg_index[j] <- mean(my_index[[j]])
}

# rowname is ticker, 4 columns from kelly table, date, and my index value
num_ideas <- 10
idea_list <- list(num_ideas)
best <- head(order(avg_index, decreasing = T), num_ideas)
for (k in 1:num_ideas) {
    spot <- best[k]
    call_spot <- which.max(my_index[[spot]])
    vec <- c(kelly_list[[spot]][call_spot, ], my_index[[spot]][call_spot])
    names(vec)[5] <- "My Index"
    idea_list[[k]] <- list(ticker = rownames(info_table)[spot], info = vec,
        date = dates[spot])
}
idea_list

