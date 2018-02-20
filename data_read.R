library(anytime)

files <- list.files('~/Documents/Fractals/Data')
call_list <- list()
kelly_list <- list()
ratio_list <- list()
info_table <- matrix(NA, nrow = length(files), ncol = 2)
rownames(info_table) <- files
dates <- rep(NA, length(files))
for (j in 1:length(files)) {
    load(files[j])
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
