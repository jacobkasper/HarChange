library(lubridate)
rm(list = ls())
leaps <- seq(1952, 2016, 4)

years <- c(1995:2018)
reg <- read.csv("bluefish_to_expand.csv")
reg
reg$start_date <- as.Date(reg$start_date, format = "%m/%d/%Y")
reg$end_date <- as.Date(reg$end_date, format = "%m/%d/%Y")

years

out <- NULL
for(n in years){
    tmp <- reg
    day(tmp$end_date) <- ifelse(day(tmp$end_date) == 29 &
                                month(tmp$end_date) == 02,
                                28,
                                day(tmp$end_date))
    year(tmp$start_date) <- n
    year(tmp$end_date)   <- n

    day(tmp$end_date) <-
        ifelse(year(tmp$end_date) %in% leaps == "TRUE" &
               month(tmp$end_date) == 02 &
               day(tmp$end_date) == 28,
               29,
               day(tmp$end_date))
    tmp$Year <- n
    out <- rbind(out, tmp)
}

out <- rbind(reg, out)
out$end_date
head(out)
tail(out)
write.csv(out, "expanded_regs.csv")

