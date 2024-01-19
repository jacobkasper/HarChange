##install.packages('blsAPI')
##library(blsAPI)

library(reshape2)
##data downlaoded on 11/20/2020 in 2019 dollars, from
##https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-households.html
income.m <- read.csv("Table H-8 Median Household Income by State 1984 to 2019_median.csv")

income.se <- read.csv("Table H-8 Median Household Income by State 1984 to 2019_se.csv")

names(income.m)[2:30] <- c(2019:1991)
names(income.se)[2:30] <- c(2019:1991)

household.income <- melt(income.m)
names(household.income)[2:3] <- c("year", "median")
head(household.income)
income.se <- melt(income.se)
head(income.se)
names(income.se)[2:3] <- c("year", "se")
household.income <- merge(household.income, income.se, by = c("state", "year"))
write.csv(household.income, "data.ready/household_income.csv")

popd <- read.csv("populationDensity.csv")
head(popd)
popd <- popd[popd$Region != "United States", ]
names(popd)[1] <- "state"
write.csv(popd, "data.ready/population_density.csv")


###household size
setwd("Household Size/tables")
hs05 <- read.csv("ACS_05_EST_B25010_with_ann.csv")
head(hs05)
hs05 <- hs05[3:5]
names(hs05) <- c("state", "estimate", "error")
hs05$year <- 2005

hs06 <- read.csv("ACS_06_EST_B25010_with_ann.csv")
hs06 <- hs06[3:5]
names(hs06) <- c("state", "estimate", "error")
hs06$year <- 2006

hs07 <- read.csv("ACS_07_1YR_B25010_with_ann.csv")
hs07 <- hs07[3:5]
names(hs07) <- c("state", "estimate", "error")
hs07$year <- 2007

hs08 <- read.csv("ACS_08_1YR_B25010_with_ann.csv")
hs08 <- hs08[3:5]
names(hs08) <- c("state", "estimate", "error")
hs08$year <- 2008

hs09 <- read.csv("ACS_09_1YR_B25010_with_ann.csv")
hs09 <- hs09[3:5]
names(hs09) <- c("state", "estimate", "error")
hs09$year <- 2009

hs10 <- read.csv("ACS_10_1YR_B25010_with_ann.csv")
hs10 <- hs10[3:5]
names(hs10) <- c("state", "estimate", "error")
hs10$year <- 2010

hs11 <- read.csv("ACS_11_1YR_B25010_with_ann.csv")
hs11 <- hs11[3:5]
names(hs11) <- c("state", "estimate", "error")
hs11$year <- 2011

hs12 <- read.csv("ACS_12_1YR_B25010_with_ann.csv")
hs12 <- hs12[3:5]
names(hs12) <- c("state", "estimate", "error")
hs12$year <- 2012

hs13 <- read.csv("ACS_13_1YR_B25010_with_ann.csv")
hs13 <- hs13[3:5]
names(hs13) <- c("state", "estimate", "error")
hs13$year <- 2013

hs14 <- read.csv("ACS_14_1YR_B25010_with_ann.csv")
hs14 <- hs14[3:5]
names(hs14) <- c("state", "estimate", "error")
hs14$year <- 2014

hs15 <- read.csv("ACS_15_1YR_B25010_with_ann.csv")
hs15 <- hs15[3:5]
names(hs15) <- c("state", "estimate", "error")
hs15$year <- 2015

hs16 <- read.csv("ACS_16_1YR_B25010_with_ann.csv")
hs16 <- hs16[3:5]
names(hs16) <- c("state", "estimate", "error")
hs16$year <- 2016

hs17 <- read.csv("ACS_17_1YR_B25010_with_ann.csv")
hs17 <- hs17[3:5]
names(hs17) <- c("state", "estimate", "error")
hs17$year <- 2017

hs18 <- read.csv("ACSST1Y2018.S1101_data_with_overlays_2020-12-14T095810.csv")
head(hs17)
head(hs18)
hs18 <- hs18[2:4]
names(hs18) <- c("state", "estimate", "error")
hs18$year <- 2018

hs90.00 <- read.csv("1990-2000_household_size.csv")
hs90.00 <- hs90.00[rep(seq_len(nrow(hs90.00)), 11), ]
hs90.00$year <- c(rep(1990, 51), rep(1991, 51), rep(1992, 51), rep(1993, 51),
                  rep(1994, 51), rep(1995, 51), rep(1996, 51), rep(1997, 51),
                  rep(1998, 51), rep(1999, 51), rep(2000, 51))
names(hs90.00) <- c("state", "estimate", "year")
hs90.00$error <- NA
hs90.00 <- hs90.00[c(1, 3, 2, 4)]


hs01.04 <- read.csv("2001-2004_household_size.csv")
dim(hs01.04)
hs01.04 <- hs01.04[rep(seq_len(nrow(hs01.04)), 4), ]
hs01.04$year <- c(rep(2001, 51), rep(2002, 51), rep(2003, 51), rep(2004, 51))
names(hs01.04) <- c("state", "estimate", "year")
hs01.04$error <- NA
hs01.04 <- hs01.04[c(1, 3, 2, 4)]

hs <- rbind(hs90.00, hs01.04, hs05, hs06, hs07, hs08, hs09, hs10,
            hs11, hs12, hs13, hs14, hs15, hs16, hs17, hs18)
head(hs)
names(hs)[3:4] <-  c("household_mean", "household_error")
write.csv(hs, "../../data.ready/household_size.csv")



###fuel prices
fp <- read.csv("fuel_prices.csv")

head(fp)
##PADD 1A == NE
##PADD 1B == MD, DE, NJ, NY
##PADD 1C == VA, NC, SC, GA, FL
##PADD 3 == Gulf of Mexico
str(fp)
library(lubridate)
fp$Date <- as.Date(fp$Date, format = "%m/%d/%Y")
fp$month <- month(fp$Date)
fp$year <- year(fp$Date)
fp$wave <-  ifelse(fp$month %in% 1:2, 1,
            ifelse(fp$month %in% 3:4, 2,
            ifelse(fp$month %in% 5:6, 3,
            ifelse(fp$month %in% 7:8, 4,
            ifelse(fp$month %in% 9:10, 5,6)))))

       
head(fp[1:11], 20)
fp <-     aggregate(fp[2:11], list(fp$year, fp$wave), mean, na.rm = T)
names(fp)[1:2] <- c("year", "wave")

write.csv(fp, "data.ready/fuel.csv")
