library(reshape2); library(cdlTools)
noaaland <- read.csv("noaa_landings.csv")##lbs
names(noaaland) <- tolower(names(noaaland))
noaaland$nmfs.name       <- NULL
noaaland$collection      <- NULL
noaaland$confidentiality <- NULL
noaaland$landings.kg <- round(noaaland$pounds * 0.453592)
noaaland$pounds          <- NULL
noaaland$dollars          <- NULL
noaaland$area            <- ifelse(noaaland$state == "FLORIDA-WEST", "12W",
                            ifelse(noaaland$state == "FLORIDA-EAST", "12E", NA))
noaaland$state           <- fips(noaaland$state)
noaaland$state[is.na(noaaland$state)]  <- 12
head(noaaland)
names(noaaland)[3]  <- "com.landing.kg"
write.csv(noaaland, "sheepshead_commercial_landings_kg.csv")

