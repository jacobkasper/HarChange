library(reshape2); library(cdlTools)
saland <- read.csv("tabula-74_ScupAssessmentLandings2015ScupBenchmarkStockAssessment.csv")
head(saland)##metic tons
noaaland <- read.csv("noaa_scup_landings.csv")##lbs
names(noaaland) <- tolower(names(noaaland))
noaaland$nmfs.name       <- NULL
noaaland$collection      <- NULL
noaaland$confidentiality <- NULL
noaaland$landings.kg <- round(noaaland$pounds * 0.453592)

noaaland$pounds          <- NULL
noaaland$state <- fips(noaaland$state)
head(noaaland)


head(saland)
saland[2:10]  <- saland[2:10] *1000 ## landings in kg
saland <- melt(saland, id.vars = "Year")
names(saland) <- c("year", "state", "landings.kg")
saland$state <- fips(saland$state)



min(noaaland$year)
head(saland)
scup.land <- merge(noaaland, saland, all = TRUE, by = c("state", "year") )
names(scup.land)[3:4] <- c("noaaland", "saland")
scup.land  <- scup.land[scup.land$year > 1990, ]
head(scup.land)

library(dplyr)

scup.land <- scup.land %>% 
    mutate(saland = coalesce(noaaland, saland))

head(scup.land)
scup.land$noaaland <- NULL
names(scup.land)[3]  <- "com.landing.kg"
head(scup.land)
write.csv(scup.land, "scup_commercial_landings_kg.csv")
dcast(scup.land, com.landing.kg ~ year + state)
scup.land.long <- dcast(scup.land, year  ~state)
scup.land.long$total <- rowSums(scup.land.long[2:13], na.rm = TRUE)/1000
