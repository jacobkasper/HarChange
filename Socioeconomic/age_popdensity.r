library(reshape)
## Age
med <- read.csv("MedianAgeByState.csv")
head(med)
colnames(med) <- c("state", as.character(seq(1980, 2018, 1)))
med <- melt(med)
colnames(med) <- c("state", "Year", "MedianAge")
write.csv(med, "data.ready/MedianAgeByState.csv",  row.names = FALSE)

##population density
pop <- read.csv("PopulationByState.csv")
colnames(pop) <- c("Region", as.character(seq(1980, 2018, 1)))
pop <- melt(pop)
colnames(pop) <- c("Region", "Year", "Population")
area <- read.csv("AreaByState.csv")
pop$area <- area$Area
pop$density <- round(pop$Population/pop$area, 2)
write.csv(pop, "PopulationDensity.csv")
