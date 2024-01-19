library(cdlTools)
pop90s <- read.csv("tabula-co-est2001-12-00.csv")
pop00s <- read.csv("nst-est2009-01.csv")
pop10s <- read.csv("nst-est2019-01.csv")

head(pop90s)
head(pop00s)
head(pop10s)



cc  <- read.csv("../../R/Environmental/Data/csv/coastline-counties-list.csv")
st.fips <- unique(cc$STATE.FIPS[cc$COASTLINE.REGION %in% c("Atlantic", "Gulf of Mexico")])

st.fips

pop90s$state <- fips(pop90s$state)
pop00s$state <- fips(pop00s$state)
pop10s$state <- fips(pop10s$state)

pop90s <- pop90s[pop90s$state %in% st.fips, ]
pop00s <- pop00s[pop00s$state %in% st.fips, ]
pop10s <- pop10s[pop10s$state %in% st.fips, ]

names(pop90s)[3:13] <- 1990:2000
names(pop00s)[2:10] <- 2009:2001
names(pop10s)[4:12] <- 2010:2018


names(pop90s)
pop90s$Base         <- NULL
names(pop00s)
pop00s[11:13]       <- NULL
pop10s[c(2, 3, 13)] <- NULL
names(pop10s)

head(pop90s)
head(pop00s)
head(pop10s)

pop90s <- reshape2::melt(pop90s, id  = c("state"))
pop00s <- reshape2::melt(pop00s, id  = c("state"))
pop10s <- reshape2::melt(pop10s, id  = c("state"))

names(pop90s) <- c("state", "year", "population")
names(pop00s) <- c("state", "year", "population")
names(pop10s) <- c("state", "year", "population")

pop <- rbind(pop90s, pop00s, pop10s)

area <- read.csv("../AreaByState.csv")
names(area) <- c("state", "area")
area$state  <- fips(area$state)

pop <- merge(pop, area, by = c("state"))
pop$density <- pop$population/pop$area
write.csv(pop, "../data.ready/population_density2.csv")
