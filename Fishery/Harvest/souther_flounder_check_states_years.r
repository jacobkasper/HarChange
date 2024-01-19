sf <- read.csv("mrip_SOUTHERN_FLOUNDER_catch_series.csv")
sf <- read.csv("mrip_SUMMER_FLOUNDER_catch_series.csv")

sf <- read.csv("mrip_STRIPED_BASS_catch_series.csv")

sf <- read.csv("mrip_TAUTOG_catch_series.csv")
sf <- read.csv("mrip_ATLANTIC_COD_catch_series.csv")
sf <- read.csv("mrip_RED_DRUM_catch_series.csv")
sf <- read.csv("mrip_SPANISH_MACKEREL_catch_series.csv")
sf <- read.csv("mrip_SPOTTED_SEATROUT_catch_series.csv")
sf <- read.csv("mrip_SCUP_catch_series.csv")
sf <- read.csv("mrip_SHEEPSHEAD_catch_series.csv")
sf <- read.csv("mrip_ATLANTIC_CROAKER_catch_series.csv")

names(sf) <- tolower(names(sf))
table(sf$year, sf$state)
head(sf)

tmp <- aggregate(sf[, 6], by = list(sf$state), sum)
381003/sum(tmp[2])

hist(sf[, 6])
sum(sf[, 6][sf$state == "VIRGINIA"] )

wf <- read.csv("mrip_WINTER_FLOUNDER_catch_series.csv")
names(wf) <- tolower(names(wf))
table(wf$year, wf$state)


wf <- read.csv("mrip_WEAKFISH_catch_series.csv")
wf <- read.csv("mrip_HADDOCK_catch_series.csv")
wf <- read.csv("mrip_BLACK_SEA_BASS_catch_series.csv")

names(wf) <- tolower(names(wf))
table(wf$year, wf$state)

table(wf$year[wf$state =="MASSACHUSETTS"], wf$wave[wf$state =="MASSACHUSETTS"])
