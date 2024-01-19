##remotes::install_github("mikejohnson51/AOI")
##remotes::install_github("mikejohnson51/climateR")
#update.packages(ask = FALSE)
library(climateR); library(raster)
library(AOI); library(readxl); library(cdlTools)
library(sf); library(tidyverse)
library(lubridate)
##citation("climateR")
##citation("AOI")

##read in coastal counties from the us
cc  <- read.csv("Data/csv/coastline-counties-list.csv")
##coastal states
st.at <- unique(cc$STATE.FIPS[cc$COASTLINE.REGION == "Atlantic"])
st.gm <- unique(cc$STATE.FIPS[cc$COASTLINE.REGION == "Gulf of Mexico"])

dates <- as.data.frame(read_excel("../aux_data/yr_wv_date.xlsx"))
doi <-
    dates %>%
    mutate(start = as.Date(dates$start.date, '%m/%d/%y'),
           end   =  as.Date(dates$end.date,  '%m/%d/%y')) %>%
    select(start, end) %>%
    filter(year(start) > 1996)

cc.at <- cc %>%
   filter(STATE.FIPS %in% st.at,
          COASTLINE.REGION == "Atlantic")
cc.gm <- cc %>%
    filter(STATE.FIPS %in% st.gm,
           COASTLINE.REGION == "Gulf of Mexico",
           STATE.NAME != "Texas")



f = system.file("co/cities_colorado.rds", package = "climateR")
cities = readRDS(f)

gm.out <- NULL
for(s in unique(cc.gm$STATE.FIPS)){
    cc.tmp           <- cc.gm[cc.gm$STATE.FIPS == s, ]
    county.tmp       <- unlist(strsplit(cc.tmp$COUNTY.NAME,
                                        " County"))
    county.tmp       <- gsub(' city', '' , county.tmp)
    county.tmp       <- gsub(' Parish', '' , county.tmp)
    aoi.tmp          <- aoi_get(state = fips(s, to = "Name"),
                                county = county.tmp)
    st_crs(aoi.tmp$geometry) <- 4326
    doi.tmp          <- doi
    st.tmp <- NULL
    for(t in 1:nrow(doi.tmp)){
        a <-  as.character(doi.tmp$start[t])
        b <-  as.character(doi.tmp$end[t])
        terr.tmp <-
            getTerraClim(AOI = aoi.tmp,
                         varname = c('tmax', 'ppt', 'ws'),
                         startDate = a,
                         endDate   = b)
        prcp <-
            as_tibble(as.data.frame((terr.tmp$ppt))) %>%
            mutate(sum = rowSums(.[1:2])) %>%
            drop_na() %>%
            summarize(mean = mean(sum))
        temp <-
            as_tibble(as.data.frame((terr.tmp$tmax))) %>%
            mutate(mean = rowMeans(across(where(is.numeric)))) %>%
            drop_na() %>%
            summarize(mean = mean(mean))
        wind <-
            as_tibble(as.data.frame((terr.tmp$ws))) %>%
            mutate(mean = rowMeans(across(where(is.numeric)))) %>%
            drop_na() %>%
            summarize(mean = mean(mean))
        st.climate.tmp <- data.frame(prcp, temp, wind)
        st.tmp <- rbind(st.tmp, st.climate.tmp)
        closeAllConnections()
        print(s)
        print(t)
    }
    colnames(st.tmp) <- c("precip", "temp", "wind")
    st.tmp$state <- s
    st.tmp <- cbind(st.tmp, doi.tmp)
    gm.out <- rbind(gm.out, st.tmp)
    print(s)
}

gm.out$coast  <- "Gulf of Mexico"
write.csv(gm.out, "Gulf_met23.csv", row.names = FALSE)


at.out <- NULL
for(s in unique(cc.at$STATE.FIPS)){
    cc.tmp           <- cc.at[cc.at$STATE.FIPS == s, ]
    county.tmp       <- unlist(strsplit(cc.tmp$COUNTY.NAME,
                                        " County"))
    county.tmp       <- gsub(' city', '' , county.tmp)
    county.tmp       <- gsub(' Parish', '' , county.tmp)
    aoi.tmp          <- aoi_get(state = fips(s, to = "Name"),
                                county = county.tmp)
    st_crs(aoi.tmp$geometry) <- 4326
    doi.tmp          <- doi
    st.tmp <- NULL
    for(t in 1:nrow(doi.tmp)){
        a <-  as.character(doi.tmp$start[t])
        b <-  as.character(doi.tmp$end[t])
        terr.tmp <-
            getTerraClim(AOI = aoi.tmp,
                         varname = c('tmax', 'ppt', 'ws'),
                         startDate = a,
                         endDate   = b)
        prcp <-
            as_tibble(as.data.frame((terr.tmp$ppt))) %>%
            mutate(sum = rowSums(.[1:2])) %>%
            drop_na() %>%
            summarize(mean = mean(sum))
        temp <-
            as_tibble(as.data.frame((terr.tmp$tmax))) %>%
            mutate(mean = rowMeans(across(where(is.numeric)))) %>%
            drop_na() %>%
            summarize(mean = mean(mean))
        wind <-
            as_tibble(as.data.frame((terr.tmp$ws))) %>%
            mutate(mean = rowMeans(across(where(is.numeric)))) %>%
            drop_na() %>%
            summarize(mean = mean(mean))
        st.climate.tmp <- data.frame(prcp, temp, wind)
        st.tmp <- rbind(st.tmp, st.climate.tmp)
        closeAllConnections()
        print(s)
        print(t)
    }
    colnames(st.tmp) <- c("precip", "temp", "wind")
    st.tmp$state <- s
    st.tmp <- cbind(st.tmp, doi.tmp)
    at.out <- rbind(at.out, st.tmp)
    print(s)
}

at.out$coast  <- "Atlantic"
write.csv(gm.out, "Atlantic_met23.csv", row.names = FALSE)










