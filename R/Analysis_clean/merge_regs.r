source("../Functions/standardize.r")
source("../Functions/standardize_species.r")

library(tidyverse); library(readxl); library(cdlTools);library(purrr)
source('prepreg.r')


#### socioeconomic
### gdp
gdp <-
    pivot_longer(
    read_csv(
    "../../Socioeconomic/data.ready/SAGDP1__ALL_AREAS_1997_2019.csv") %>%
    filter(Description ==
           "Current-dollar GDP (millions of current dollars)")  %>%
    mutate(state =  fips(GeoName)) %>%
    select(-c(GeoFIPS, GeoName, Region, TableName, LineCode, Description, IndustryClassification, Unit)),
    !state, names_to = 'year', values_to = 'gdp') %>%
    filter(state %in% harregs$state) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year >= 1997)
### median household income
hhi <-
    read_csv("../../Socioeconomic/data.ready/household_income.csv") %>%
    select(-1, -se) %>%
    mutate(state = fips(state)) %>%
    rename(med.income = median) %>%
    filter(state %in% harregs$state) %>%
    filter(year >= 1997)
### population density
pd <-
    read_csv("../../Socioeconomic/data.ready/population_density2.csv") %>%
    select(-c(1, population, area)) %>%
    mutate(state = fips(state)) %>%
    filter(state %in% harregs$state) %>%
    rename(pop.density = density) %>%
    filter(year >= 1997)
### median age
ma <-
    read_csv("../../Socioeconomic/data.ready/MedianAgeByState.csv") %>%
    mutate(state = fips(state)) %>%
    filter(state %in% harregs$state) %>%
    rename(year = variable, med.age = value) %>%
    filter(year >= 1997)
### household size
hs <-
    read_csv("../../Socioeconomic/data.ready/household_size.csv") %>%
    select(-1, -household_error) %>%
    mutate(state = fips(state)) %>%
    rename(med.house.size = household_mean) %>%
    filter(state %in% harregs$state) %>%
    filter(year >= 1997)
### unemployment
ue <-
    pivot_longer(read_csv("../../Socioeconomic/data.ready/unemployment.csv"),
                 !year, names_to = 'state', values_to = 'unemp.rt') %>%
    mutate(state = fips(state),
           unemp.rt = unemp.rt/100) %>%
    filter(state %in% harregs$state) %>%
    filter(year >= 1997)
### fuel prices
fuelstates <-
    tibble(state = fips(c('ME', 'NH', 'MA', 'RI', 'CT',
                          'NY', 'NJ', 'DE', 'MD',
                          'VA', 'NC', 'SC', 'GA', 'FL',
                          'FL', 'AL', 'LA', 'MS')),
           fuelregion = c(rep('new_england_regular', 5),
                          rep('central_atlantic_regular', 4),
                          rep('lower_atlantic_regular', 5),
                          rep('gulf_coast_regular', 4)),
           region = c(rep('Atlantic', 14),
                      rep('GoM', 4)))

fp <-
    pivot_longer(read_csv("../../Socioeconomic/data.ready/fuel.csv") %>%
                 select(-c(1, fl_regular, ma_regular, ny_regular, tx_regular,
                           east_coast_regular, us_regular)),
                 !c(year, wave), names_to = 'fuelregion', values_to = 'price') %>%
    filter(year >= 1997,
           year < 2019)
fp <-
    left_join(fp, fuelstates, multiple = 'all') %>% #, relationship = 'many-to-many') %>%
    select(-fuelregion)



##### meteorological
met  <-
    bind_rows(read_csv("../Environmental/Atlantic_met23.csv") %>%
              filter(state %in% harregs$state) %>%
              mutate(year = year(as.Date(start, '%d/%m/%Y')),
                     wave = ifelse(month(start) == 1, 1,
                            ifelse(month(start) == 3, 2,
                            ifelse(month(start) == 5, 3,
                            ifelse(month(start) == 7, 4,
                            ifelse(month(start) == 9, 5, 6)))))) %>%
              select(-c(start, end)) %>%
              rename(region = coast),
              read_csv("../Environmental/Gulf_met23.csv") %>%
              filter(state %in% harregs$state) %>%
              mutate(year = year(as.Date(start, '%d/%m/%Y')),
                     wave = ifelse(month(start) == 1, 1,
                            ifelse(month(start) == 3, 2,
                            ifelse(month(start) == 5, 3,
                            ifelse(month(start) == 7, 4,
                            ifelse(month(start) == 9, 5, 6)))))) %>%
              select(-c(start, end)) %>%
              rename(region = coast) %>%
              mutate(region = 'GoM')) %>%
    filter(year >= 1997)



semerged <-
    gdp %>%
    left_join(hhi, by = c("state", "year"), multiple = 'all') %>%
    left_join(pd, by = c("state", "year"), multiple = 'all') %>%
    left_join(ma, by = c("state", "year"), multiple = 'all') %>%
    left_join(hs, by = c("state", "year"), multiple = 'all') %>%
    left_join(ue, by = c("state", "year"), multiple = 'all') %>%
    left_join(fp, by = c("state", "year"), multiple = 'all')

SeMet <-
    left_join(semerged, met)

observation <-
    left_join(harregs, SeMet) %>%
    filter(!is.na(gdp))


observation <-
    bind_rows(observation,
              left_join(harregs %>%
                        filter(region %in% c('LIS', 'NJNYB')),
                        SeMet %>%
                        filter(region == 'Atlantic') %>%
                        select(-region))) %>%
    relocate(har) %>%
    mutate(har = har/numDays)


##source("../Functions/standardize.r")
##source("../Functions/standardize_species.r")

observation  <-
    observation %>%
    group_by(species) %>%
    mutate(bag.limit = standardize(bag.limit),
           min.size = standardize(min.size),
           pr.wave = standardize(empirlogitTransform(pr.wave, min(pr.wave))),
           pr.year = standardize(empirlogitTransform(pr.year, min(pr.year))),
           gdp        = standardize(gdp),
           med.income = standardize((med.income)),
           pop.density = standardize((pop.density)),
           med.age = standardize(med.age),
           med.house.size = standardize(med.house.size),
           unemp.rt = standardize(logitTransform(unemp.rt)),
           price = standardize(price),
           precip = standardize(precip),
           temp = standardize((temp)),
           wind = standardize((wind)))

observation <- 
  observation %>% 
    select(-c(year, wave, state, region, numDays, yrdays))


