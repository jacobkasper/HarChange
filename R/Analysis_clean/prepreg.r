library(tidyverse); library(readxl); library(cdlTools);library(purrr)


# Set the path to the directory containing your CSV files
directory_path <- "../../Fishery/Harvest/"

# Get a list of all CSV files in the directory
csv_files <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE) %>%
    setdiff(list.files(directory_path, pattern = "_kg_", full.names = TRUE))
tog <- csv_files[grep("Tautog_LIS", csv_files, ignore.case = TRUE)]
csv_files <- csv_files[grep("_n_", csv_files, ignore.case = TRUE)]
fl_files <- csv_files[grep("_fl_", csv_files, ignore.case = TRUE)]
# Find indices of elements with "fl" in the name
exclude_indices <- grep("_fl_", csv_files, ignore.case = TRUE)
# Remove elements with "fl" from the vector
csv_files <- csv_files[-exclude_indices]
exclude_indices <- grep("red_porgy_catch_gom_n_series.csv", csv_files, ignore.case = TRUE)
csv_files <- csv_files[-exclude_indices]
exclude_indices <- grep("bluefish_catch_gom_n_series.csv", csv_files, ignore.case = TRUE)
csv_files <- csv_files[-exclude_indices]

##csv_files <- c(filtered_vector, tog)
##names(csv_data_list$tautog_lis.csv) <- str_to_title(names(csv_data_list$tautog_lis.csv))
##save tautog LIS for later



# Read all CSV files into a list
csv_data_list <- map(csv_files, read_csv)
fl_data_list <- map(fl_files, read_csv)
##perp all except those two fl files
listnames <- sub("../../Fishery/Harvest/mrip_", "", csv_files)
listnames <- sub("../../Fishery/Harvest/", "", listnames)
names(csv_data_list) <- tolower(listnames)
updatewave <- function(df){
    df %>%
        mutate(Wave = ifelse(Wave == "JANUARY/FEBRUARY",  1,
                      ifelse(Wave == "MARCH/APRIL",       2,
                      ifelse(Wave == "MAY/JUNE",          3,
                      ifelse(Wave == "JULY/AUGUST",       4,
                      ifelse(Wave == "SEPTEMBER/OCTOBER", 5, 6))))))
}

csv_data_list <- lapply(csv_data_list, updatewave)
har <- lapply(csv_data_list, function(df) {
  colnames(df) <- make.names(colnames(df))
  return(df) })
har <- lapply(har, function(x){x %>% select(- c(Estimate.Status, PSE)) %>%
                            rename(har = Total.Harvest..A.B1.)})
har <- lapply(har, function(x){x %>% mutate(State = fips(State))})
new_column_vector <- c(rep('Atlantic', 5), 'GoM',
                       rep('Atlantic', 2), 'GoM',
                       'Atlantic', 'GoM',
                       'Atlantic', 'GoM',
                       rep('Atlantic', 2), 'GoM',
                       rep('Atlantic', 2), 'GoM',
                       'Atlantic', 'GoM',
                       'Atlantic', 'GoM',
                       'Atlantic', 'GoM',
                       rep('Atlantic', 5))
# Function to add a new column to each data frame
add_new_column <- function(df, new_col) {
  cbind(df, NewColumn = new_col)
}
# Use Map to add the new column to each data frame in the list
har <- Map(add_new_column, har, new_col = new_column_vector)
har <- lapply(har, function(x) {tibble(x) %>%
                             rename(region = NewColumn)})
har <- do.call(rbind, har)
har <-
    har %>%
    filter(Year >= 1997) %>%
    rename(species = Common.Name) %>%
    mutate(species = tolower(species))
fl_data_list <- map(fl_files, read_csv)

##perp  those two fl files
listnames <- sub("../../Fishery/Harvest/mrip_", "", fl_files)
listnames <- sub("../../Fishery/Harvest/", "", listnames)
names(fl_data_list) <- tolower(listnames)
fl_data_list <- lapply(fl_data_list, updatewave)
flhar <- lapply(fl_data_list, function(df) {
    colnames(df) <- make.names(colnames(df))
    return(df) })
flhar <- lapply(flhar, function(x){x %>%
                                       select(-c(Estimate.Status, PSE)) %>%
                                       rename(har = 4) %>%
                                       mutate(State = 12) %>%
                                       mutate(region = 'Atlantic')})
flhar <- do.call(rbind, flhar)
flhar <-
    flhar %>%
    filter(Year >= 1997) %>%
    rename(species = Common.Name) %>%
    mutate(species = tolower(species)) %>%
    relocate(State, .after = Wave)
flhar <-
    flhar %>%
    mutate(unique = paste0(Year, Wave, species, State, region))

har <-
    bind_rows(
    har %>%
          mutate(har = as.numeric(har),
          unique = paste0(Year, Wave, species, State, region)) %>%
          filter(!unique %in% flhar$unique),
          flhar %>%
          mutate(unique = paste0(Year, Wave, species, State, region)))
tog <-
    read_csv(tog) %>%
    filter(state == 36)

har <-
    bind_rows(
        har %>%
        mutate(har = as.numeric(har)) %>%
        filter(!is.na(har)) %>%
        filter(!species == 'tautog' | !State == 36),
        tog %>%
        select(-harvest.lb) %>%
        mutate(species = 'tautog') %>%
        rename(Year = year, Wave = wave, State = state,
               har = harvest.n, region = area) %>%
        relocate(Year, Wave, State, species, har, region))
har <-
    har %>%
    rename(year = Year, wave = Wave, state = State) %>%
    mutate(unique = paste0(year, wave, state, species, region)) %>%
    filter(year >= 1997,
           year < 2019)

## read regs
directory_path <- "../../Fishery/Regs_by_Species/Regs"
# Get a list of all CSV files in the directory
regs <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE)
listnames <- sub("../../Fishery/Regs_by_Species/Regs/", "", regs)
listnames <- sub(".csv", "", listnames)

##problems(read_csv("../../Fishery/Regs_by_Species/Regs/Red_Drum_regs.csv") )
##  mutate(area = as.character(area)))

regs <- map(regs, read_csv)
listnames <- sub('_regs', '', listnames)
listnames <- sub('_Regs', '', listnames)
listnames <- sub('_', ' ', listnames)
listnames <- sub('BSB', 'Black Sea Bass', listnames)
listnames <- tolower(listnames)
names(regs) <- listnames
regsbu <- regs

regs <- regsbu
regs <- bind_rows(lapply(regs,
                         function(x){x %>%
                                         mutate_all(~replace(., . == 9999, NA)) %>%
                                         setNames(tolower(names(.))) %>%
                                         mutate(bag.limit = ifelse(is.na(bag.limit), poss.limit, bag.limit),
                                                bag.limit = ifelse(is.na(bag.limit), agg.bag.limit, bag.limit)) %>%
                                         select(-c(agg.bag.limit, juristiction, depth.allowed,
                                                   daily.closures, imputed, poss.limit, boat.limit,
                                                   other, comments, gear.restriction, id)) %>%
                                         mutate(area = as.character(area))})) %>%
    mutate(species = tolower(sub('_', ' ', species))) %>%
    mutate(species = tolower(sub('_', ' ', species))) %>%
    select(-defaults.fed) %>%
    filter(!state %in% c('EEZ', 'PFRC', 'PRFC', 'DC', 'PA', 'TX'))  %>%
    mutate(region = ifelse(state %in% c('CT', 'DE', 'FLE', 'GA', 'MA', 'MD', 'ME', 'NC', 'NH', 'NJ', 'NY',
                                        'RI', 'SC', 'VA'), 'Atlantic',
                    ifelse(state %in% c('AL', 'FLW', 'LA', 'MS'), 'GoM', 'FLcoasts')),
           area = ifelse(state %in% c('AL', 'FLW', 'LA', 'MS', 'TX'), 'GoM', area),
           area = ifelse(area == 'FLAO', 'Atlantic', area),
           area = ifelse(area == 'Gulf_of_mexico', 'GoM', area),
           area = ifelse(area == 'Gulf_of_Mexico', 'GoM', area),
           area = ifelse(area == 'Gulf_of_mexico_except_monroe', 'GoM', area),
           area = ifelse(area == 'GMX.MO', 'GoM', area),
           area = ifelse(area == 'GMX', 'GoM', area),
           state = ifelse(state %in% c('FLE', 'FLW'), 'FL', state),
           area = ifelse(species == 'black sea bass', 'Atlantic', area),
           area = ifelse(species == 'atlantic croaker', 'Atlantic', area),
           area = ifelse(species == 'weakfish', 'Atlantic', area)) %>%
    mutate(state = fips(state)) %>%
    filter(year >= 1997) %>%
    filter(year < 2019) %>%
    mutate(region = ifelse(area %in% c('FNER', 'FSER', 'Atlantic') &
                           state == 12, 'Atlantic', region),
           region = ifelse(area %in% c('FNWR', 'FNW', 'FSWR', 'GoM') &
                           state == 12, 'GoM', region),
           region = ifelse(area %in% c('FNR', 'FSR') &
                           state == 12, 'FLcoasts', region),
           area =   ifelse(area %in% c('FNF', 'FNR',  'FSR') &
                           state == 12, 'FLcoasts', area),
           area =   ifelse(region == 'FLcoasts' &
                           state == 12, 'FLcoasts', area)) %>%
    mutate(area = ifelse(is.na(area) & region == 'Atlantic', 'Atlantic', area)) %>%
    mutate(region = ifelse(area == 'LIS', 'LIS', region)) %>%
    mutate(mode = ifelse(is.na(mode), 0, mode))


regs <- ##repeat fl regs which are not coast-specific
    bind_rows(regs %>%
    mutate(region = ifelse(state == 12 & region == 'FLcoasts', 'GoM', region)),
        regs %>%
        filter(state == 12 & region == 'FLcoasts') %>%
        mutate(region = 'Atlantic'))
##fix tautog in NY
regs <- bind_rows(
        regs %>%
        mutate(region = ifelse(state == 36 & species == 'tautog' & year != 2018, 'LIS', region)),
        regs %>%
        filter(state == 36 & species == 'tautog' & year != 2018) %>%
        mutate(region = 'NJNYB'))



##remove regs for which no harvest records
regs <-
    regs %>%
    mutate(unique = paste0(year, wave, state, species, region)) %>%
    filter(unique %in% har$unique) %>%
    select(-unique)

regdays <- ##calculate the number of days in each year/wave/state/species/region that regulations is open
    regs %>%
    filter(bag.limit > 0 | is.na(bag.limit))  %>%
    group_by(year, wave, state, species, region, rn = row_number()) %>%
    do(data.frame(.,Date = seq(as.Date(dmy(.$start.date), "%d-%m-%Y"),
                               as.Date(dmy(.$end.date), "%d-%m-%Y"),
                               '1 day'))) %>%
    group_by(year, wave, state, species, region) %>%
    summarize(numDays = n_distinct(Date))



########### aggregated bag limits for Atlantic Croaker
table(regs$agg.bag.limit.wt, regs$state, useNA = 'always')

acrfl <-
    read_csv("../../Fishery/Harvest/mrip_ATLANTIC_CROAKER_catch_series.csv") %>%
    setNames(tolower(names(.))) %>%
    mutate(state = fips(state)) %>%
    rename(Wave = wave, harvest.kg = `harvest (a+b1) total weight (kg)`) %>%
    select(-c('estimate status', pse, 'common name', `landings (no.) without size information`)) %>%
    filter(state == 12,
           harvest.kg > 0)
acrfl <-
    inner_join(updatewave(acrfl) %>%
    setNames(tolower(names(.))),
    har %>%
    setNames(tolower(names(.))) %>%
    filter(species  == 'atlantic croaker',
           state == 12,
           har > 0)) %>%
    mutate(harvest.lb = harvest.kg * 2.20462,
           mean.fish.wt = harvest.lb/har) %>%
    mutate(mean.fish.wt = mean(mean.fish.wt)) %>%
    select(-harvest.kg)
acrfl <-
    inner_join(regs %>%
    filter(species == 'atlantic croaker',
           state == 12,
           agg.bag.limit.wt == 100),
    acrfl) %>%
        ungroup() %>%
        select(year, wave, state, species, agg.bag.limit.wt, mean.fish.wt) %>%
    mutate(bag.limit = agg.bag.limit.wt/mean.fish.wt) %>%
    select(agg.bag.limit.wt, bag.limit) %>%
    distinct()


acdrfl <-
    read_csv("../../Fishery/Harvest/mrip_ATLANTIC_COD_catch_series.csv") %>%
    setNames(tolower(names(.))) %>%
    mutate(state = fips(state)) %>%
    rename(Wave = wave, harvest.kg = `harvest (a+b1) total weight (kg)`) %>%
    select(-c('estimate status', pse, 'common name', `landings (no.) without size information`)) %>%
    filter(state == 25,
           harvest.kg > 0)
acdrfl <-
    inner_join(updatewave(acdrfl) %>%
    setNames(tolower(names(.))),
    har %>%
    setNames(tolower(names(.))) %>%
    filter(species  == 'atlantic cod',
           state == 25,
           har > 0)) %>%
    mutate(harvest.lb = harvest.kg * 2.20462,
           mean.fish.wt = harvest.lb/har) %>%
    mutate(mean.fish.wt = mean(mean.fish.wt)) %>%
    select(-harvest.kg)
acdrfl <-
    inner_join(regs %>%
    filter(species == 'atlantic cod',
           state == 25,
           agg.bag.limit.wt == 75),
    acdrfl) %>%
        ungroup() %>%
        select(year, wave, state, species, agg.bag.limit.wt, mean.fish.wt) %>%
    mutate(bag.limit = agg.bag.limit.wt/mean.fish.wt) %>%
    select(agg.bag.limit.wt, bag.limit) %>%
    distinct()

regs <-
    regs %>%
    mutate(agg.bag.limit.wt = ifelse(is.na(agg.bag.limit.wt), 0, agg.bag.limit.wt))


regs <-
    bind_rows(
        bind_rows(
        regs %>%
        filter(!agg.bag.limit.wt %in% c(75, 100)),
        regs %>%
        filter(agg.bag.limit.wt == 100) %>%
        mutate(bag.limit = acrfl$bag.limit),
        regs %>%
        filter(agg.bag.limit.wt == 75) %>%
        mutate(bag.limit = acdrfl$bag.limit))) %>%
    select(-agg.bag.limit.wt)

##### atlantic croaker and atlantic cod bag limit by weight done

######### change fl to tl
smtl <-
    regs %>%
    filter(species == 'spanish mackerel',
           tl.fl == 2) %>%
    mutate(min.size.mm = min.size * 25.4) %>%
    mutate(min.size = round((21.599 + 1.123 * min.size.mm) / 25.4, 1)) %>%
    select(-min.size.mm)
shtl <-
    regs %>%
    filter(species == 'sheepshead',
           tl.fl == 2) %>%
        mutate(min.size.mm = min.size * 25.4)  %>%
    mutate(min.size = 280/25.4) %>%
    select(-min.size.mm)
####round((2.1416 + 0.9038 * 280) / 25.4, 1)
##formula was backwards to fixed it
kmtl <-
    regs %>%
    filter(species == 'king mackerel',
           tl.fl == 2) %>%
    mutate(min.size.mm = min.size * 25.4) %>%
    mutate(min.size = round((48.968 + 1.067 * min.size.mm) / 25.4, 1)) %>%
    select(-min.size.mm)
regs <-
    bind_rows(smtl, shtl, kmtl, regs %>%
                            filter(is.na(tl.fl) | tl.fl != 2)) %>%
    select(-tl.fl)


#####replace missing bag limits with 2x max bag limit
yr.wv <- read_csv('../aux_data/yr_wv_date.csv') %>%
    filter(year >= 1997) %>%
    select(-c(start.date, end.date, ndays))

str(regs)



regs <-
    left_join(
        regs %>%
        mutate(bag.above.max = ifelse(is.na(bag.above.max), 0, bag.above.max),
               region = as.character(region),
               state = as.numeric(state),
               start.date = as.Date(dmy(.$start.date), "%d-%m-%Y"),
               end.date   = as.Date(dmy(.$end.date),   '%d/%m/%Y'),
               reg.days   = as.numeric(difftime(end.date, start.date,
                                                units = "days")) + 1) %>%
        ungroup() %>%
        filter(bag.limit > 0 | is.na(bag.limit))  %>%
        select(-c(min.size2, max.size, max.size2, bag.below.min,
                  bag.above.max2, #bag.above.max,
                  area, bonus.limit, annual.bonus)) %>%
        group_by(year, wave, state, species, region) %>%
        mutate(bag.limit = ifelse(is.na(bag.limit),
                                  2 * max(bag.limit, na.rm = TRUE), bag.limit),
               bag.limit = ifelse(is.infinite(bag.limit), NA, bag.limit),
               min.size  = sum(min.size*reg.days, na.rm = TRUE)/sum(reg.days),
               bag.limit = sum(bag.limit*reg.days, na.rm = TRUE)/sum(reg.days),
               bag.limit = ifelse(bag.limit == 0, NA, bag.limit),
               bag.above.max = sum(bag.above.max*reg.days, na.rm = TRUE)/sum(reg.days),
               bag.above.max = ifelse(bag.above.max == 0, NA, bag.above.max))        %>%
        group_by(species, state) %>%
        mutate(bag.limit = ifelse(is.na(bag.limit),
                                  2 * max(bag.limit, na.rm = TRUE), bag.limit),
               bag.limit = ifelse(is.infinite(bag.limit), NA, bag.limit)) %>%
        group_by(species) %>%
        mutate(bag.limit = ifelse(is.na(bag.limit),
                                  2 * max(bag.limit, na.rm = TRUE), bag.limit)) %>%
        select(-c(start.date, end.date, mode)) %>%
        distinct() %>%
        group_by(year, wave, state, species, region) %>%
        distinct(), yr.wv) %>%
    ungroup()



regs <-
    left_join(regs, regdays) %>%
    select(-c(reg.days)) %>%
    distinct() %>%
    mutate(pr.wave = numDays/wavelength)

regs <-
  left_join(regs, yr.wv %>%
              group_by(year) %>%
              summarize(yrdays = sum(wavelength)) ) %>%
  select(-wavelength) %>%
  group_by(year, state, species, region) %>%
  mutate(pr.year = sum(numDays)/yrdays)

harregs <-
    left_join(regs, har %>%
                    select(-unique))

table(regdays$species)
