library(tidyverse)#; library(rstanarm); library(broom.mixed)

source("../Functions/standardize.r")
source("../Functions/standardize_species.r")
source('merge_regs.r')


##no pr.daysLS for these species
##c(Atlantic_Croaker, King_Mackerel, Red_Drum, Sheepshead,
##Spanish_Mackerel, Weakfish)
## dropped Atlantic_Croaker, Bluefish, King_Mackerel, Sheepshead,
## Spanish_Mackerel, Weakfish
## Red_Grouper, Red_Drum, Spotted_Seatrout, Striped_Bass
table(harregs$year)
observation %>%
    ungroup() %>%
    select(-c(numDays, yrdays), species, year, wave, state, pr.wave) %>%
    print(width = Inf)

summary(observation)
table(observation$species)#, observation$pr.wave, useNA = 'always')

obs <-
    observation %>%
        mutate(species = str_replace(species, 'atlantic cod', 'Gadus morhua'),
           species = str_replace(species, 'atlantic croaker',
                                 'Micropogonias undulatus'),
           species = str_replace(species, 'black sea bass',
                                 'Centropristis striata'),
           species = str_replace(species, 'bluefish',
                                 'Pomatomus saltatrix'),
           species = str_replace(species, 'greater amberjack',
                                 'Seriola dumerili'),
           species = str_replace(species, 'haddock',
                                 'Melanogrammus aeglefinus'),
           species = str_replace(species, 'king mackerel',
                                 'Scomberomorus cavalla'),
           species = str_replace(species, 'red drum',
                                 'Sciaenops ocellatus'),
           species = str_replace(species, 'red grouper',
                                 'Epinephelus morio'),
           species = str_replace(species, 'red porgy', 'Pagrus pagrus'),
           species = str_replace(species, 'red snapper',
                                 'Lutjanus campechanus'),
           species = str_replace(species, 'scup', 'Stenotomus chrysops'),
           species = str_replace(species, 'sheepshead',
                                 'Archosargus probatocephalus'),
           species = str_replace(species, 'southern flounder',
                                 'Paralichthys lethostigma'),
           species = str_replace(species, 'spanish mackerel',
                                 'Scomberomorus maculatus'),
           species = str_replace(species, 'spotted seatrout',
                                 'Cynoscion nebulosus'),
           species = str_replace(species, 'striped bass',
                                 'Morone saxatilis'),
           species = str_replace(species, 'summer flounder',
                                 'Paralichthys dentatus'),
           species = str_replace(species, 'tautog', 'Tautoga onitis'),
           species = str_replace(species, 'weakfish', 'Cynoscion regalis'),
           species = str_replace(species, 'winter flounder',
                                 'Pseudopleuronectes americanus'))

table(obs$species, useNA = 'always')
table(harregs$species, useNA = 'always')
obs
harregs
table(fips(harregs$state, 'name'), harregs$species, useNA = 'always')

 NoSpDayReg <-
    observation %>%
    filter(species %in% c('atlantic croaker', 'king mackerel',
                           'sheepshead', 'southern flounder', 'weakfish')) %>%
    arrange(species)

rhat <- NULL
mcmcout  <- NULL
mu_prior <- 100
for(s in 1:length(unique(NoSpDayReg$species))){
    rhattmp <- NULL
    mcmctmp <- NULL
    soi <-
        NoSpDayReg %>%
        filter(species == unique(NoSpDayReg$species)[s]) %>%
        ungroup() %>%
        select(-c(species, year, state)) %>%
        mutate(har = round(har))
    D  <- dim(soi)[2]-1
    p0 <- floor(D/2)
    tau0 <- p0 / (D - p0) / sqrt(mu_prior) / sqrt(dim(soi)[1])
    assign(paste0('out', unique(NoSpDayReg$species)[s]),
           stan_glm(
               formula = round(har) ~
                   bag.limit + min.size + pr.year +
                       gdp + med.income + pop.density + med.age +
                       med.house.size + unemp.rt + price +
                        precip + temp + wind,
               family = neg_binomial_2(),
               data = soi,
               prior = hs(global_scale = tau0, slab_df = 100,
                          slab_scale = 1),
               seed = 7304, QR = TRUE,
               iter = 15000,
               adapt_delta = 0.99
           ))
    pdf(file = paste0('postclean/', unique(NoSpDayReg$species)[s], '.pdf'))
    print(pp_check(get(paste0('out', unique(NoSpDayReg$species)[s]))))
    dev.off()
    rhattmp <- cbind(unique(NoSpDayReg$species)[s],
               range(summary(get(paste0('out',
                             unique(NoSpDayReg$species)[s])))[, "Rhat"])[1],
               range(summary(get(paste0('out',
                             unique(NoSpDayReg$species)[s])))[, "Rhat"])[2])
    rhat <- rbind(rhat, rhattmp)
    mcmctmp <- cbind(species = unique(NoSpDayReg$species)[s],
                     as.data.frame(tidyMCMC(get(paste0('out',
                                   unique(NoSpDayReg$species)[s])),
                                   conf.int = TRUE,
                                   conf.method = "quantile"))[c(1:2, 4:5)])
    mcmcout <- rbind(mcmcout, mcmctmp)
}



save.image(file = 'out/nodayspecies.RData')
