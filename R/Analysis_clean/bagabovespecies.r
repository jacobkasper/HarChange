ls()
library(tidyverse); library(rstanarm); library(broom.mixed)

source("../Functions/standardize.r")
source("../Functions/standardize_species.r")
source('merge_regs.r')


##no pr.daysLS for these species
##c(Atlantic_Croaker, King_Mackerel, Red_Drum, Sheepshead,
##Spanish_Mackerel, Weakfish)
## dropped Atlantic_Croaker, Bluefish, King_Mackerel, Sheepshead,
## Spanish_Mackerel, Weakfish
## Red_Grouper, Red_Drum, Spotted_Seatrout, Striped_Bass



bagReg <-
    observation %>%
    filter(species %in% c('bluefish', 'striped bass', 'spotted seatrout')) %>%
    arrange(species) %>%
    mutate(bag.above.max = ifelse(is.na(bag.above.max), 0, bag.above.max))

rhat <- NULL
mcmcout  <- NULL
mu_prior <- 100
for(s in 1:length(unique(bagReg$species))){
    rhattmp <- NULL
    mcmctmp <- NULL
    soi <-
        bagReg %>%
        filter(species == unique(bagReg$species)[s]) %>%
        ungroup() %>%
        select(-c(species)) %>%
        mutate(har = round(har))
    D  <- dim(soi)[2]-1
    p0 <- floor(D/2)
    tau0 <- p0 / (D - p0) / sqrt(mu_prior) / sqrt(dim(soi)[1])
    assign(paste0('out', unique(bagReg$species)[s]),
           stan_glm(
               formula = round(har) ~
                   bag.limit + min.size + pr.wave +
                       pr.year + bag.above.max +
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
    pdf(file = paste0('postclean/', unique(bagReg$species)[s], '.pdf'))
    print(pp_check(get(paste0('out', unique(bagReg$species)[s]))))
    dev.off()
    rhattmp <- cbind(unique(bagReg$species)[s],
               range(summary(get(paste0('out',
                             unique(bagReg$species)[s])))[, "Rhat"])[1],
               range(summary(get(paste0('out',
                             unique(bagReg$species)[s])))[, "Rhat"])[2])
    rhat <- rbind(rhat, rhattmp)
    mcmctmp <- cbind(species = unique(bagReg$species)[s],
                     as.data.frame(tidyMCMC(get(paste0('out',
                                   unique(bagReg$species)[s])),
                                   conf.int = TRUE,
                                   conf.method = "quantile"))[c(1:2, 4:5)])
    mcmcout <- rbind(mcmcout, mcmctmp)
}


rdbagReg <-
    observation %>%
    filter(species == 'red drum') %>%
    mutate(bag.above.max = ifelse(is.na(bag.above.max), 0, bag.above.max))
rhattmp <- NULL
mcmctmp <- NULL
soi <-
    rdbagReg %>%
    ungroup() %>%
    select(-c(species, pr.wave)) %>%
    mutate(har = round(har))
D  <- dim(soi)[2]-1
p0 <- floor(D/2)
tau0 <- p0 / (D - p0) / sqrt(mu_prior) / sqrt(dim(soi)[1])
assign(paste0('out', 'red drum'),
       stan_glm(
           formula = round(har) ~
               bag.limit + min.size + pr.year + bag.above.max +
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
pdf(file = paste0('postclean/', 'red drum', '.pdf'))
print(pp_check(get(paste0('out', 'red drum'))))
dev.off()
rhattmp <- cbind('red drum',
                 range(summary(get(paste0('out',
                                          'red drum')))[, "Rhat"])[1],
                 range(summary(get(paste0('out',
                                          'red drum')))[, "Rhat"])[2])
rhat <- rbind(rhat, rhattmp)
mcmctmp <- cbind(species = 'red drum',
                 as.data.frame(tidyMCMC(get(paste0('out',
                                                   'red drum')),
                                        conf.int = TRUE,
                                        conf.method = "quantile"))[c(1:2, 4:5)])
mcmcout <- rbind(mcmcout, mcmctmp)



save.image(file = 'out/bagspecies.RData')
