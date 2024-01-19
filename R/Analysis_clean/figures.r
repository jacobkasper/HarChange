library(tidyverse); library(rstanarm); library(broom.mixed)
library(ggplot2);library(prismatic);library(viridis)

load('out/dayspecies.RData')
rhatsimp <- data.frame(rhat)
DayMCMC <- mcmcout
load('out/nodayspecies.RData')
rhatsave <-
    bind_rows(tibble(rhatsimp),
              data.frame(rhat)
              )
mcmcsave <-
    bind_rows(tibble(DayMCMC), tibble(mcmcout) )
load('out/bagspecies.RData')
rhat <- bind_rows(rhatsave, data.frame(rhat))
colnames(rhat) <- c('species', 'rhatmin', 'rhatmax')
mcmc <- bind_rows(mcmcsave, tibble(mcmcout))


table(rhat$species)
table(mcmc$species)

##min(c(min(rhat[, 2:3]), min(rhatsimp[, 2:3]), min(NoDayRhat[, 2:3])))
##max(c(max(rhat[, 2:3]), max(rhatsimp[, 2:3]), max(NoDayRhat[, 2:3])))
table(mcmc$term)


species_estimates <-
    mcmc %>%
    filter(term != '(Intercept)',
           term != 'reciprocal_dispersion') %>%
    arrange(species) %>%
    mutate(non0 = ifelse(conf.low < 0 & conf.high < 0, 1,
                  ifelse(conf.low > 0 & conf.high > 0, 1, 0))) %>%
    mutate(term = str_replace(term, "bag.limit", 'Bag'),
           term = str_replace(term, "min.size", 'Min Size'),
           term = str_replace(term, "pr.wave", 'Pr Wave'),
           term = str_replace(term, "pr.year", 'Pr Year'),
           term = str_replace(term, "gdp", 'State GDP'),
           term = str_replace(term, "pop.density", 'Population'),
           term = str_replace(term, "med.age", 'HH Age'),
           term = str_replace(term, "med.income", 'HH Income'),
           term = str_replace(term, "med.house.size", 'HH Size'),
           term = str_replace(term, "unemp.rt", 'Unemployment'),
           term = str_replace(term, "price", 'Fuel Price'),
           term = str_replace(term, "temp", 'Temperature'),
           term = str_replace(term, "precip", 'Precipitation'),
           term = str_replace(term, "wind", 'Wind Speed'),
           term = str_replace(term, "bag.above.max", 'Bag Above Max')) %>%
    mutate(species = sub("_", " ", species)) %>%
               mutate(term = factor(term,
                                    levels =
                                        rev(c("Bag", "Min Size", "Pr Wave", "Pr Year",
                                              "Fuel Price", "HH Age", "HH Income",
                                              "HH Size", "Population", "State GDP",
                                              "Unemployment", "Precipitation", "Temperature",
                                              "Wind Speed", "Bag Above Max")))) %>%
               group_by(species) %>%
               complete(term) %>%
    ungroup() %>%
    mutate(color =
               rep(c(rep("#440154AF", 1),
                     rep("#21908CAF", 3),
                     rep("#FDE725AF", 7),
                     rep("#440154AF", 4)), 21)) %>%
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
                                 'Pseudopleuronectes americanus')) %>%
    arrange(species)

## species_estimates %>%
##     filter(term == 'Max Size') %>%
##     filter(!is.na(estimate))
## ggplot(species_estimates) +
##     geom_point(aes(x = estimate, y = term, color = non0 > 0)) +
##     facet_wrap(.~species, scales = 'free_x', ncol = 3) +
##     scale_color_manual(values = setNames(c('black', 'grey'),
##                                          c(TRUE, FALSE))) +
##     geom_errorbar(aes(x = estimate, y = term,
##                       xmin = conf.low, xmax = conf.high,
##                       color = non0 > 0)) +
##     theme(legend.position = "none",
##           axis.text.y = element_text(size = 5))
all_groups <- unique(species_estimates$species)
n_all_groups <- length(all_groups) + 3
n_col <- 3
n_row <- 4
# split the groups so that you'd have n_col*nrow groups in each split
start_idx <- seq(1, n_all_groups, n_col*n_row)
group_splits <- lapply(start_idx,
       function(i){
           all_groups[i:(i+ n_col*n_row -1)]
})
group_splits[[2]][10] <- 'x'
group_splits[[2]][11] <- 'y'
group_splits[[2]][12] <- 'z'
## species_estimates[1:16, c(2, 7)]
table(species_estimates$term)

species_estimates <-
    bind_rows(species_estimates,
              data.frame(species = c(rep('x', 15),
                                     rep('y', 15),
                                     rep('z', 15)),
                     term =
                         rep(c("Bag Above Max",
                               "Wind Speed", "Temperature",
                               "Precipitation",
                               "Unemployment", "State GDP",
                               "Population",
                               "HH Size", "HH Income",
                               "HH Age", "Fuel Price",
                               "Pr Wave",
                               "Pr Year",
                               "Min Size", "Bag"), 3),
           estimate = 0,
           conf.low = 0,
           conf.high = 0,
           non0 = 0,
           color =
               rep(c(rep("#440154AF", 1),
                     rep("#21908CAF", 3),
                     rep("#FDE725AF", 7),
                     rep("#440154AF", 4)), 3)
           )
           ) %>%
    mutate(term = factor(term,
                         levels =
    rev(c("Bag", "Min Size", "Pr Wave", "Pr Year",
          "Fuel Price", "HH Age", "HH Income",
          "HH Size", "Population", "State GDP",
          "Unemployment", "Precipitation", "Temperature",
          "Wind Speed", "Bag Above Max")))) %>%
    mutate(non0 = ifelse( non0 > 0, TRUE, FALSE))
##dev.new()
##barplot(GNP ~ Year, data = longley, col = c("#440154AF", "#21908CAF", "#FDE725AF"))

rects <- as_tibble(data.frame(ystart = seq(0.5,14.5,1),
                    yend   = seq(1.5,15.5,1),
                    term =     rev(c("Bag", "Min Size", "Pr Wave", "Pr Year",
                                     "Fuel Price", "HH Age", "HH Income",
                                     "HH Size", "Population", "State GDP",
                                     "Unemployment", "Precipitation", "Temperature",
                                     "Wind Speed", "Bag Above Max")),
                   term2 =     rev(c("Bag (+)", "Min Size (-)", "Pr Wave (-)", "Pr Year (+)",
                                     "Fuel Price (-)", "HH Age (NA)", "HH Income (+)",
                                     "HH Size (-)", "Population (-)", "State GDP (-)",
                                     "Unemployment (-)", "Precipitation (+)", "Temperature (+)",
                                     "Wind Speed (-)", "Bag Above Max (+)"))))

species_estimates2 <-
    left_join(species_estimates, rects) %>%
    select(-term) %>%
    relocate(term2, .after = species) %>%
    rename(term = term2) %>%
    group_by(species) %>%
    ##mutate(min = min(conf.low, na.rm = TRUE)) %>%
    ##mutate(max = max(conf.high, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(term = factor(term,
                         levels =
                               rev(c("Bag (+)", "Min Size (-)", "Pr Wave (-)", "Pr Year (+)",
                                     "Fuel Price (-)", "HH Age (NA)", "HH Income (+)",
                                     "HH Size (-)", "Population (-)", "State GDP (-)",
                                     "Unemployment (-)", "Precipitation (+)", "Temperature (+)",
                                     "Wind Speed (-)", "Bag Above Max (+)"))))  %>%
    mutate(z = rep(c(rep('reg', 1),
                     rep('env', 3),
                     rep('se',  7),
                     rep('reg', 4)), 24))


i <- 0
for(groups in group_splits){
    i <- i + 1
    sp <- species_estimates2 %>%
        filter(species %in% groups)
    p <-
    sp %>% ggplot(aes(fill = color)) +
    geom_point(aes(x = estimate, y = term, colour = non0)) +
    scale_colour_manual(values = setNames(c('black', 'grey70'),
                                          c(TRUE, FALSE))) +
    facet_wrap(.~species, scales = 'free_x', ncol = 3, nrow = 4)  +
    geom_linerange(aes(x = estimate, y = term,
                       xmin = conf.low, xmax = conf.high,
                       color = non0)) +
    ## geom_rect(aes(ymin = ystart,
    ##               ymax = yend,
    ##               xmin = -Inf,
    ##               xmax = Inf,
    ##           fill = stage(start = z,
    ##                        after_scale = clr_lighten(fill, space = 'combined')))) +
         ## fill = stage(start = z,
        ##                        after_scale =
        ##                        clr_lighten(fill, space = 'combined')))) +
        geom_vline(xintercept = 0, linetype = "dotted",
                   color = "lightgrey") +
        geom_point(aes(x = estimate, y = term, colour = non0)) +
        geom_linerange(aes(x = estimate, y = term,
                           xmin = conf.low, xmax = conf.high,
                           color = non0)) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.y = element_text(size = 6),
              axis.title.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.text = element_text(face = "italic"),
              strip.background = element_rect(fill = "white"),
              axis.line = element_line(colour = "black")) +
        #coord_cartesian(expand = FALSE) +
        labs(x = "Estimate")
    ggsave(plot = p, filename = paste0('Figures/Figure_4', i, '.tiff'),
           width = 210, height = 200, units = "mm")#, dpi = 300)
}


out.table <-
    species_estimates %>%
    filter(!species %in% c('x', 'y', 'z')) %>%
    mutate(direction = ifelse(conf.low < 0 & conf.high < 0, 'Negative',
                       ifelse(conf.low > 0 & conf.high > 0, 'Positive',
                              'No effect'))) %>%
    mutate(direction = ifelse(is.na(direction), 'NA', direction)) %>%
    select(-c('estimate', 'conf.low', 'conf.high', 'species')) %>%
    group_by(term, color, non0) %>%
    count(direction) %>%
    mutate(direction = factor(direction,
                              levels =
                                  c("Negative", "No effect",
                                    "Positive", 'NA')))
rects2 <- as_tibble(data.frame(ystart = seq(0.5,14.5,1),
                    yend   = seq(1.5,15.5,1),
                    term =     rev(c("Bag", "Min Size", "Pr Wave", "Pr Year",
                                     "Fuel Price", "HH Age", "HH Income",
                                     "HH Size", "Population", "State GDP",
                                     "Unemployment", "Precipitation", "Temperature",
                                     "Wind Speed", "Bag Above Max")),
                    term2 =     rev(c("Bag (+)", "Min Size (-)", "Pr Wave (-)", "Pr Year (+)",
                                     "Fuel Price (-)", "HH Age (NA)", "HH Income (+)",
                                     "HH Size (-)", "Population (-)", "State GDP (-)",
                                     "Unemployment (-)", "Precipitation (+)", "Temperature (+)",
                                     "Wind Speed (-)", "Bag Above Max (+)")),
                    direction =
                              rep(c('NA', 'No effect', 'Positive', 'Negative'), each = 15)))

out.table2 <-
    full_join(out.table, rects2)  %>%
    ungroup() %>%
    select(-term) %>%
    relocate(term2) %>%
    rename(term = term2) %>%
    group_by(term) %>%
    tidyr::fill(color, .direction = 'downup') %>%
    ungroup() %>%
    mutate(direction = factor(direction,
                              levels =
                                  rev(c("Positive",
                                    "Negative", "No effect", 'NA')))) %>%
     mutate(term = factor(term,
                          levels =
                              rev(c("Bag (+)", "Min Size (-)", "Pr Wave (-)", "Pr Year (+)",
                                     "Fuel Price (-)", "HH Age (NA)", "HH Income (+)",
                                     "HH Size (-)", "Population (-)", "State GDP (-)",
                                     "Unemployment (-)", "Precipitation (+)", "Temperature (+)",
                                     "Wind Speed (-)", "Bag Above Max (+)"))))



#pdf("Figure_6_effects.pdf", width = 6, height = 5, paper = "letter")
ggplot(out.table2, aes(x = n, y = term, fill = direction)) +
    geom_bar(position = 'stack', stat = 'identity') +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 3) + # Add text labels
      scale_x_continuous(breaks = seq(0, 20, by = 5)) +
    theme_bw() +
    scale_fill_viridis_d()+
    coord_cartesian(expand = FALSE) +
    labs(x = 'Number of  species',
         fill = 'Effect') +
    theme(axis.title.y = element_blank(),
                                        #panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black")) +
    theme(panel.spacing = unit(0.6, "lines"))  +
  guides(fill = guide_legend(reverse = TRUE))
#dev.off()
ggsave('Figures/Figure_5.pdf',  width = 6, height = 5, dpi = 700)




species_estimates %>%
    filter(!species %in% c('x', 'y', 'z')) %>%
    filter(term == 'Max Size') %>%
    filter(!is.na(estimate) )

species_estimates %>%
    filter(!species %in% c('x', 'y', 'z')) %>%
    filter(term == 'Bag Above Max Size') %>%
    filter(!is.na(estimate) )

write.csv(out.table, 'EffectsSummary.csv', row.names = FALSE)


viridis(3)














