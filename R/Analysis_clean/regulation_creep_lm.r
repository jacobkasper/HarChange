rm(list = ls())

library(tidyverse)
library(ggpubr)
##https://stackoverflow.com/questions/51735481/ggplot2-change-axis-limits-for-each-individual-facet-panel
scale_inidividual_facet_y_axes = function(plot, ylims) {
  init_scales_orig = plot$facet$init_scales
  init_scales_new = function(...) {
    r = init_scales_orig(...)
    ## Extract the Y Scale Limits
    y = r$y
    ## If this is not the y axis, then return the original values
    if(is.null(y)) return(r)
    ##If these are the y axis limits, then we iterate over them,
    ##replacing them as specified by our ylims parameter
    for (i in seq(1, length(y))) {
      ylim = ylims[[i]]
      if(!is.null(ylim)) {
        y[[i]]$limits = ylim
      }
    }
    # Now we reattach the modified Y axis limit list to the original return object
    r$y = y
    return(r)
  }
  plot$facet$init_scales = init_scales_new
  return(plot)
}



source('prepreg.r')

regs <-
    harregs %>%
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



##library(ggpubr)

names(regs)
table(regs$numDays)
table(days$yrdays)

library(broom)

as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, region, numDays) %>%
    group_by(year, state, species, region) %>%
    dplyr::mutate(yrdays = sum(numDays)) %>%
    group_by(species) %>%
    do(tidy(lm(yrdays~year,  .))) %>%
    filter(term  == 'year',
           p.value < 0.05)


m1 <- lm(yrdays ~ year,  data =  tmp)
coef(m1)
summary(m1)


days1 <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, region, numDays) %>%
    group_by(year, state, species, region) %>%
    dplyr::mutate(yrdays = sum(numDays)) %>%
    ggplot(aes(x = year, y = yrdays))+
    geom_point(col = 'grey', alpha = 0.4) +
    geom_smooth(method = "lm") +
    facet_wrap(~species, ##scales = "free_y",
               ncol = 3) +
    theme_bw() +
    stat_regline_equation() +
    xlab('Year') +
    ylab('Annual mean season length (days)') +
    xlim(1997, 2018) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))


library(ggpmisc)




days1 <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, region, numDays) %>%
    group_by(year, state, species, region) %>%
    dplyr::mutate(yrdays = sum(numDays)) %>%
    ggplot(aes(x = year, y = yrdays))+
    geom_point(col = 'grey', alpha = 0.4) +
    geom_smooth(method = "lm") +
    facet_wrap(~species, ##scales = "free_y",
               ncol = 3) +
    theme_bw() +
    stat_fit_glance(label.y = 'bottom',
                    method = "lm", method.args = list(formula = y ~ x),
                    aes(label = paste("p-value =", formatC(..p.value.., digits = 2)))) +
    stat_fit_tidy(label.y = 'bottom', label.x = 'center',
                  method = "lm",
                  mapping = aes(label = sprintf('slope~"="~%.3g',
                                                after_stat(x_estimate))),
                  parse = TRUE) +
    xlab('Year') +
    ylab('Annual mean season length (days)') +
    xlim(1997, 2018) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))

ggsave(plot = days1, filename = 'Figures/creep/Figure_days1.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)


days2 <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, region, numDays) %>%
    group_by(year, state, species, region) %>%
    dplyr::mutate(yrdays = sum(numDays)) %>%
    group_by(species, year) %>%
    dplyr::summarize(mdaysw = mean(yrdays)) %>%
    ggplot(aes(x = year, y = mdaysw))+
    geom_point(col = 'grey')+
    geom_smooth(method = "lm") +
    facet_wrap(~species, scales = "free_y",  ncol = 3) +
    theme_bw() +
    stat_regline_equation() +
    xlab('Year') +
    ylab('Annual mean season length (days)') +
    xlim(1997, 2018) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))
ggsave(plot = days2, filename = 'Figures/creep/Figure_days2.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)


ylims = list(
    NULL, c(200, 310), NULL,
    NULL, c(220, 350), NULL,  ##c(170, 250),
    NULL, NULL, NULL, ##c(170, 310), c(140, 270), c(270, 310),
    NULL, NULL, c(200, 260), ##c(240, 330),
    c(300, 360), c(250, 310), c(70, 250),
    NULL, NULL, NULL,##c(320, 350), c(250, 330), c(240, 300),
    NULL, NULL, c(170, 290)) ##c(120, 210),
scale_inidividual_facet_y_axes(days, ylims = ylims)

ggsave(plot = days, filename = 'Figures/Figure_3lm.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)

ggsave('Figures/Figure3lm.tiff', dpi = 700)


########### min size
dfmsl <-
  as_tibble(regs) %>%
  filter(year >= 1997) %>%
  select(species, year, state, min.size) %>%
  group_by(species, year) %>%
  dplyr::summarize(msize = mean(min.size))


minsize1 <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, min.size) %>%
    ggplot(aes(x = year, y = min.size))+
    geom_point(col = 'grey', alpha = 0.2) +
    geom_smooth(method = "lm") +
    facet_wrap(~species, scales = "free_y",  ncol = 3) +
    theme_bw() +
    stat_regline_equation() +
    xlab('Year') +
    ylab('Minimum size (inch)') +
    xlim(1997, 2018) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))


minsize1 <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, min.size) %>%
    ggplot(aes(x = year, y = min.size))+
    geom_point(col = 'grey', alpha = 0.2) +
    geom_smooth(method = "lm") +
    facet_wrap(~species, scales = "free_y",  ncol = 3) +
    theme_bw() +
    stat_fit_glance(label.y = 'bottom',
                    method = "lm", method.args = list(formula = y ~ x),
                    aes(label = paste("p-value =", formatC(..p.value.., digits = 2)))) +
    stat_fit_tidy(label.y = 'bottom', label.x = 'center',
                  method = "lm",
                  mapping = aes(label = sprintf('slope~"="~%.3g',
                                                after_stat(x_estimate))),
                  parse = TRUE) +
    xlab('Year') +
    ylab('Minimum size (inch)') +
    xlim(1997, 2018) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))


ylims = list(NULL, NULL, NULL, ##c(12, 15),
             NULL, NULL, NULL,
             NULL, NULL, c(0, 11),
             NULL, c(11, 15), NULL,
             NULL, c(0, 13), c(10, 13),
             NULL, NULL, NULL,
             NULL, NULL, NULL)
scale_inidividual_facet_y_axes(minsize1, ylims = ylims)
ggsave(plot = minsize1, filename = 'Figures/creep/Figure_minsize1.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)

as_tibble(regs) %>%
    filter(year >= 1997,
           species == 'Lutjanus campechanus',
           min.size == 0)


## minsize2 <-
##     as_tibble(regs) %>%
##     filter(year >= 1997) %>%
##     select(species, year, state, min.size) %>%
##     group_by(species, year) %>%
##     dplyr::summarize(msize = mean(min.size)) %>%
##     ggplot(aes(x = year, y = msize))+
##     geom_point(col = 'grey')+
##     geom_smooth(method = "lm") +
##     facet_wrap(~species, scales = "free_y",  ncol = 3) +
##     theme_bw() +
##     stat_regline_equation() +
##     xlab('Year') +
##     ylab('Minimum size (inch)') +
##     xlim(1997, 2018) +
##     theme(legend.position = "none",
##           panel.grid.major = element_blank(),
##           axis.text.y = element_text(size = 10),
##           strip.text = element_text(size = 8, face = "italic"),
##           strip.background = element_rect(fill = "white"),
##           axis.line = element_line(colour = "black"))
## ggsave(plot = minsize2, filename = 'Figures/creep/Figure_minsize2.pdf',
##        width = 210, height = 200, units = "mm")#, dpi = 300)

##### bag limit
bag1 <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, bag.limit) %>%
    ggplot(aes(x = year, y = bag.limit))+
    geom_point(col = 'grey', alpha = 0.2) +
    geom_smooth(method = "lm") +
    facet_wrap(~species, scales = "free_y",  ncol = 3) +
    theme_bw() +
    stat_regline_equation() +
    xlab('Year') +
    ylab('Bag limit') +
    xlim(1997, 2018) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))

bag1 <-
as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, bag.limit) %>%
    ggplot(aes(x = year, y = bag.limit))+
    geom_point(col = 'grey', alpha = 0.2) +
    geom_smooth(method = "lm") +
    facet_wrap(~species, scales = "free_y",  ncol = 3) +
    theme_bw() +
    stat_fit_glance(label.y = 'bottom',
                    method = "lm", method.args = list(formula = y ~ x),
                    aes(label = paste("p-value =", formatC(..p.value.., digits = 2)))) +
    stat_fit_tidy(label.y = 'bottom', label.x = 'center',
                  method = "lm",
                  mapping = aes(label = sprintf('slope~"="~%.3g',
                                                after_stat(x_estimate))),
                  parse = TRUE) +
    xlab('Year') +
    ylab('Bag limit') +
    xlim(1997, 2018) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))


ylims = list(NULL, NULL, NULL,
             NULL, c(0, 11), NULL,
             NULL, NULL, NULL,
             c(1, 4), NULL, NULL,
             NULL, NULL, NULL,
             NULL, NULL, NULL,
             c(0, 4), NULL, NULL)
scale_inidividual_facet_y_axes(bag1, ylims = ylims)


ggsave(plot = bag1, filename = 'Figures/creep/Figure_bag1.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)

regs %>%
    ungroup() %>%
    filter(##species == 'Morone saxatilis',
        min.size == 0) %>%
    select(species) %>%
    distinct()

regs %>%
    filter(species == 'Lutjanus campechanus',
        min.size == 0)

bag2 <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, bag.limit) %>%
    group_by(species, year) %>%
    dplyr::summarize(mbag = mean(bag.limit)) %>%
    ggplot(aes(x = year, y = mbag))+
    geom_point(col = 'grey')+
    geom_smooth(method = "lm") +
    facet_wrap(~species, scales = "free_y",  ncol = 3) +
    theme_bw() +
    stat_regline_equation() +
    xlab('Year') +
    ylab('Bag limit') +
    xlim(1997, 2018) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))
ggsave(plot = bag2, filename = 'Figures/creep/Figure_bag2.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)


table(regs$species)
ggsave(plot = bag, filename = 'Figures/Figure_1lm.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)

ggsave('Figures/Figure1lm.tiff', dpi = 700)

getwd()
