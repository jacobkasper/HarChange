rm(list = ls())

library(tidyverse)

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

days <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, region, numDays) %>%
    group_by(year, state, species, region) %>%
    mutate(yrdays = sum(numDays)) %>%
    group_by(species, year) %>%
    summarize(mdaysw = mean(yrdays)) %>%
    ggplot(aes(x = year, y = mdaysw)) +
    geom_line() +
    facet_wrap(.~species, scales = "free_y",  ncol = 3) +
               #labeller = label_wrap_gen(width=10),
    xlab('Year') +
    ylab('Annual mean season length (days)') +
    xlim(1997, 2018) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
          strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))

days

ylims = list(
    NULL, c(200, 310), NULL,
    NULL, c(220, 350), NULL,  ##c(170, 250),
    NULL, NULL, NULL, ##c(170, 310), c(140, 270), c(270, 310),
    NULL, NULL, c(200, 260), ##c(240, 330),
    c(300, 360), c(250, 310), c(70, 250),
    NULL, NULL, NULL,##c(320, 350), c(250, 330), c(240, 300),
    NULL, NULL, c(170, 290)) ##c(120, 210),
scale_inidividual_facet_y_axes(days, ylims = ylims)

ggsave(plot = days, filename = 'Figures/Figure_3.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)

ggsave('Figures/Figure3.tiff', height = 8, width = 8, dpi = 700)



## as_tibble(regs) %>%
##     filter(year > 1997) %>%
##     select(species, year, state, region, numDays) %>%
##     group_by(year, state, species, region) %>%
##     mutate(yrdays = sum(numDays)) %>%
##     ggplot(aes(x = year, y = yrdays, fill = factor(state))) +
##     geom_area(position = 'stack') +
##     facet_wrap(.~species, scales = "free_y",  ncol = 3) +
##     xlab('Year') +
##     ylab('Annual mean season length (days)') +
##     xlim(1998, 2018) +
##     theme_bw() +
##     theme(legend.position = "none",
##           panel.grid.major = element_blank(),
##           axis.text.y = element_text(size = 10),
##           strip.text = element_text(size = 8, face = "italic"),
##           strip.background = element_rect(fill = "white"),
##           axis.line = element_line(colour = "black"))

msl <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, min.size) %>%
    group_by(species, year) %>%
    summarize(msize = mean(min.size)) %>%
    ggplot(aes(x = year, y = msize)) +
    geom_line() +
    facet_wrap(.~species, scales = "free_y",  ncol = 3) +
    xlim(1997, 2018) +
    xlab('Year') +
    ylab('Annual mean minimum size limit (in)') +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
                    strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))

msl


ylims = list(NULL, NULL, c(11, 15),
             NULL, c(16, 22), NULL,
             c(16, 19), c(12, 23), NULL,
             c(17, 30), c(11, 15), NULL,
             NULL, c(2, 5), NULL,
             c(15, 18), NULL, c(10, 13),
             c(27, 32), c(7, 10), NULL)#c(13, 16))
scale_inidividual_facet_y_axes(msl, ylims = ylims)
ggsave(plot = msl, filename = 'Figures/Figure_2.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)


ggsave('Figures/Figure2.tiff',height = 8, width = 8, dpi = 700)

bag <-
    as_tibble(regs) %>%
    filter(year >= 1997) %>%
    select(species, year, state, bag.limit) %>%
    group_by(species, year) %>%
    summarize(mbag = mean(bag.limit)) %>%
    ggplot(aes(x = year, y = mbag)) +
    geom_line() +
    facet_wrap(.~species, scales = "free_y", ncol = 3) +
    xlim(1997, 2018) +
    xlab('Year') +
    ylab('Annual mean bag limit') +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8, face = "italic"),
                    strip.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"))
bag


ylims = list(NULL, NULL, c(8, 16), ##c(10, 45), c(8, 45), c(8, 16),
             NULL, c(4, 10), c(13, 30), ##c(0, 14), c(1, 5), c(9, 15),
             c(1, 5), c(10, 40), NULL, ##c(0, 4), c(10, 25), c(70, 140),
             c(1, 4), c(0, 13), c(2, 13), # c(2, 10),
             NULL, c(10, 13), c(0, 15), ##c(20, 40), c(10, 13), c(0, 20),
             c(0, 4), c(0, 4), c(8, 16),
             c(0, 4), c(30, 120), c(2, 15))
scale_inidividual_facet_y_axes(bag, ylims = ylims)

ggsave(plot = bag, filename = 'Figures/Figure_1.pdf',
       width = 210, height = 200, units = "mm")#, dpi = 300)

ggsave('Figures/Figure1.tiff', height = 8, width = 8, dpi = 700)

getwd()
