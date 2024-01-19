empirlogitTransform <- function(p, eps) { log(eps + p/(1-p + eps)) }

stand.species <- function(df, species){
    df <- df[df$species == species,]
    df <- with(df, data.frame(year, wave, species, area, state, ##5
                              har.n.day, bag.limitLS, min.sizeLS, ##8
                              bag.above.maxLS, max.sizeLS, ##10
                              min.size2LS, bonus.limitLS, ##12
                              pr.days = empirlogitTransform(pr.days, min(pr.days)), ##13
                              F.ls, SSB.ls, ##15
                              gdp,
                              pop.densityL = log(pop.density),
                              med.age,
                              med.income, med.house.size,
                              unemp.rtL = logitTransform(unemp.rt),
                              price, p.min, p.max,
                              t.minL = log(t.min + abs(min(t.min)) + 1),
                              t.maxL = log(t.max + abs(min(t.max)) + 1),
                              w.minL = log(w.min),
                              w.maxL = log(w.max)))
    df <- data.frame(df[1:15],
                     apply(df[16:28], 2, standardize))
}
