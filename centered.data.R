# centered data remodel

both.years.subset$center.julian <- scale(both.years.subset$julian, scale = F, center = T)
both.years.subset$center.temp <- scale(both.years.subset$temp, scale = F, center = T)
both.years.subset$center.noise <- scale(both.years.subset$median.db, scale = F, center = T)

# call rate models -----

callrate.model.full <- lme(callrate ~ center.noise*center.temp*center.julian,
                           random = ~1|site,
                           data = both.years.subset)

callrate.aic <- dredge(callrate.model.full)

# best is the simplest: noise and temp

# freq models ----

freq.model.full <- lme(mean.center.freq ~ center.noise*center.temp*center.julian,
                       random = ~1|site,
                       data = both.years.subset)

freq.aic <- dredge(freq.model.full)

# best fitting is noise + temp + julian day

# source level models ----

source.level.model.full <- lme(source.level ~ center.noise*center.temp*center.julian,
                               random = ~1|site,
                               data = both.years.subset)

sl.aic <- dredge(source.level.model.full)

# best model is intercept-- it's not significantly related to anything.

# duration models ----

duration.model.full <- lme(mean.duration ~ center.noise*center.temp*center.julian,
                           random = ~1|site,
                           data = both.years.subset)

dur.aic <- dredge(duration.model.full)

# best fit is date and temp -- not noise at all

# final models ----

duration.model.centered <- lme(mean.duration ~ center.temp + center.julian,
                               random = ~1|site,
                               data = both.years.subset)

freq.model.centered <- lme(mean.center.freq ~ center.noise + center.temp + center.julian,
                           random = ~1|site,
                           data = both.years.subset)

callrate.model.centered <- lme(callrate ~ center.noise + center.temp,
                               random = ~1|site,
                               data = both.years.subset)
