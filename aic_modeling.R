# aic testing of every. single. damn. model. -----

callrate.model <- lme(callrate ~ median.db + temp,
                      random = ~1|site,
                      method = "ML",
                      data = both.years.subset)

callrate.model.date <- lme(callrate ~ median.db + temp + date,
                         random = ~1|site,
                           method = "ML",
                           data = both.years.subset)

callrate.model.interact <- lme(callrate ~ median.db * temp,
                               random = ~1|site,
                               method = "ML",
                               data = both.years.subset)
callrate.model.date.interact <- lme(callrate ~ median.db + temp*date,
                                    random = ~1|site,
                                    method = "ML",
                                    data = both.years.subset)

anova(callrate.model, callrate.model.date, callrate.model.interact,callrate.model.date.interact)


duration.model <- lme(mean.duration ~ median.db + temp,
                      random = ~1|site,
                      method = "ML",
                      data = both.years.subset)

duration.model.date <- lme(mean.duration ~ median.db + temp + date,
                                    random = ~1|site,
                                    method = "ML",
                                    data = both.years.subset)

duration.model.date.interact <- lme(mean.duration ~ median.db + temp * date,
                      random = ~1|site,
                      method = "ML",
                      data = both.years.subset)

duration.model.interact <- lme(mean.duration ~ median.db * temp,
                      random = ~1|site,
                      method = "ML",
                      data = both.years.subset)

anova(duration.model, duration.model.date, duration.model.interact, duration.model.date.interact)

freq.model <- lme(mean.center.freq ~ median.db + temp,
                  random = ~1|site,
                  method = "ML",
                  data = both.years.subset)

freq.model.date <- lme(mean.center.freq ~ median.db + temp + date,
                  random = ~1|site,
                  method = "ML",
                  data = both.years.subset)

freq.model.date.interact <- lme(mean.center.freq ~ median.db + temp * date,
                       random = ~1|site,
                       method = "ML",
                       data = both.years.subset)

freq.model.interact <- lme(mean.center.freq ~ median.db * temp,
                  random = ~1|site,
                  method = "ML",
                  data = both.years.subset)

anova(freq.model, freq.model.date, freq.model.interact, freq.model.date.interact)

source.level.model <- lme(source.level ~ median.db + temp,
                  random = ~1|site,
                  method = "ML",
                  data = both.years.subset)

source.level.model.date <- lme(source.level ~ median.db + temp + date,
                          random = ~1|site,
                          method = "ML",
                          data = both.years.subset)

source.level.model.interact <- lme(source.level ~ median.db * temp,
                          random = ~1|site,
                          method = "ML",
                          data = both.years.subset)

anova(source.level.model, source.level.model.date, source.level.model.interact, source.level.model.date.interact)

# final models -----

source.level.model.final <- lme(source.level ~ median.db + temp + date,
                               random = ~1|site,
                               data = both.years.subset)

freq.model.final <- lme(mean.center.freq ~ median.db + temp,
                  random = ~1|site,
                  data = both.years.subset)

duration.model.final <- lme(mean.duration ~ median.db + temp * date,
                                    random = ~1|site,
                                    data = both.years.subset)

callrate.model.final <- lme(callrate ~ median.db + temp,
                      random = ~1|site,
                      data = both.years.subset)
