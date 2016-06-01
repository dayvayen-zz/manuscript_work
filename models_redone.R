# models, all over again.

# callrate ----

callrate.model <- lme(callrate ~ median.db + temp,
                      random = ~1|site,
                      data = both.years.subset)

callrate.res <- resid(callrate.model)
callrate.fit <- fitted(callrate.model)
qplot(callrate.fit, callrate.res)
qqnorm(callrate.res)
qqline(callrate.res)
callrate <- both.years.subset$callrate
qplot(temp, callrate.res)
qplot(noise, callrate.res)


# duration -----

duration.model <- lme(mean.duration ~ median.db + temp, 
                      random = ~1|site/date,
                      data = both.years.subset)

dur.res <- resid(duration.model)
dur.fit <- fitted(duration.model)
qplot(dur.fit, dur.res)
qqnorm(dur.res)
qqline(dur.res)
# no good-- definitely balloons outward.

duration.model <- lme(mean.duration ~ median.db + temp,
                      random = ~1|site/date,
                      weights = vdur5,
                      data = both.years.subset)

dur <- both.years.subset$mean.duration
temp <- both.years.subset$temp
noise <- both.years.subset$median.db
qplot(dur, dur.res)
qplot(temp, dur.res)
qplot(noise, dur.res)
vdurFixed <- varFixed(~temp) # closer
vdurIdent <- varIdent(form = ~1|temp) # big nope
vdurPower <- varPower(form = ~temp)  # best so far? 
vdurPower2 <- varPower(form = ~temp) # NOPE
vdurPower3 <- varPower(form = ~temp|median.db) # closer
vdur4 <- varExp(form = ~temp)
vdur5 <- varConstPower(form = ~temp)

#none of these are great...

both.years.subset$nlog.dur <- log(both.years.subset$mean.duration)

log.dur.model <- lme(nlog.dur ~ median.db + temp,
                     random = ~1|site/date,
                     data = both.years.subset)

logdur.res <- resid(log.dur.model)
logdur.fit <- fitted(log.dur.model)
qplot(logdur.fit, logdur.res)
qqnorm(logdur.res)
qqline(logdur.res)

# YES: the log model works. Okay. I'll get back to this later.

# source level ----
source.level.model <- lme(source.level ~ median.db + temp,
                          random = ~1|site/date,
                          data = both.years.subset)

sl.res <- resid(source.level.model)
sl.fit <- fitted(source.level.model)
qplot(sl.fit, sl.res)
qqnorm(sl.res)
qqline(sl.res)
# yeah that should be fine.

# frequency ----
freq.model <- lme(mean.center.freq ~ median.db + temp,
                  random = ~1|site/date,
                  data = both.years.subset)

freq.res <- resid(freq.model)
freq.fit <- fitted(freq.model)

qplot(freq.fit, freq.res)
qqnorm(freq.res)
qqline(freq.res)
# yep that's good
# final models ----

callrate.model.final <- lme(callrate ~ median.db + temp,
                            random = ~1|site/date,
                            data = both.years.subset)

source.level.model.final <- lme(source.level ~ median.db + temp,
                                random = ~1|site/date,
                                data = both.years.subset)

freq.model.final <- lme(mean.center.freq ~ median.db + temp, 
                        random = ~1|site/date,
                        data = both.years.subset)

duration.model.final <- lme(nlog.dur ~ median.db + temp,
                       random = ~1|site/date,
                       data = both.years.subset)

total.time.model <- lme(total.time ~ median.db + temp,
                        random = ~1|site/date,
                        data = both.years.subset)
