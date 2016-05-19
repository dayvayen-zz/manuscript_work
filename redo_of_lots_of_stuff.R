# new both years combo

both.years.subset <- both.years
both.years.subset$individual <- as.character(both.years.subset$individual)
both.years.subset$individual[34] <- "Q2_individual-005_2"
both.years.subset <- both.years.subset[both.years.subset$individual %in% freq.summary.subset$individual,]
both.years.subset$X <- NULL
both.years.subset <- both.years.subset[,c(1:4, 6:7, 12:13 )]

both.years.subset$mean.center.freq <- NA

freq.summary.subset$individual[13] <- "Q2_individual-005_2"
dur.summary.subset$individual[13] <- "Q2_individual-005_2"


for(i in 1:nrow(both.years.subset)) {
  indiv <- both.years.subset$individual[i]
  freq <- freq.summary.subset[freq.summary.subset$individual == indiv,]$mean.center.freq
  both.years.subset$mean.center.freq[i] <- freq
}

both.years.subset$mean.duration <- NA

for(i in 1:nrow(both.years.subset)) {
  indiv <- both.years.subset$individual[i]
  dur <- dur.summary.subset[dur.summary.subset$individual == indiv,]$mean.duration
  both.years.subset$mean.duration[i] <- dur
}

dependent.variables <- as.matrix(cbind(both.years.subset[,c(2,5,9,10)]))
cor(dependent.variables)

pairs(dependent.variables)

# multi-response stuff ----

mvm <- MCMCglmm(cbind(source.level,callrate,mean.center.freq,mean.duration) ~ trait,
                random = ~site,
                family = c("gaussian", "gaussian", "gaussian", "gaussian"),
                rcov = ~site:individual,
                data = both.years.subset) # doesn't work, don't know why

melt.both.years.subset <- melt(both.years.subset, 
                               id.var = c("individual", "date", "site", "temp", "median.db"),
                               variable.name = "trait")

# L2_unid <- lmer(value ~ trait - 1 + (0 + trait | id) + (0 + trait | obs), data = mSpending)

mvm <- lmer(value ~ trait - 1 +
              (0 + trait | site),
            data = melt.both.years.subset)

scaled <- as.data.frame(scale(dependent.variables))
names(scaled) <- c("scaled.sl", "scaled.callrate", "scaled.mcfreq", "scaled.duration")
both.years.scaled <- cbind(both.years.subset, scaled)

both.years.scaled <- both.years.scaled[,c(1,3,4,6:8,11:14)]

melt.scaled <- melt(both.years.scaled, 
              id.var = c("individual", "date", "site", "temp", "median.db","rh"),
                variable.name = "trait",
               measured = c("scaled.sl", "scaled.callrate", "scaled.mcfreq", "scaled.duration"))

scaled.mvm <- lmer(value ~ trait - 1 +
                     (0 + trait|site),
                   data = melt.scaled)




# let's check the radius----

t <- mean(both.years.subset$temp)
dur.intercepts <- summary(downsamp.duration.model)$tTable[,1]
freq.intercepts <- summary(downsamp.freq.model)$tTable[,1]

both.years.subset$time.area <- with(both.years.subset, 
                                   callrate*mean.duration*(pi*radius^2))

qplot(data = both.years.subset, x = median.db, y = total.time)

both.years.subset$total.time <- with(both.years.subset,
                                     callrate*mean.duration)

ggplot(both.years.subset, aes(x = median.db, y = total.time)) +
  geom_point() +
  stat_smooth(method = "lm")

newdat <- expand.grid(both.years.subset$temp, both.years.subset$median.db)
names(newdat) <- c("temp", "median.db")

# total time model?----

total.time.model <- lme(total.time ~ median.db + temp,
                        random = ~1|site,
                        data = both.years.subset)

total.time.predict <- predict(total.time.model, newdat, level = 0)
total.time.predict <- as.data.frame(cbind(newdat, total.time.predict))
names(total.time.predict) <- c("temp", "median.db", "value")
palette2 <- colorRampPalette(c("blue", "green", "red"), space = "Lab")

ggplot() +
  geom_line(data = total.time.predict, aes(x = median.db, y = value,
                                         color = as.factor(temp)),
            size = 2) +
  scale_color_manual(values = palette2(23)) +
  theme_bw() +
  xlab("RMS received level (RL) in 1-4.5kHz bandwidth (dB re 20 micropascals)") +
  ylab("Call rate (calls per minute)") +
  theme(legend.position = "none") +
  theme(text = element_text(size=14)) +
  ggtitle("c") +
  theme(plot.title = element_text(hjust = 0))

freq.predict <- predict(downsamp.freq.model, newdat, level = 0)
freq.predict <- as.data.frame(cbind(newdat, freq.predict))
names(freq.predict) <- c("temp", "median.db", "value")

ggplot() +
  geom_line(data = freq.predict, aes(x = median.db, y = value,
                                           color = as.factor(temp)),
            size = 2) +
  scale_color_manual(values = palette2(23)) +
  theme_bw() +
  xlab("RMS received level (RL) in 1-4.5kHz bandwidth (dB re 20 micropascals)") +
  ylab("Call rate (calls per minute)") +
  theme(legend.position = "none") +
  theme(text = element_text(size=14)) +
  ggtitle("c") +
  theme(plot.title = element_text(hjust = 0))

# dicking around ----

temperatures <- summarise(group_by(both.years.subset, site),
                          mean.temp = mean(temp),
                          max.temp = max(temp),
                          min.temp = min(temp))

grouped.both.years <- group_by(both.years.subset, site)

sites <- unique(as.character(both.years.subset$site))

newdf <- as.data.frame(cbind(sites, rep(NA, 8)))
names(newdf) <- c("site", "freq.8c")
newdf$freq.8c <- as.numeric(newdf$freq.8c)
for(i in 1:length(sites)) {
  site <- sites[i]
  temp.df <- both.years.subset[both.years.subset$site == site, ]
  freq.8c <- mean(temp.df[which.min(abs(temp.df$temp - 8)),]$mean.center.freq)
  newdf$freq.8c[i] <- freq.8c
}

# 3d scatterplot----
library(plotly)
plot_ly(both.years.subset, x = temp, y = median.db, z = mean.center.freq,
        type = "scatter3d",
        mode = "markers",
        color = site) %>%
  layout(title = "Mean centroid freq against temperature and noise")

both.years.subset$normalized.freq <- scale(both.years.subset$mean.center.freq)

plot_ly(both.years.subset, x = temp, y = median.db, z = normalized.freq,
        type = "scatter3d",
        mode = "markers",
        color = median.db) %>%
  layout(title = "Mean centroid freq against temperature and noise")

both.years.subset$group <- substr(both.years.subset$site, 1, 1)

plot_ly(both.years.subset, x = temp, y = median.db, z = normalized.freq,
        type = "scatter3d",
        mode = "markers",
        color = group) %>%
  layout(title = "Mean centroid freq against temperature and noise")

both.years.subset$normalized.temp <- scale(both.years.subset$temp)
both.years.subset$normalized.db <- scale(both.years.subset$median.db)

plot_ly(both.years.subset, x = normalized.temp, y = normalized.db, z = normalized.freq,
        type = "scatter3d",
        mode = "markers",
        color = normalized.db) %>%
  layout(title = "Mean centroid freq against temperature and noise")

plot_ly(both.years.subset, x = normalized.temp, y = normalized.db, z = normalized.freq,
        type = "scatter3d",
        mode = "markers",
        color = group) %>%
  layout(title = "Mean centroid freq against temperature and noise")


# radius plot ----
temp.given <- mean(both.years.subset$temp)
radius.summary <- summarise(group_by(both.years.subset, site),
                            Radius = mean(radius),
                            Noise = mean(median.db))
callrate.fixed <- summary(callrate.model)$tTable[,1]
duration.fixed <- summary(downsamp.duration.model)$tTable[,1]
radius.summary$Callrate <- with(radius.summary, 
                                callrate.fixed[1] + 
                                  callrate.fixed[2]*Noise +
                                  callrate.fixed[3]*temp.given)

radius.summary$Duration <- with(radius.summary,
                                duration.fixed[1] +
                                  duration.fixed[2]*Noise +
                                  duration.fixed[3]*temp.given)

finalplot.dat <-as.data.frame(matrix(nrow = 8))
finalplot.dat$site <- radius.summary$site
finalplot.dat$radius <- radius.summary$Radius 
finalplot.dat$noise <- radius.summary$Noise
finalplot.dat$distance <- NA
for(i in 1:length(sites)) {
  site <- sites[i]
  distance <- distance.measures[distance.measures$site == site,]$meters
  finalplot.dat[finalplot.dat$site == site,]$distance <- distance
}
finalplot.dat$callrate <- radius.summary$Callrate
finalplot.dat$duration <- radius.summary$Duration
finalplot.dat$time.area <- with(finalplot.dat,
                                  callrate*duration)

finalplot.dat$V1 <- NULL

# names(finalplot.dat) <- c("Site", "Active space radius", "Noise level",
#                           "Distance", "Call rate", "Call duration", 
#                           "Time-area")

callrate.radius <- ggplot(finalplot.dat) +
  geom_point(aes(x = distance, y = noise, size = radius, 
                 color =callrate)) +
  scale_colour_gradientn(colours=c("blue", "green", "red")) +
  scale_size(range = c(4, 25)) +
  scale_y_continuous(limits = c(20, 60)) +
  theme_bw() +
  xlab("Distance from road with 30,000 AADT or more (m)") +
  ylab("RMS received level (RL) in 1-4.5kHz\n bandwidth (dB re 20 micropascals)") +
  theme(text = element_text(size=20))


ggplot(finalplot.dat) +
  geom_point(aes(x = distance, y = noise, size = radius, 
                 color = duration)) +
  scale_colour_gradientn(colours=c("blue", "green", "red")) +
  scale_size(range = c(4, 25)) +
  scale_y_continuous(limits = c(20, 60)) +
  theme_bw() +
  xlab("Distance from road with 30,000 AADT or more (m)") +
  ylab("RMS received level (RL) in 1-4.5kHz\n bandwidth (dB re 20 micropascals)") +
  theme(text = element_text(size=20))

total.time.radius <- ggplot(finalplot.dat) +
  geom_point(aes(x = distance, y = noise, size = radius, 
                 color = total.time)) +
  scale_colour_gradientn(colours=c("blue", "green", "red"), 
                         limits = c(0, 27)) +
  scale_size(range = c(4, 25)) +
  scale_y_continuous(limits = c(20, 60)) +
  theme_bw() +
  xlab("Distance from road with 30,000 AADT or more (m)") +
  ylab("RMS received level (RL) in 1-4.5kHz\n bandwidth (dB re 20 micropascals)") +
  theme(text = element_text(size=20))

multiplot(callrate.radius, total.time.radius)

ggplot(both.years.subset, aes(x = median.db, y = total.time,
                          color = temp)) +
  geom_point(size = 4) +
  scale_colour_gradientn(colours=c("blue", "green", "red")) +
  scale_size(range = c(4, 25)) +
  theme_bw() 

ggplot(finalplot.dat, aes(x = noise, y = duration, size = radius,
                          color = callrate)) +
  geom_point()

both.years.subset$tempfactor <- mround(both.years.subset$temp, 1)

mround <- function(x,base){ 
  base*round(x/base) 
} 


both.years.subset$noiseround <- mround(both.years.subset$median.db, 5)

# facet by temperature, bin by noise

both.years.subset$tempfactor <- as.factor(both.years.subset$tempfactor)
both.years.subset$noiseround <- as.factor(both.years.subset$noiseround)

ggplot(both.years.subset, 
       aes(x = tempfactor, y = mean.duration)) +
  geom_bar(stat = "identity") +
  facet_wrap(~noiseround)

ggplot(both.years.subset, aes(x = median.db, y =callrate, 
                              color = tempfactor, size = radius)) +
  geom_point() +
#   scale_colour_gradientn(colours=c("blue", "green", "red")) +
  scale_size(range = c(2, 15)) +
  theme_bw()

ggplot(both.years.subset, aes(x = median.db, y = temp, 
                              color = callrate, size = radius)) +
  geom_point(size = 4) +
  scale_colour_gradientn(colours=c("navy", "gray90")) +
  scale_size(range = c(2, 15)) +
  theme_bw()

# redo confidence intervals and models ----

downsamp.callrate.model <- lme(callrate ~ median.db + temp,
                               random = ~1|site,
                               data = both.years.subset)

downsamp.sourcelevel.model <- lme(source.level ~ median.db + temp,
                               random = ~1|site,
                               data = both.years.subset)

total.time.model <- lme(total.time ~ median.db + temp,
                        random = ~1|site,
                        data = both.years.subset)

downsamp.callrate.ints <- as.data.frame(intervals(downsamp.callrate.model)$fixed)
callrate.p.vals <-  summary(downsamp.callrate.model)$tTable[,5]
callrate.t.vals <-  summary(downsamp.callrate.model)$tTable[,4]

downsamp.callrate.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(downsamp.callrate.ints) <- NULL
names(downsamp.callrate.ints) <- c("lower", "est", "upper", "type")
downsamp.callrate.ints <- downsamp.callrate.ints[2:3,]
ggplot(downsamp.callrate.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(y=0), linetype = "dashed") +
  xlab("Fixed effect") +
  ylab("Slope estimate") +
  annotate("text", x = 1, y = 2, label = paste("p = ", signif(callrate.p.vals[2], 3),
                                                "\nt79 = ", signif(callrate.t.vals[2],3),
                                                sep = "")) +
  annotate("text", x = 2.2, y =2, label = paste("p = ", signif(callrate.p.vals[3],3),
                                                  "\nt79 = ", signif(callrate.t.vals[3],3),
                                                  sep = "")) +
  theme(text = element_text(size = 20)) +
  ggtitle("Callrate slope estimates")

callrate.ci <- ggplot(downsamp.callrate.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(y=0), linetype = "dashed") +
  xlab("Fixed effect") +
  ylab("Slope estimate") +
  annotate("text", x = 1, y = 2, label = paste("p = ", signif(callrate.p.vals[2], 3),
                                               "\nt79 = ", signif(callrate.t.vals[2],3),
                                               sep = "")) +
  annotate("text", x = 2.2, y =2, label = paste("p = ", signif(callrate.p.vals[3],3),
                                                "\nt79 = ", signif(callrate.t.vals[3],3),
                                                sep = "")) +
  theme(text = element_text(size = 20)) +
  ggtitle("Callrate slope estimates")

downsamp.sourcelevel.ints <- as.data.frame(intervals(downsamp.sourcelevel.model)$fixed)
sourcelevel.p.vals <-  summary(downsamp.sourcelevel.model)$tTable[,5]
sourcelevel.t.vals <-  summary(downsamp.sourcelevel.model)$tTable[,4]

downsamp.sourcelevel.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(downsamp.sourcelevel.ints) <- NULL
names(downsamp.sourcelevel.ints) <- c("lower", "est", "upper", "type")
downsamp.sourcelevel.ints <- downsamp.sourcelevel.ints[2:3,]
sourcelevel.ci <- ggplot(downsamp.sourcelevel.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(y=0), linetype = "dashed") +
  xlab("Fixed effect") +
  ylab("Slope estimate") +
  annotate("text", x = 1, y = -.5, label = paste("p = ", signif(sourcelevel.p.vals[2], 3),
                                               "\nt79 = ", signif(sourcelevel.t.vals[2],3),
                                               sep = "")) +
  annotate("text", x = 2.2, y =-.5, label = paste("p = ", signif(sourcelevel.p.vals[3],3),
                                                "\nt79 = ", signif(sourcelevel.t.vals[3],3),
                                                sep = "")) +
  theme(text = element_text(size = 20)) +
  ggtitle("Source level slope estimates")

total.time.ints <- as.data.frame(intervals(total.time.model)$fixed)
total.time.p.vals <-  summary(total.time.model)$tTable[,5]
total.time.t.vals <-  summary(total.time.model)$tTable[,4]

total.time.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(total.time.ints) <- NULL
names(total.time.ints) <- c("lower", "est", "upper", "type")
total.time.ints <- total.time.ints[2:3,]
total.time.ci <- ggplot(total.time.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(y=0), linetype = "dashed") +
  xlab("Fixed effect") +
  ylab("Slope estimate") +
  annotate("text", x = 1, y = -.2, label = paste("p = ", signif(total.time.p.vals[2], 3),
                                                 "\nt79 = ", signif(total.time.t.vals[2],3),
                                                 sep = "")) +
  annotate("text", x = 2.2, y =-.2, label = paste("p = ", signif(total.time.p.vals[3],3),
                                                  "\nt79 = ", signif(total.time.t.vals[3],3),
                                                  sep = "")) +
  theme(text = element_text(size = 20)) +
  ggtitle("Total calling time slope estimates")

multiplot(sourcelevel.ci, callrate.ci, total.time.ci, freq.ci, duration.ci, 
          cols = 2)

# Tables ----
stargazer(downsamp.callrate.model, downsamp.freq.model, downsamp.duration.model, downsamp.sourcelevel.model,
          report = "vcp*", 
          intercept.bottom = F, 
          single.row = T, 
          omit.stat = c("aic", "bic", "ll", "lr"),
          title = "Model summary",
          covariate.labels = c("Intercept", "Median noise level", "Temperature"),
          dep.var.labels = c("Call rate in calls/min", "Frequency in Hz", "Duration in s", "Source level in dB re 20 micropascals"),
          type = "html")

grouped.both.years <- group_by(both.years.subset,site)
by.site <- dplyr::summarise(grouped.both.years, 
                     n = n(),
                     med.source.level = median(source.level),
                     iqr.source.level = IQR(source.level),
                     mean.callrate = mean(callrate),
                     sd.callrate = sd(callrate),
                     meanfreq = mean(mean.center.freq),
                     sdfreq = sd(mean.center.freq),
                     meanduration = mean(mean.duration),
                     sdduration = sd(mean.duration),
                      distance = mean())

names(by.site) <- c("Site", "n", "Median \n source level", "IQR \n source level",
                    "Mean \n callrate", "SD \n callrate", "Mean centroid \n frequency",
                    "SD centroid \n frequency", "Mean duration", "SD Duration")

by.site$Site <- c("Talking Water Gardens", "Bond Butte", "Ogle Rd",
                  "Finley Finger", "Ankeny", "Baskett Slough", "E. E. Wilson",
                  "Jackson Frazier")
by.site$site <- NULL

print(xtable(by.site), floating =F, type = "html")


radius.summary$"Total time" <- with(radius.summary,
                                    Callrate*Duration)

radius.summary$"Time-volume" <- radius.summary$"Total time"*
  (2/3*pi*radius.summary$Radius^3)

print(xtable(radius.summary), floating = F)


# partial regression plots -----

library(visreg)

dur.partial <- visreg(downsamp.duration.model, "median.db", type = "conditional",
       xlab = "Noise level \n(dB re 20 micropascals)", 
       ylab = "Duration (s)",
       main = "Partial regression: noise vs. duration")

callrate.partial <- visreg(downsamp.callrate.model, "median.db", type = "conditional",
       xlab = "Noise level \n(dB re 20 micropascals)",
       ylab = "Call rate (calls/min)",
       main = "Partial regression: noise vs. call rate")

total.time.partial <- visreg(total.time.model, "median.db", type = "conditional",
       xlab = "Noise level \n(dB re 20 micropascals)",
       ylab = "Total time spent calling (s/min)",
       main = "Partial regression: noise vs. total time calling")

# total time ----

both.years.subset$total.time <- with(both.years.subset, callrate * mean.duration)
both.years.subset$time.volume <- with(both.years.subset, total.time * 
                                        (2/3 * pi * radius^2))
