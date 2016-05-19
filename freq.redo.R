library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)
library(GGally)
library(lattice)
library(lme4)
library(gmodels)
library(ez)
library(nlme)
library(stargazer)
library(xtable)
library(texreg)

# year 1  ----

dir <- "~/Google_Drive/School/Research/Data/Season_1/Call_structure/Individual_recording_data/downsamp"
setwd(dir)
myFiles = list.files(path=dir, pattern="*_osprey.txt")

data <- lapply(myFiles, read.table, sep="\t", header=T)
names(data) <- sub(".txt", "", myFiles)
combined.data <- do.call(rbind, data)

combined.data$site <- substr(rownames(combined.data), 1, 2)
combined.data$individual <- substr(rownames(combined.data), 1, 17)
combined.data$date <- substr(rownames(combined.data), 19, 26)
combined.data$filename <- sapply(str_split(rownames(combined.data), "[.]"), "[", 1)

names(combined.data) <- c("peak.freq", "center.freq", "start.time",
                          "end.time", "lower.freq", "upper.freq",
                          "duration", "median.freq", "pk.cell.freq",
                          "pk.overall.freq", "x", "site", "individual",
                          "date", "filename")

combined.data$x <- NULL


freq.summary.y1 <- summarise(group_by(combined.data, individual, date, site),
                             mean.peak.freq = mean(peak.freq),
                             mean.center.freq = mean(center.freq),
                             median.freq = median(median.freq),
                             mean.pk.cell.freq = mean(pk.cell.freq),
                             mean.pk.overall.freq = mean(pk.overall.freq))

freq.summary.y1$date <- as.Date(freq.summary.y1$date, "%m%d%y")

freq.summary.y1$site[freq.summary.y1$site == "L2"] <- "L1"
freq.summary.y1$site[freq.summary.y1$site == "L3"] <- "L2"


# year 2 ----
dir <- "~/Google_Drive/School/Research/Data/Season_2/Soundfiles/single_channel/done/downsamp"
setwd(dir)
myFiles = list.files(path=dir, pattern="*_osprey.txt")
data <- lapply(myFiles, read.table, sep="\t", header=T)
names(data) <- sub(".txt", "", myFiles)
combined.data <- do.call(rbind, data)

combined.data$site <- substr(rownames(combined.data), 1, 2)
combined.data$individual <- substr(rownames(combined.data), 1, 15)
combined.data$date <- substr(rownames(combined.data), 8,15)
combined.data$filename <- sapply(str_split(rownames(combined.data), "[.]"), "[", 1)

names(combined.data) <- c("peak.freq", "center.freq", "start.time",
                          "end.time", "lower.freq", "upper.freq",
                          "duration", "median.freq", "pk.cell.freq",
                          "pk.overall.freq", "x", "site", "individual",
                          "date", "filename")

combined.data$x <- NULL
freq.summary.y2 <- summarise(group_by(combined.data, individual,date, site),
                             mean.peak.freq = mean(peak.freq),
                             mean.center.freq = mean(center.freq),
                             median.freq = median(median.freq),
                             mean.pk.cell.freq = mean(pk.cell.freq),
                             mean.pk.overall.freq = mean(pk.overall.freq))

freq.summary.y2$date <- as.Date(ymd(freq.summary.y2$date))

# merge together ----

freq.summary <- rbind(freq.summary.y1, freq.summary.y2)

both.years <- read.csv("~/Google_Drive/School/Research/Data/both.years.csv")

freq.summary.subset <- freq.summary[freq.summary$individual %in% both.years$individual,]

freq.summary.subset$median.db <- NA

for(i in 1:nrow(freq.summary.subset)) {
  indiv <- freq.summary.subset$individual[i]
  db <- both.years$median.db[both.years$individual == indiv]
  freq.summary.subset$median.db[i] <- db
}

freq.summary.subset$temp <- NA

for(i in 1:nrow(freq.summary.subset)) {
  indiv <- freq.summary.subset$individual[i]
  temp <- both.years$temp[both.years$individual == indiv]
  freq.summary.subset$temp[i] <- temp
}

year(freq.summary.subset$date[1:20]) <- 2014


# PCA to determine best variable -----

PCsd<-prcomp(freq.summary.subset[,c(4:8)], scale=FALSE , center = TRUE)
summary(PCsd)
PCsd
head(PCsd$x)
plot(PCsd, type="l")
princomp(PCsd)
biplot(PCsd)

# to do: rescale variables to their means. 

freqmeasures <- scale(freq.summary.subset[,c(4:8)])
PCscaled <- prcomp(freqmeasures, scale = F, center = F)
summary(PCscaled)
PCscaled

biplot(PCscaled)
# looks like center freq is the best and explains the most variation.

# modeling ----

downsamp.freq.model <- lme(mean.center.freq ~ median.db + temp,
                  random = ~1|site,
                  data = both.years.subset)

# graphing----

downsamp.freq.ints <- as.data.frame(intervals(downsamp.freq.model)$fixed)
freq.p.vals <-  summary(downsamp.freq.model)$tTable[,5]
freq.t.vals <-  summary(downsamp.freq.model)$tTable[,4]

downsamp.freq.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(downsamp.freq.ints) <- NULL
names(downsamp.freq.ints) <- c("lower", "est", "upper", "type")
downsamp.freq.ints <- downsamp.freq.ints[2:3,]
freq.ci <- ggplot(downsamp.freq.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(y=0), linetype = "dashed") +
  xlab("Fixed effect") +
  ylab("Slope estimate") +
  annotate("text", x = 1, y = 10, label = paste("p = ", signif(freq.p.vals[2], 3),
                                                "\nt79 = ", signif(freq.t.vals[2],3),
                                                sep = "")) +
  annotate("text", x = 2.2, y = 20, label = paste("p = ", signif(freq.p.vals[3],3),
                                                  "\nt79 = ", signif(freq.t.vals[3],3),
                                                  sep = "")) +
  theme(text = element_text(size = 20)) +
  ggtitle("Frequency slope estimates")

ggsave("~/Google_Drive/School/Research/Thesis/presentation/freq.ci.png",
       width= 8, height = 6)

# just making sure ----
freq.site.model <- lm(mean.center.freq ~ site + temp, 
                      data = freq.summary.subset)