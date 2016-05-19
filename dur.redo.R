# need to redo duration as well
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

names(combined.data) <- c( "start.time","end.time", "lower.freq", "upper.freq",
                          "duration", "x", "site", "individual",
                          "date", "filename")


combined.data$x <- NULL

dur.summary.y1 <- summarise(group_by(combined.data, individual, date, site),
                             mean.duration = mean(duration))

dur.summary.y1$date <- as.Date(dur.summary.y1$date, "%m%d%y")
year(dur.summary.y1$date) <- 2014

dur.summary.y1$site[dur.summary.y1$site == "L2"] <- "L1"
dur.summary.y1$site[dur.summary.y1$site == "L3"] <- "L2"

# year 2
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

names(combined.data) <- c( "start.time","end.time", "lower.freq", "upper.freq",
                           "duration", "x", "site", "individual",
                           "date", "filename")


combined.data$x <- NULL

dur.summary.y2 <- summarise(group_by(combined.data, individual, date, site),
                            mean.duration = mean(duration))

dur.summary.y2$date <- as.Date(ymd(dur.summary.y2$date))

dur.summary <- rbind(dur.summary.y1, dur.summary.y2)

dur.summary.subset <- dur.summary[dur.summary$individual %in% both.years$individual,]

dur.summary.subset$median.db <- NA

for(i in 1:nrow(dur.summary.subset)) {
  indiv <- dur.summary.subset$individual[i]
  db <- both.years$median.db[both.years$individual == indiv]
  dur.summary.subset$median.db[i] <- db
}

dur.summary.subset$temp <- NA

for(i in 1:nrow(dur.summary.subset)) {
  indiv <- dur.summary.subset$individual[i]
  temp <- both.years$temp[both.years$individual == indiv]
  dur.summary.subset$temp[i] <- temp
}

#modeling

downsamp.duration.model <- lme(mean.duration ~ median.db + temp,
                           random = ~1|site,
                           data = both.years.subset)

#graphing----
downsamp.duration.ints <- as.data.frame(intervals(downsamp.duration.model)$fixed)
duration.p.vals <-  summary(downsamp.duration.model)$tTable[,5]
duration.t.vals <-  summary(downsamp.duration.model)$tTable[,4]

downsamp.duration.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(downsamp.duration.ints) <- NULL
names(downsamp.duration.ints) <- c("lower", "est", "upper", "type")
downsamp.duration.ints <- downsamp.duration.ints[2:3,]
duration.ci <- ggplot(downsamp.duration.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(y=0), linetype = "dashed") +
  xlab("Fixed effect") +
  ylab("Slope estimate") +
  annotate("text", x = 1, y = -.005, label = paste("p = ", signif(duration.p.vals[2], 3),
                                                "\nt79 = ", signif(duration.t.vals[2],3),
                                                sep = "")) +
  annotate("text", x = 2.25, y = -.005, label = paste("p = ", signif(duration.p.vals[3],3),
                                                  "\nt79 = ", signif(duration.t.vals[3],3),
                                                  sep = "")) +
  theme(text = element_text(size = 20)) +
  ggtitle("Duration slope estimates")
ggsave("~/Google_Drive/School/Research/Thesis/presentation/duration.ci.png",
       width= 8, height = 6)

# 256 analysis-----
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

names(combined.data) <- c( "start.time","end.time", "lower.freq", "upper.freq",
                           "duration", "x", "site", "individual",
                           "date", "filename")


combined.data$x <- NULL
dur256.summary.y1 <- summarise(group_by(combined.data, individual, date, site),
                            mean.duration = mean(duration))

dur256.summary.y1$date <- as.Date(dur256.summary.y1$date, "%m%d%y")
year(dur256.summary.y1$date) <- 2014

dur256.summary.y1$site[dur256.summary.y1$site == "L2"] <- "L1"
dur256.summary.y1$site[dur256.summary.y1$site == "L3"] <- "L2"

# year 2
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

names(combined.data) <- c( "start.time","end.time", "lower.freq", "upper.freq",
                           "duration", "x", "site", "individual",
                           "date", "filename")


combined.data$x <- NULL

dur256.summary.y2 <- summarise(group_by(combined.data, individual, date, site),
                            mean.duration = mean(duration))

dur256.summary.y2$date <- as.Date(ymd(dur256.summary.y2$date))

dur256.summary <- rbind(dur256.summary.y1, dur256.summary.y2)

dur256.summary.subset <- dur256.summary[dur256.summary$individual %in% both.years$individual,]

dur256.summary.subset$median.db <- NA

for(i in 1:nrow(dur256.summary.subset)) {
  indiv <- dur256.summary.subset$individual[i]
  db <- both.years$median.db[both.years$individual == indiv]
  dur256.summary.subset$median.db[i] <- db
}

dur256.summary.subset$temp <- NA

for(i in 1:nrow(dur256.summary.subset)) {
  indiv <- dur256.summary.subset$individual[i]
  temp <- both.years$temp[both.years$individual == indiv]
  dur256.summary.subset$temp[i] <- temp
}

#modeling

downsamp.dur256ation.model <- lme(mean.duration ~ median.db + temp,
                               random = ~1|site,
                               data = dur256.summary.subset)

  
  
  
  

# just making sure of things ----

dur.site.model <- lm(mean.duration ~ site + temp,
                     data = dur.summary.subset)
