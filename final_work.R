# stuff for new freq analysis

library(car)

indiv.summary$date <- parse_date_time(indiv.summary$date, "%y%m%d")
dates <- indiv.summary.y1$date

dates2 <- recode(dates, 
                                '"040414_o" = "20140404";
                                "031414_o" = "20140314";
                                "04252014" = "20140425";
                                "04212014" = "20140421";
                                "04182014" = "20140418"')
indiv.summary.y1$date <- dates2

indiv.summary.y1$date <- parse_date_time(indiv.summary.y1$date, "%y%m%d")

indiv.summary <- rbind(indiv.summary.y1, indiv.summary)

indiv.summary$date <- as.Date(indiv.summary$date)

indiv.summary[c(1:24),]$individual <- substr(indiv.summary[c(1:24),]$individual, 1,17)
indiv.summary[c(1:6),]$site <- "L1"
indiv.summary[c(7:10),]$site <- "L2"

indiv.summary$individual %in% both.years$individual

both.years <- both.years[order(both.years$date),]
indiv.summary <- indiv.summary[order(indiv.summary$date),]
indiv.summary2 <- indiv.summary[indiv.summary$individual %in% both.years$individual,]

indiv.summary2[90,] <- both.years[77,c(1,7,3,8,9)]
indiv.summary2 <- indiv.summary2[order(indiv.summary2$date),]
both.years$mean.freq <- indiv.summary2$mean.freq
both.years$sd.freq <- indiv.summary2$sd.freq


group.summary.both.years <- summarise(group_by(both.years, site),
                                      freq = mean(mean.freq),
                                      sdfreq = sd(mean.freq),
                                      dur = mean(mean.duration),
                                      sddur= sd(mean.duration),
                                      sl = median(source.level),
                                      sl.iqr = IQR(source.level),
                                      callrate = mean(callrate),
                                      sdrate = sd(callrate))

both.years.temp.reduced <- both.years[both.years$temp>=9 &both.years$temp <=12,]

p3 <- ggplot(both.years.temp.reduced, aes(x = median.db, y = callrate)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw() +
  xlab("RMS received level (RL) in 1-4.5kHz bandwidth (dB re 20 micropascals)") +
  ylab("Call rate (calls per minute)") +
  ggtitle("b") +
  theme(plot.title = element_text(hjust = 0)) +
  theme(text = element_text(size=14)) 

ggsave("~/Google_Drive/School/Research/Thesis/noise.callrate.raw.png",
       width= 8, height = 6)

multiplot(p2, p3, p1)

# distances

y1.distance <- read.csv("~/Google_Drive/School/Research/Data/Season_1/distance_gain_y1.csv")


y1.distance$site <- substr(y1.distance$filename, 1, 2)
y1.distance$individual <- substr(y1.distance$filename, 1, 17)
y2.distance$site <- substr(y2.distance$individual, 1, 2)
y1.distance <- y1.distance[, c("individual", "meters", "site")]
y2.distance <- all.data.gain[,c("individual", "distance", "site")]
names(y1.distance) <- c("individual", "distance", "site")
y1.distance[y1.distance$site == "L2",]$site <- "L1"
y1.distance[y1.distance$site == "L3",]$site <- "L2"


distance <- rbind(y1.distance, y2.distance)

distance <- distance[distance$individual %in% both.years$individual,]

site.distance <- summarise(group_by(distance, site),
                           "Mean recording distance" = mean(distance))