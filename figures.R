

# CONFIDENCE INTERVALS:----
## Callrate =====

callrate.ints <- as.data.frame(intervals(callrate.model.final)$fixed)
callrate.p.vals <-  summary(callrate.model.final)$tTable[,5]
callrate.t.vals <-  summary(callrate.model.final)$tTable[,4]
callrate.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(callrate.ints) <- NULL
names(callrate.ints) <- c("lower", "est", "upper", "type")
callrate.ints <- callrate.ints[2:3,]
callrate.ci <- ggplot(callrate.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype = "dashed") +
  ylab("Callrate\n slope estimate") +
  xlab(NULL) +
  annotate("text", x = 2.1, y = .1, label = "*", size = 12) +
  theme(text = element_text(size = 14)) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())




## Source level ====
sourcelevel.ints <- as.data.frame(intervals(source.level.model.final, which = "fixed")$fixed)
sourcelevel.p.vals <-  summary(source.level.model.final)$tTable[,5]
sourcelevel.t.vals <-  summary(source.level.model.final)$tTable[,4]

sourcelevel.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(sourcelevel.ints) <- NULL
names(sourcelevel.ints) <- c("lower", "est", "upper", "type")
sourcelevel.ints <- sourcelevel.ints[2:3,]
sl.ci <- ggplot(sourcelevel.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype = "dashed") +
  ylab("Source level\n slope estimate") +
  xlab(NULL) +
  theme(text = element_text(size = 14)) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())

## Frequency ====
freq.ints <- as.data.frame(intervals(freq.model.final, which = "fixed")$fixed)
freq.p.vals <-  summary(freq.model.final)$tTable[,5]
freq.t.vals <-  summary(freq.model.final)$tTable[,4]

freq.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(freq.ints) <- NULL
names(freq.ints) <- c("lower", "est", "upper", "type")
freq.ints <- freq.ints[2:3,]
freq.ci <- ggplot(freq.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype = "dashed") +
  ylab("Frequency\n slope estimate") +
  xlab(NULL) +
  annotate("text", x = 1, y = 5, label = "*", size = 12) +
  annotate("text", x = 2.1, y = 20, label = "*", size = 12) +
  theme(text = element_text(size = 14))

## Duration ====
duration.ints <- as.data.frame(exp(intervals(duration.model.final, which = "fixed")$fixed))
duration.p.vals <-  summary(duration.model.final)$tTable[,5]
duration.t.vals <-  summary(duration.model.final)$tTable[,4]

duration.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(duration.ints) <- NULL
names(duration.ints) <- c("lower", "est", "upper", "type")
duration.ints <- duration.ints[2:3,]
dur.ci <- ggplot(duration.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype = "dashed") +
  ylab("Duration \nslope estimate") +
  xlab(NULL) +
  annotate("text", x = 1, y = .5, label = "*", size = 12) +
  annotate("text", x = 2, y = .5, label = "*", size = 12) +
  theme(text = element_text(size = 14)) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())

## Total time ====

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
  geom_hline(aes(yintercept=0), linetype = "dashed") +
  ylab("Total time\n slope estimate") +
  xlab(NULL) + 
  theme(text = element_text(size = 14)) 

# Collected confidence interval plots ----
library(gtable)
library(grid) # low-level grid functions are required
g1 <- ggplotGrob(callrate.ci)
g2 <- ggplotGrob(dur.ci)
g3 <- ggplotGrob(total.time.ci)
g <- rbind(g1, g2, g3) # stack the two plots
g$widths <- unit.pmax(g1$widths, g2$widths) # use the largest widths
# center the legend vertically
g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)


g4 <- ggplotGrob(freq.ci)
g5 <- ggplotGrob(sl.ci)
g.a <- rbind(g5, g4)
g.a$widths <- unit.pmax(g1$widths, g2$widths)
g.a$layout[grepl("guide", g.a$layout$name),c("t","b")] <- c(1,nrow(g.a))
grid.newpage()
grid.draw(g.a)

# Partial regression plots ----
library(visreg)
library(RGraphics)
library(gridExtra)
library(gridBase)
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
pushViewport(vp.BottomRight)

dur.partial <- visreg(duration.model.final, "median.db", 
                      trans = exp,
                      type = "conditional", 
                      ylab = "Duration (s)", xlab = NULL)

callrate.partial <- visreg(callrate.model.final, "median.db", type = "conditional",
                           ylab = "Call rate (calls/min)")

total.time.partial <- visreg(total.time.model, "median.db", type = "conditional",
                             xlab = "Noise level \n(dB re 20 micropascals)",
                             ylab = "Total time spent calling (s/min)")

# Final bubble plot ----

distance.measures <- read.csv("distance.measures.csv")
distance.measures$meters <- with(distance.measures, distance/3.2808)

temp.given <- mean(both.years.subset$temp)
radius.summary <- summarise(group_by(both.years.subset, site),
                            Radius = mean(radius),
                            Noise = mean(median.db))
callrate.fixed <- summary(callrate.model.final)$tTable[,1]
duration.fixed <- summary(duration.model.final)$tTable[,1]
radius.summary$Callrate <- with(radius.summary, 
                                callrate.fixed[1] + 
                                  callrate.fixed[2]*Noise +
                                  callrate.fixed[3]*temp.given)

radius.summary$Duration <- with(radius.summary,
                                exp(duration.fixed[1]) +
                                  exp(duration.fixed[2])*Noise +
                                  exp(duration.fixed[3])*temp.given)

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
finalplot.dat$total.time <- finalplot.dat$callrate * finalplot.dat$duration

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


duration.radius <- ggplot(finalplot.dat) +
  geom_point(aes(x = distance, y = noise, size = radius, 
                 color = duration)) +
  scale_colour_gradientn(colours=c("blue", "green", "red")) +
  scale_size(range = c(4, 25)) +
  scale_y_continuous(limits = c(20, 60)) +
  theme_bw() +
  xlab("Distance from road with 30,000 AADT or more (m)") +
  ylab("RMS received level (RL) in 1-4.5kHz\n bandwidth (dB re 20 micropascals)") +
  theme(text = element_text(size=14))

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
  theme(text = element_text(size=14))

rad.grob1 <- ggplotGrob(duration.radius)
rad.grob2 <- ggplotGrob(total.time.radius)

rad.grob <- rbind(rad.grob1, rad.grob2)
rad.grob$widths <- unit.pmax(rad.grob1$widths, rad.grob2$widths)
rad.grob$layout[grepl("guide", rad.grob$layout$name),c("t","b")] <- c(1,nrow(rad.grob))
grid.newpage()
grid.draw(rad.grob)
