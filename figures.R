

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
  geom_hline(aes(yintercept=1), linetype = "dashed") +
  ylab("Duration proportion \nchange estimate") +
  xlab(NULL) + 
  annotate("text", x = 2, y = .9, label = "*", size = 12) +
  theme(text = element_text(size = 14))


# Collected confidence interval plots ----
library(gtable)
library(gridExtra)
library(grid) # low-level grid functions are required
g1 <- ggplotGrob(callrate.ci)
g2 <- ggplotGrob(dur.ci)
g4 <- ggplotGrob(freq.ci)
g5 <- ggplotGrob(sl.ci)

g <- grid.arrange(g1, g5, g2, g4, ncol = 2)



# radius reduction plot ----

ggplot() +
  geom_point(aes(both.years.subset$median.db, both.years.subset$radius)) +
  geom_smooth(aes(x = newdat$median.db, y = radius.predict)) +
  ylab("Radius (m)") +
  xlab("RMS received level (RL) in 1-4.5kHz\n bandwidth (dB re 20 micropascals)") +
  theme_bw()

rgl.open()
rgl.bg(color="white")
rgl.clipplanes(0,1,0,.0001)
rgl.spheres(0,0,0,507.2757,color="#0000ff",front="line",back="line",lwd=1,lit=FALSE)
rgl.spheres(0,0,0,507.2757,color="#0000ff",alpha=0.1)
rgl.spheres(800,0,0,17.42,color="#cc0000",front="line",back="line",lwd=1,lit=FALSE)
rgl.spheres(0,0,0,17.42,color="#cc0000",alpha=0.1)
axes3d(color = "#000000", alpha = 1, expand = 1.03, labels = F)
grid3d(side = c("z", "x","y"), col = "black")
snapshot3d("hemispheres.png")
