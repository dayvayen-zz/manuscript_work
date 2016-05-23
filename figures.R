# CONFIDENCE INTERVALS:----
## Callrate =====

callrate.ints <- as.data.frame(intervals(callrate.model.final)$fixed)
callrate.p.vals <-  summary(callrate.model.final)$tTable[,5]
callrate.t.vals <-  summary(callrate.model.final)$tTable[,4]
callrate.ints$x <- c("Intercept", "Noise", "Temperature")
rownames(callrate.ints) <- NULL
names(callrate.ints) <- c("lower", "est", "upper", "type")
callrate.ints <- callrate.ints[2:3,]
ggplot(callrate.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype = "dashed") +
  xlab("Fixed effect") +
  ylab("Callrate slope estimate") +
  annotate("text", x = 1, y = .1, label = "*", size = 12) +
  annotate("text", x = 2, y = .1, label = "*", size = 12) +
  theme(text = element_text(size = 20))




## Source level ====
sourcelevel.ints <- as.data.frame(intervals(source.level.model.final, which = "fixed")$fixed)
sourcelevel.p.vals <-  summary(source.level.model.final)$tTable[,5]
sourcelevel.t.vals <-  summary(source.level.model.final)$tTable[,4]

sourcelevel.ints$x <- c("Intercept", "Noise", "Temperature", "Date")
rownames(sourcelevel.ints) <- NULL
names(sourcelevel.ints) <- c("lower", "est", "upper", "type")
sourcelevel.ints <- sourcelevel.ints[2:4,]
sourcelevel.ci <- ggplot(sourcelevel.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype = "dashed") +
  xlab("Fixed effect") +
  ylab("Source level slope estimate") +
  annotate("text", x = 1, y = -.2, label = "*", size = 12) +
  theme(text = element_text(size = 20))

## Frequency ====
freq.ints <- as.data.frame(intervals(freq.model.final)$fixed)
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
  xlab("Fixed effect") +
  ylab("Frequency slope estimate") +
  annotate("text", x = 1, y = 5, label = "*", size = 12) +
  annotate("text", x = 2.1, y = 20, label = "*", size = 12) +
  theme(text = element_text(size = 20))

## Duration ====
duration.ints <- as.data.frame(intervals(duration.model.final, which = "fixed")$fixed)
duration.p.vals <-  summary(duration.model.final)$tTable[,5]
duration.t.vals <-  summary(duration.model.final)$tTable[,4]

duration.ints$x <- c("Intercept", "Noise", "Temperature", "Date", "Temperature:Date")
rownames(duration.ints) <- NULL
names(duration.ints) <- c("lower", "est", "upper", "type")
duration.ints <- duration.ints[2:3,]
duration.ci <- ggplot(duration.ints, aes(x = type, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(width = .15, aes(ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype = "dashed") +
  ylab("Duration slope estimate") +
  xlab(NULL) +
  # annotate("text", x = 1, y = -.005, label = paste("p = ", signif(duration.p.vals[2], 3),
  #                                                  "\nt79 = ", signif(duration.t.vals[2],3),
  #                                                  sep = "")) +
  # annotate("text", x = 2.25, y = -.005, label = paste("p = ", signif(duration.p.vals[3],3),
  #                                                     "\nt79 = ", signif(duration.t.vals[3],3),
  #                                                     sep = "")) +
  theme(text = element_text(size = 20))

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
  geom_hline(aes(y=0), linetype = "dashed") +
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