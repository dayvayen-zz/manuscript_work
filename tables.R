# tables for final manuscript
library(stargazer)
library(xtable)




# final model table ----
stargazer(callrate.model.final, freq.model.final,duration.model.final, source.level.model.final,
          report = "vcp*", 
          intercept.bottom = F, 
          single.row = T, 
          omit.stat = c("aic", "bic", "ll", "lr"),
          notes = NULL,
          title = "Model summary",
          type = "text",
          omit.table.layout = "n",
          covariate.labels = c("Intercept", "Median noise level", "Temperature"),
          dep.var.labels = c("Call rate in calls/min", "Frequency in Hz", "Log(Duration in s)", "Source level in dB \nre 20 micropascals"))

# what else do I need? 
# Site information table ----

table1 <- read.csv("~/Google_Drive/School/Research/Thesis/table1.csv")
table1$Coordinates <- NULL

table1$"Songmeter gain settings (2014, 2015)" <- paste(table1$Songmeter.gain..2014, table1$Songmeter.gain..2015, sep = ", ")
table1$"Number recorded (2014, 2015)" <- paste(table1$n..2014, table1$n..2015, sep = ", ")
table1$"Recording nights (2014, 2015)" <- paste(table1$Recording.nights..2014, table1$Recording.nights..2015, sep = ", ")

table1$Songmeter.gain..2014 <- NULL
table1$Songmeter.gain..2015 <- NULL
table1$n..2014 <- NULL
table1$n..2015 <- NULL
table1$Recording.nights..2014 <- NULL
table1$Recording.nights..2015 <- NULL

names(table1$Site.name) <- "Site name"
names(table1$Years.recorded) <- "Years recorded"

stargazer(table1, summary = F)

# Summary statistics table ----
grouped.both.years <- group_by(both.years.subset,site)
by.site <- summarise(grouped.both.years, 
                     n = n(),
                     med.source.level = median(source.level),
                     iqr.source.level = IQR(source.level),
                     mean.callrate = mean(callrate),
                     sd.callrate = sd(callrate),
                     meanfreq = mean(mean.center.freq),
                     sdfreq = sd(mean.center.freq),
                     meanduration = mean(mean.duration),
                     sdduration = sd(mean.duration))

names(by.site) <- c("Site", "n", "Median source level\n (dB re 20 micropa)", "IQR source level \n (dB re 20 micropa)",
                    "Mean callrate \n (calls/min)", "SD callrate \n (calls/min)", "Mean centroid \n frequency (Hz)",
                    "SD centroid \n frequency (Hz)", "Mean duration (s)", "SD duration (s)", "Mean recording distance (m)")

by.site$Site <- c("Talking Water Gardens", "Bond Butte", "Ogle Rd",
                  "Finley Finger", "Ankeny", "Baskett Slough", "E. E. Wilson",
                  "Jackson Frazier")
by.site$site <- NULL
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}
distance <- c(3.38, 2.10, 3.39,3.53,3.04,2.72,2.39,2.79)
by.site$"Mean recording distance (m)" <- distance
by.site[,c(3:10)] <- round_df(by.site[,c(3:10)], digits = 2)
stargazer(by.site, summary = F)

# List of parameters ----

parameters <- c("Call rate", "Duration", "Source level", "Frequency")
units <- c("Calls per minute", "Seconds", "dB re 20 micropascals", "Hz")

parameters.table <- as.data.frame(cbind(parameters, units))
names(parameters.table) <- c("Parameter", "Units")

stargazer(parameters.table, summary = F)


# radius model table
stargazer(rad.mod.glmer.log,
          apply.coef = exp,
          report = "vcp*", 
          intercept.bottom = F, 
          single.row = T, 
          omit.stat = c("aic", "bic", "ll", "lr"),
          notes = NULL,
          title = "Radius model summary",
          omit.table.layout = "n",
          covariate.labels = c("Intercept", "Median noise level", "Temperature"),
          dep.var.labels = "Radius (m)",
          align = T)
