library(commspacetime)
library(dplyr)
library(ggplot2)
library(lubridate)

both.years.subset <- read.csv("~/Google_Drive/School/Research/Thesis/both.years.subset.csv")

both.years.subset$date <- mdy(both.years.subset$date)

both.years.subset$absorption <- with(both.years.subset, 
                                     find_absorption(mean.center.freq, temp,
                                                C=TRUE, rh, p))

r_vector <- seq(0,3000,0.01)
both.years.subset$radius <- NA

for(i in 1:nrow(both.years.subset)) {
  temp_df <- both.years.subset[i,]
  radius <- with(temp_df, find_r(source.level, median.db, r_vector, absorption, perception, sphere = F))
  both.years.subset$radius[i] <- radius
}

both.years.subset$volume <- (2/3)*pi*both.years.subset$radius^3
both.years.subset$total.time <- with(both.years.subset, callrate*mean.duration)
both.years.subset$time.volume <- with(both.years.subset, total.time*volume)
