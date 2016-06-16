for(i in 1:nrow(both.years.subset)){
  sl <- both.years.subset$source.level[i]
  noise <- both.years.subset$median.db[i]
  abs <- both.years.subset$absorption[i]
  radius <- find_r(sl, noise, r_vector, abs, perception = 0, sphere = F)
  both.years.subset$radius[i] <- radius
}
both.years.subset$volume <- (2/3)*pi*both.years.subset$radius^2 
both.years.subset$time.volume <- with(both.years.subset, 
                                      total.time*volume)