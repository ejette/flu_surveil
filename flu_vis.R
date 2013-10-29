library(ggplot2)
library(zoo)
setwd("~/flu_surveil_data")

mmwr <- read.csv("mmwr.csv")
flu <- mmwr[,c('city','state','mmrw_week_year','pneum_flu')]
colnames(flu) <- c('city','state','date','deaths')

pdf(file="~/flu_surveil_data/flu_seasons.pdf")

start = 200845
end = 200914
start_yr = 2008
end_yr = 2012
par(mfrow=c(2,2))
cities = unique(flu[,c('city','state')])

for (j in 1:nrow(cities)){
  for (i in 1:4){
    start = start + 100
    end = end + 100
    test <- flu[which((flu$date >= start & flu$date <= end) & (flu$city == cities[j,1] & flu$state == cities[j,2])),]
    date <- as.Date(paste(start_yr + i, '-11-03' , sep = "")) + seq(from = 1, to = 160, by = 7) - 1
    plot <- zoo(test$deaths, date)
    plot(plot, xlab = paste(start_yr+i,'Flu Season'), ylab = 'deaths', main = paste(cities[j,1],cities[j,2]))
  }
  start = 200845
  end = 200914
  start_yr = 2008
  end_yr = 2012
}

dev.off() 
# test$date_new <- test$date
# for (i in 1:nrow(test)){
#   if (test$date[i] >= 200901){
#     test$date_new[i] = test$date[i] - 47
#   }
# }
# 
# test$date_new[which(test$date >= 200901)] = test$date - 47
# y <- flu$deaths[which((flu$date >= 200844 & flu$date <= 200914) & flu$city == "Boston")]
# plot(flu$date[which((flu$date >= 200844 & flu$date <= 200914) & flu$city == "Boston")], flu$deaths[which((flu$date >= 200844 & flu$date <= 200914) & flu$city == "Boston")])
# 
# 
# try <- flu_boston[which((flu_boston$date >= 200844 & flu_boston$date <= 200914) | (flu_boston$date >= 200944 & flu_boston$date <= 201014)),]
