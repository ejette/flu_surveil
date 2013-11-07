library(zoo)
library(stringr)
library(ggplot2)

setwd("~/flu_surveil_data")

mmwr <- read.csv("mmwr.csv")
flu <- mmwr[,c('city','state','mmrw_week_year','pneum_flu')]
colnames(flu) <- c('city','state','date','deaths')

load("~/flu_surveil_data/ili_trim.Rda")

cities = unique(flu[,c('city','state')])
#cities = cities[order(cities$city,cities$state),]
cities = cities[order(cities$state),]

states <- str_trim(unique(ili_trim$state))
ili_trim$state = as.character(ili_trim$state)
ili_trim$city = as.character(ili_trim$city)

# make flu trend plots for all providers in one of the 122 cities
pdf(file="~/flu_surveil_data/flu_seasons_by_state_both.pdf")

#caclulate bounds for graphs
flu_agg <- aggregate(. ~ date,  data = flu[,c('date','deaths')], FUN = mean)
ili_agg <- aggregate(. ~ date,  data = ili_trim[,c('date','total')], FUN = mean)

max_y = max(max(flu_agg$deaths),max(ili_agg$total))

# initialize the first flu season
# start of the flu season
start = 200845
# end of the flu season
end = 200914
# first year we're looking at
start_yr = 2008

all_missing = ''

for (j in 1:length(states)){
  par(mfrow=c(2,2))
  for (i in 1:4){
    start = start + 100
    end = end + 100
    test <- ili_trim[which((ili_trim$date >= start & ili_trim$date <= end) & (ili_trim$state == states[j])), c('date','total')]
    test_mmwr <- flu[which((flu$date >= start & flu$date <= end) & (flu$state == states[j])), c('date','deaths')]
    if (nrow(test) > 0 | nrow(test_mmwr) > 0){
      if (nrow(test)>0){
        test_agg <- aggregate(. ~ date,  data = test, FUN = mean)
        if (sum(is.na(test_agg$total)) == nrow(test_agg)){
          test_agg$total <- rep(0,nrow(test_agg))
          all_missing = " - all missing"
        }
        date <- as.Date(paste(start_yr + i, '-11-03' , sep = "")) + seq(from = 1, to = 160, by = 7) - 1
        plot_ili <- zoo(test_agg$total, date)
        plot(plot_ili, xlab = paste(start_yr+i,'Flu Season'), ylim=c(0,max_y), ylab = 'deaths', main = paste(states[j],all_missing))
        all_missing = ''
      }
      if (nrow(test_mmwr) > 0){
        test_agg_mmwr <- aggregate(. ~ date,  data = test_mmwr, FUN = mean)
        if (sum(is.null(test_agg_mmwr$total)) == nrow(test_agg_mmwr)){
          test_agg_mmwr$deaths <- rep(0,nrow(test_agg_mmwr))
          all_missing = " - all missing"
        }
        date <- as.Date(paste(start_yr + i, '-11-03' , sep = "")) + seq(from = 1, to = 160, by = 7) - 1
        plot_mmwr <- zoo(test_agg_mmwr$deaths, date)
        lines(plot_mmwr, col = 'red')
        all_missing = ''
      }
    }
  }
  start = 200845
  end = 200914
  start_yr = 2008
}

dev.off()
