library(zoo)
library(stringr)

setwd("~/flu_surveil_data")

mmwr <- read.csv("mmwr.csv")
flu <- mmwr[,c('city','state','mmrw_week_year','pneum_flu')]
colnames(flu) <- c('city','state','date','deaths')

load("~/flu_surveil_data/ili_trim.Rda")

pdf(file="~/flu_surveil_data/flu_seasons_all_cities.pdf")

# initialize the first flu season
# start of the flu season
start = 200845
# end of the flu season
end = 200914
# first year we're looking at
start_yr = 2008

par(mfrow=c(2,2))
cities = unique(flu[,c('city','state')])
#cities = cities[order(cities$city,cities$state),]
cities = cities[order(cities$state),]
all_missing = ''

# make a time series plot for each city in 122 cities list for each flu season between 2008 and 2012
for (j in 1:nrow(cities)){
  for (i in 1:4){
    start = start + 100
    end = end + 100
    test <- flu[which((flu$date >= start & flu$date <= end) & (flu$city == cities[j,1] & flu$state == cities[j,2])),]
    if (sum(is.na(test$deaths)) == nrow(test)){
      test$deaths <- rep(0,nrow(test))
      all_missing = " - all missing"
    }
    date <- as.Date(paste(start_yr + i, '-11-03' , sep = "")) + seq(from = 1, to = 160, by = 7) - 1
    plot <- zoo(test$deaths, date)
    plot(plot, xlab = paste(start_yr+i,'Flu Season'), ylab = 'deaths', main = paste(cities[j,1],cities[j,2],all_missing))
    all_missing = ''
  }
  start = 200845
  end = 200914
  start_yr = 2008
}

dev.off()

# make timeseries plots but by the different regions defined by the MMWR system
# make lists containing states in different regions
new_england = c('MA','CT','RI')
mid_atlantic = c('NY','NJ','PA')
en_central = c('OH','IL','MI','IN','WI')
wn_central = c('IA','MN','NE','KS','MO')
es_central = c('AL','TN','KY') 
s_atlantic = c('MD','NC','VA','GA','DC','DE','FL','GA')
ws_central = c('TX','LA','OK','AR')
mountain = c('ID','NM','CO','NV','UT','AZ','NAT')
pacific = c('CA','HI','WA','OR')

all = c(new_england, mid_atlantic, en_central, wn_central,es_central, s_atlantic, ws_central, mountain, pacific)
all_regions = list(new_england, mid_atlantic, en_central,es_central, wn_central, s_atlantic, ws_central, mountain, pacific, all)

pdf(file="~/flu_surveil_data/flu_seasons_by_region_mmwr.pdf")
par(mfrow=c(2,2))

start = 200845
end = 200914
start_yr = 2008
all_missing = ''

for (j in 1:length(all_regions)){
  flu_sub = flu[flu$state %in% all_regions[[j]], c('date','deaths')]
  # aggregate 122 city data to be totals by date
  flu_agg <- aggregate(. ~ date,  data = flu_sub, FUN = mean)
  for (i in 1:4){
      start = start + 100
      end = end + 100
      test <- flu_agg[which((flu_agg$date >= start & flu_agg$date <= end)),]
      if (sum(is.na(test$deaths)) == nrow(test)){
          test$deaths <- rep(0,nrow(test))
          all_missing = " - all missing"
          }
       date <- as.Date(paste(start_yr + i, '-11-03' , sep = "")) + seq(from = 1, to = 160, by = 7) - 1
       plot <- zoo(test$deaths, date)
       plot(plot, xlab = paste(start_yr+i,'Flu Season'), ylab = 'deaths', main = paste(all_regions[[j]], collapse = ', '))
       all_missing = ''
      }  
      start = 200845
      end = 200914
      start_yr = 2008
}

dev.off()

pdf(file="~/flu_surveil_data/flu_seasons_by_state_mmwr.pdf")
par(mfrow=c(2,2))

start = 200845
end = 200914
start_yr = 2008
all_missing = ''

for (j in 1:length(all)){
  for (i in 1:4){
    start = start + 100
    end = end + 100
    test <- flu[which((flu$date >= start & flu$date <= end) & (flu$state == all[j])), c('date','deaths')]
    if (nrow(test) > 0){
      test_agg <- aggregate(. ~ date,  data = test, FUN = mean)
      if (sum(is.null(test_agg$total)) == nrow(test_agg)){
        test_agg$total <- rep(0,nrow(test_agg))
        all_missing = " - all missing"
      }
      date <- as.Date(paste(start_yr + i, '-11-03' , sep = "")) + seq(from = 1, to = 160, by = 7) - 1
      plot <- zoo(test_agg$deaths, date)
      plot(plot, xlab = paste(start_yr+i,'Flu Season'), ylab = 'deaths', main = paste(all[j],all_missing))
      all_missing = ''}
  }
  start = 200845
  end = 200914
  start_yr = 2008
}

dev.off() 

# make flu trend plots for all providers in one of the 122 cities
pdf(file="~/flu_surveil_data/flu_seasons_by_state_ili.pdf")

ili_trim$state = str_trim(ili_trim$state)
ili_trim$city = str_trim(ili_trim$city)

# initialize the first flu season
# start of the flu season
start = 200845
# end of the flu season
end = 200914
# first year we're looking at
start_yr = 2008

par(mfrow=c(2,2))
all_missing = ''

states <- str_trim(unique(ili_trim$state))
ili_trim$state = as.character(ili_trim$state)
ili_trim$city = as.character(ili_trim$city)

for (j in 1:length(states)){
  for (i in 1:4){
    start = start + 100
    end = end + 100
    test <- ili_trim[which((ili_trim$date >= start & ili_trim$date <= end) & (ili_trim$state == states[j])), c('date','total')]
    if (nrow(test) > 0){
    test_agg <- aggregate(. ~ date,  data = test, FUN = mean)
    if (sum(is.na(test_agg$total)) == nrow(test_agg)){
      test_agg$total <- rep(0,nrow(test_agg))
      all_missing = " - all missing"
    }
    date <- as.Date(paste(start_yr + i, '-11-03' , sep = "")) + seq(from = 1, to = 160, by = 7) - 1
    plot <- zoo(test_agg$total, date)
    plot(plot, xlab = paste(start_yr+i,'Flu Season'), ylab = 'deaths', main = paste(states[j],all_missing))
    all_missing = ''}
  }
  start = 200845
  end = 200914
  start_yr = 2008
}

dev.off()

# make flu trend plots for all providers in one of the 122 cities
pdf(file="~/flu_surveil_data/flu_seasons_by_state_both.pdf")

# initialize the first flu season
# start of the flu season
start = 200845
# end of the flu season
end = 200914
# first year we're looking at
start_yr = 2008

par(mfrow=c(2,2))
all_missing = ''

for (j in 1:length(states)){
  for (i in 1:4){
    start = start + 100
    end = end + 100
    test <- ili_trim[which((ili_trim$date >= start & ili_trim$date <= end) & (ili_trim$state == states[j])), c('date','total')]
    test_mmwr <- flu[which((flu$date >= start & flu$date <= end) & (flu$state == states[j])), c('date','deaths')]
    if (nrow(test) > 0 | nrow(test_mmwr) > 0){
      if (nrow(test_mmwr) > 0){
        test_agg_mmwr <- aggregate(. ~ date,  data = test_mmwr, FUN = mean)
        if (sum(is.na(test_agg_mmwr$total)) == nrow(test_agg_mmwr)){
          test_agg_mmwr$deaths <- rep(0,nrow(test_agg_mmwr))
          all_missing = " - all missing"
        }
        date <- as.Date(paste(start_yr + i, '-11-03' , sep = "")) + seq(from = 1, to = 160, by = 7) - 1
        plot_mmwr <- zoo(test_agg_mmwr$deaths, date)
        plot(plot_mmwr, xlab = paste(start_yr+i,'Flu Season'), ylab = 'deaths', main = paste(states[j],all_missing))
        all_missing = ''
      }
      if (nrow(test)>0){
        test_agg <- aggregate(. ~ date,  data = test, FUN = mean)
        if (sum(is.na(test_agg$total)) == nrow(test_agg)){
          test_agg$total <- rep(0,nrow(test_agg))
          all_missing = " - all missing"
        }
        date <- as.Date(paste(start_yr + i, '-11-03' , sep = "")) + seq(from = 1, to = 160, by = 7) - 1
        plot_ili <- zoo(test_agg$total, date)
        plot(plot_ili, xlab = paste(start_yr+i,'Flu Season'), ylab = 'deaths', main = paste(states[j],all_missing))
        all_missing = ''
      }
    }
  }
  start = 200845
  end = 200914
  start_yr = 2008
}

dev.off()
