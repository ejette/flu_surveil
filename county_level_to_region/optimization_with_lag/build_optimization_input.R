source("~/repos/flu_surveil/county_level_to_region/optimization_with_lag/make_positive_lag.R")
source("~/repos/flu_surveil/county_level/var_select_cnty.R")
source("~/repos/flu_surveil/county_level/var_select_cnty_season.R")
library(plyr)

# lag data constructed seasonal variable
load("~/flu_surveil_data/lag_data/lag_with_season.Rda")
# lag data constructed without seasonal variable
load("~/flu_surveil_data/lag_data/lag_without_season.Rda")
# lag data constructed without seasonal variable and leaving out the third season
load("~/flu_surveil_data/lag_data/lag_without_season3.Rda")

# inputs: lag = 0, predictor_data, y_data, n_providers, lag_data, season_3, use_season_var
# output: r2_values, provider_subset
# make lagged data frame
# run optimization

# run with no seasonal variable and all seasons
# run with season 3, no seasonal variable
# run with seasonal variable

# load optimization data
#load("~/Dropbox/122cities/r2_lag_single_var.Rda")
# limit lag data to only have HHS and national lag information
regions = c("deaths.national", "deaths.1", "deaths.2","deaths.3","deaths.4","deaths.5",
           "deaths.6","deaths.7","deaths.8","deaths.9","deaths.10")
#lag_data = r2_lag_single_var[r2_lag_single_var$city %in% regions,]

# load gold timeseries
load("~/flu_surveil_data/flu_gold_regions.Rda")
deaths = flu_gold_regions
# load ILINet data
load("~/Dropbox/122cities/ili_reg.Rda")

# find lags associated with mode lag for a given region and with max r^2 value for a given region
find_lag = function(lag_data, regions){
  for (i in 1:length(regions)){
    one_region = lag_data[lag_data$city == regions[i],]
    max_r2_lag = as.numeric(one_region$max_r2_lag[which.max(one_region$max_r2)])
    lag_freqs = count(lag_data$max_r2_lag[lag_data$city == regions[i]])
    mode_lag = which.max(lag_freqs$freq) - 1
    if (i == 1){
      opt_lag_data = data.frame(region = regions[i], type = 'max', lag = max_r2_lag, stringsAsFactors = FALSE)
      opt_lag_data = rbind(opt_lag_data, c(regions[i],'mode',mode_lag))
    } else {
      opt_lag_data = rbind(opt_lag_data, c(regions[i], 'max', max_r2_lag))
      opt_lag_data = rbind(opt_lag_data, c(regions[i], 'mode',mode_lag))
    }
  }
  opt_lag_data$lag = as.numeric(opt_lag_data$lag)
  return(opt_lag_data)
}
opt_lag_with_season = find_lag(lag_with_season, regions)
opt_lag_without_season = find_lag(lag_without_season, regions)
opt_lag_without_season3 = find_lag(lag_without_season3, regions)
opt_lag_with_season$season = "season"
opt_lag_without_season$season = "no_season"
opt_lag_without_season3$season = "no_season3"
opt_lag_data = rbind(rbind(opt_lag_with_season, opt_lag_without_season), opt_lag_without_season3)

provider_data = ili_reg[,colSums(ili_reg) > 0]

# make a season indicator in both dataframes
deaths$season = NA
deaths$season[deaths$date >= 200840 & deaths$date <= 200921] = 1
deaths$season[deaths$date >= 200940 & deaths$date <= 201021] = 2
deaths$season[deaths$date >= 201040 & deaths$date <= 201121] = 3
deaths$season[deaths$date >= 201140 & deaths$date <= 201221] = 4
deaths$season = as.factor(deaths$season)

provider_data$season[provider_data$date >= 200840 & provider_data$date <= 200921] = 1
provider_data$season[provider_data$date >= 200940 & provider_data$date <= 201021] = 2
provider_data$season[provider_data$date >= 201040 & provider_data$date <= 201121] = 3
provider_data$season[provider_data$date >= 201140 & provider_data$date <= 201221] = 4
provider_data$season = as.factor(provider_data$season)
  
# function lags the input data
make_lag_input = function(provider_data, lag_step){
  for (i in 2:(ncol(provider_data)-1)){
    provider_data[,i] = make_positive_lag(provider_data[,c(1,i)], lag = lag_step)
  }
  return(provider_data)
}

# size of optimal network
n_counties = 10
# ranks is a dataframe that keeps track of the counties chosen for the optimal network 
# and the order in which they were chosen
ranks = as.data.frame(1:n_counties)
colnames(ranks) = 'index'
# r2_values is a dataframe that keeps track of the r2 values associated with adding a provider in the
# ranks dataframe to the regression
r2_values = as.data.frame(1:n_counties)
colnames(r2_values) = 'index'
save(r2_values, file = 'r2_values.Rda')

# if season_3 = TRUE & use_season_var = FALSE
  # go through both max and mode
# if season_3 = FALSE & use_season_indicator = FALSE
  # go through both max and mode
# if season_3 = TRUE & use_season_indicator = TRUE
  # go through both max and mode
# go through each row in opt_lag_data
opt_lag_data$name = paste(opt_lag_data$region, opt_lag_data$type, opt_lag_data$lag, opt_lag_data$season, sep='_')
opt_lag_data$name = gsub("deaths\\.", "region", opt_lag_data$name) 
opt_lag_data$name = gsub("regionnational", "national", opt_lag_data$name) 

providers_lag0 = provider_data
providers_lag1 = make_lag_input(provider_data, 1) 
providers_lag2 = make_lag_input(provider_data, 2) 
providers_lag3 = make_lag_input(provider_data, 3) 
providers_lag4 = make_lag_input(provider_data, 4)
providers_lag5 = make_lag_input(provider_data, 5) 

for (j in 1:nrow(opt_lag_data)){
  #providers_lag = make_lag_input(provider_data, opt_lag_data[j,'lag']) 
  if (opt_lag_data[j,'lag'] == 0){
    providers_lag = providers_lag0 
  }
  if (opt_lag_data[j,'lag'] == 1){
    providers_lag = providers_lag1 
  }
  if (opt_lag_data[j,'lag'] == 2){
    providers_lag = providers_lag2 
  }
  if (opt_lag_data[j,'lag'] == 3){
    providers_lag = providers_lag3 
  }
  if (opt_lag_data[j,'lag'] == 4){
    providers_lag = providers_lag4 
  }
  if (opt_lag_data[j,'lag'] == 5){
    providers_lag = providers_lag5 
  }
  load('r2_values.Rda')
  if (opt_lag_data$season[j] == "no_season"){
    ranks = var_select_cnty(obj = deaths[,opt_lag_data[j,'region']], vars = providers_lag[,2:(ncol(providers_lag)-1)], goal = n_counties, r2_values = r2_values, ranks = ranks)
  }
  if (opt_lag_data$season[j] == "no_season3"){
    deaths_temp = deaths[deaths$season%in%c(1,2,4),opt_lag_data[j,'region']]
    providers_temp = providers_lag[providers_lag$season %in% c(1,2,4),2:(ncol(providers_lag)-1)]
    ranks = var_select_cnty(obj = deaths_temp, vars = providers_temp, goal = n_counties, r2_values = r2_values, ranks = ranks)
  }
  if (opt_lag_data$season[j] == "season"){
    ranks = var_select_cnty_season(obj = deaths[,opt_lag_data[j,'region']], vars = providers_lag[,2:ncol(providers_lag)], goal = n_counties, r2_values = r2_values, ranks = ranks)
  }
  save(ranks,file = 'ranks_w_0.Rda')
  colnames(ranks)[length(colnames(ranks))] = opt_lag_data$name[j]
  load('r2_values.Rda')
  colnames(r2_values)[length(colnames(r2_values))] = opt_lag_data$name[j]  
  save(r2_values, file = 'r2_values.Rda')
}

load("~/flu_surveil_data/ranks_w_0.Rda")

seq_mode = seq(1,67, by = 2)
ranks_mode = ranks[,seq_mode]
colnames(ranks_mode)[ncol(ranks_mode)] = 'region10_mode_1_no_season3'
pdf(file = '~/flu_surveil_data/lag_data/regression_residuals_mode.pdf', width = 8.5, height = 11)
par(mfrow=c(3,2))
# run regressions with optimized sets of providers for each region and then make histogram/plots of residuals to decide which approach is best to use
# (approach being using a seasonal variable, no seasonal, or dropping season 3)
for (i in 2:ncol(ranks_mode)){
  lag = as.numeric(strsplit(colnames(ranks_mode)[i], split = "_")[[1]][3])
  print(lag)
  var_name = strsplit(colnames(ranks_mode)[i], split = "_")
  print(var_name)
  type = paste(var_name[[1]][4],var_name[[1]][5],sep='_')
  region_num  = var_name[[1]][1]
  region_num = gsub('region', 'HHS region ', region_num)
  region = paste('deaths.',var_name[[1]][1],sep = '')
  region = gsub('region','',region)
  top_10 = paste('total.',ranks_mode[,i],sep='')
  if (lag == 0){
    providers_lag = providers_lag0 
  }
  if (lag == 1){
    providers_lag = providers_lag1 
  }
  if (lag == 2){
    providers_lag = providers_lag2 
  }
  if (lag == 3){
    providers_lag = providers_lag3 
  }
  if (lag == 4){
    providers_lag = providers_lag4 
  }
  if (lag == 5){
    providers_lag = providers_lag5 
  }
  if (type == 'season_NA'){
    X = providers_lag[,c(top_10,"season")]
    reg_opt = cbind(deaths[,region], X)
    colnames(reg_opt)[1] = 'deaths'
    fit = lm(deaths ~., data = reg_opt)
    summary(fit)
    hist(fit$residuals, main = paste("Histogram of residuals for ", region_num," optimization 
     using a lag of ", lag, " (using seasonal indicator)", sep = ''), xlab = "residual")
    plot(fit$residuals, main = paste("Residuals for ", region_num," optimization 
     using a lag of ", lag, " (using seasonal indicator)", sep = ''), xlab = "time in weeks", ylab = "residuals")
  }
  if (type == 'no_season'){
    X = providers_lag[,c(top_10)]
    reg_opt = cbind(deaths[,region], X)
    colnames(reg_opt)[1] = 'deaths'
    fit = lm(deaths ~., data = reg_opt)
    summary(fit)
    hist(fit$residuals, main = paste("Histogram of residuals for ", region_num," optimization 
     using a lag of ", lag, " (using no seasonal indicator)", sep = ''), xlab = "residual")
    plot(fit$residuals, main = paste("Residuals for ", region_num," optimization 
     using a lag of ", lag, " (using no seasonal indicator)", sep = ''), xlab = "time in weeks", ylab = "residuals")
  }
  if (type == 'no_season3'){
    X = providers_lag[,c(top_10)]
    reg_opt = cbind(deaths[,region], X)
    colnames(reg_opt)[1] = 'deaths'
    fit = lm(deaths ~., data = reg_opt)
    summary(fit)
    hist(fit$residuals, main = paste("Histogram of residuals for ", region_num," optimization 
     using a lag of ", lag, " (using no season 3)", sep = ''), xlab = "residual")
    plot(fit$residuals, main = paste("Residuals for ", region_num," optimization 
     using a lag of ", lag, " (using no season 3)", sep = ''), xlab = "time in weeks", ylab = "residuals")
  }
}

dev.off()
