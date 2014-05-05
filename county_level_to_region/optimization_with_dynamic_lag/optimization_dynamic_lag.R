library(plyr)
source("~/repos/flu_surveil/county_level_to_region/optimization_with_lag/make_positive_lag.R")
source("~/repos/flu_surveil/county_level/var_select_cnty.R")
source("~/repos/flu_surveil/county_level/var_select_cnty_season.R")

# load gold timeseries
load("~/flu_surveil_data/flu_gold_regions.Rda")
deaths = flu_gold_regions
# load ILINet data
load("~/Dropbox/122cities/ili_reg.Rda")

provider_data = ili_reg[,colSums(ili_reg) > 0]

# function lags the input data
make_lag_input = function(provider_data, lag_step){
  for (i in 2:(ncol(provider_data)-1)){
    provider_data[,i] = make_positive_lag(provider_data[,c(1,i)], lag = lag_step)
  }
  return(provider_data)
}

# create lags from 0 to 5 weeks and combine all the data
providers_lag0 = provider_data
colnames(providers_lag0) = paste(names,'_0',sep="")
providers_lag1 = make_lag_input(provider_data, 1)
colnames(providers_lag1) = paste(names,'_1',sep="")
providers_lag2 = make_lag_input(provider_data, 2) 
colnames(providers_lag2) = paste(names,'_2',sep="")
providers_lag3 = make_lag_input(provider_data, 3) 
colnames(providers_lag3) = paste(names,'_3',sep="")
providers_lag4 = make_lag_input(provider_data, 4)
colnames(providers_lag4) = paste(names,'_4',sep="")
providers_lag5 = make_lag_input(provider_data, 5) 
colnames(providers_lag5) = paste(names,'_5',sep="")

all_lags = cbind(cbind(cbind(cbind(cbind(providers_lag0,providers_lag1[,-1]), providers_lag2[,-1]), providers_lag3[,-1]),
                       providers_lag4[,-1]), providers_lag5[,-1]) 

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

n = ncol(deaths)
#save(flu_gold_all, file = 'flu_gold_all.Rda')
for (i in 2:n){
  print(i)
  load('r2_values.Rda')
  ranks = var_select_cnty(obj = deaths[,i], vars = all_lags[,2:ncol(all_lags)], goal = n_counties, r2_values = r2_values, ranks = ranks)
  save(ranks,file = 'ranks_w_0.Rda')
}
load("~/flu_surveil_data/ranks_w_0.Rda")
names = c('deaths.national' ,paste('deaths.', seq(1,10), sep=''))
colnames(ranks) = c('index',names)

ranks_mode = ranks

pdf(file = '~/flu_surveil_data/lag_data/regression_residuals_dynamic_lag.pdf', width = 8.5, height = 11)
par(mfrow=c(3,2))
# run regressions with optimized sets of providers for each region and then make histogram/plots of residuals to decide which approach is best to use
# (approach being using a seasonal variable, no seasonal, or dropping season 3)
for (i in 2:ncol(ranks_mode)){
  region = colnames(ranks_mode)[i]
  top_10 = paste('total.',ranks_mode[,i],sep='')
  
  # extract the top ten variables
  X = all_lags[,c(top_10)]
  # make regression ijnput
  reg_opt = cbind(deaths[,region], X)
  colnames(reg_opt)[1] = 'deaths'
  # do the fit
  fit = lm(deaths ~., data = reg_opt)
  summary(fit)
  # plot the residuals
  hist(fit$residuals, main = paste("Histogram of residuals for ", region_num," optimization 
                                     using a lag of ", lag, " (using no seasonal indicator)", sep = ''), xlab = "residual")
  plot(fit$residuals, main = paste("Residuals for ", region_num," optimization 
     using a lag of ", lag, " (using no seasonal indicator)", sep = ''), xlab = "time in weeks", ylab = "residuals")
  # calculate the predicted deaths
  coefs = as.data.frame(t(coef(summary(fit))[,'Estimate']))
  deaths_pred = reg_opt
  deaths_pred[,1] = rep(coefs[1,1],nrow(deaths_pred))
  for (i in 2:ncol(reg_lag)){
    deaths_pred[,i] = reg_lag[,i]*coefs[1,i]
  } 
  deaths_pred$x = rowSums(deaths_pred)
  predicted_deaths = cbind(predicted_deaths,deaths_pred$x)
}

dev.off()

# make the predicted and actual death comparison plots
names = c('National' ,paste('Region', seq(1,10)))
colnames(predicted_deaths) = c('date',names)

load("~/flu_surveil_data/ili_wide_cnty_zeros_FIPS.Rda")
all_dates = data.frame(date = ili_wide_cnty_zeros_FIPS[,'date'])
predicted_deaths_full = merge(all_dates, predicted_deaths, all.x = TRUE)

deaths_full =  merge(deaths,all_dates, all.y = TRUE)

start = 200800
start_yr = as.numeric(substr(start,1,4))
end = 201200
date <- as.Date(paste(start_yr, '-01-01' , sep = "")) + seq(from = 1, to = 1460, by = 7) - 1

# the following incredibly cumbersome code plots actual vs predicted time series (4 plots per page)
pdf(file = '~/flu_surveil_data/lag_data/actual_vs_predict_dynamic_lag.pdf', onefile = TRUE, width = 8.5, height = 11)
i = 2
deaths_df = cbind(predicted_deaths_full[,i],deaths_full[,i])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p1 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'),panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(),panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+1],deaths_full[,i+1])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p2 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+1], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+2],deaths_full[,i+2])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')
p3 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+2], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+3],deaths_full[,i+3])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p4 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+3], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+4],deaths_full[,i+4])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p5 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+4], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+5],deaths_full[,i+5])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p6 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+5], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+6],deaths_full[,i+6])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p7 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+6], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+7],deaths_full[,i+7])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p8 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+7], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+8],deaths_full[,i+8])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p9 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+8], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+9],deaths_full[,i+9])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p10 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+9], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+10],deaths_full[,i+10])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p11 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+10], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

grid.arrange(p1,p2,p3,p4,ncol=1,nrow=4)
grid.arrange(p5,p6,p7,p8,ncol=1,nrow=4)
grid.arrange(p9,p10,p11,ncol=1,nrow=3)
dev.off()
