load("~/flu_surveil_data/ranks_w_0.Rda")
load("~/Dropbox/122cities/ili_reg.Rda")
source("~/repos/flu_surveil/county_level_to_region/optimization_with_lag/make_positive_lag.R")
load("~/flu_surveil_data/flu_gold_regions.Rda")

seq_mode = seq(1,67, by = 2)
ranks_mode_all = ranks[,seq_mode]
ranks_mode = ranks_mode_all[,13:23]
deaths = flu_gold_regions
national = paste('total.',ranks[,'national_mode_2_no_season'],sep ='')

# lag by 2 weeks
reg = ili_reg[,c('date',national)]

# function lags the input data
make_lag_input = function(provider_data, lag_step){
  for (i in 2:(ncol(provider_data)-1)){
    provider_data[,i] = make_positive_lag(provider_data[,c(1,i)], lag = lag_step)
  }
  return(provider_data)
}

provider_data = ili_reg[,colSums(ili_reg) > 0]
providers_lag0 = provider_data
providers_lag1 = make_lag_input(provider_data, 1) 
providers_lag2 = make_lag_input(provider_data, 2) 
providers_lag3 = make_lag_input(provider_data, 3) 
providers_lag4 = make_lag_input(provider_data, 4)
providers_lag5 = make_lag_input(provider_data, 5) 

predicted_deaths = data.frame(date = ili_reg[,'date'])

par(mfrow=c(3,2))
# run regressions with optimized sets of providers for each region and then make histogram/plots of residuals to decide which approach is best to use
# (approach being using a seasonal variable, no seasonal, or dropping season 3)
for (i in 1:ncol(ranks_mode)){
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
  X = providers_lag[,c(top_10)]
  reg_opt = cbind(deaths[,region], X)
  colnames(reg_opt)[1] = 'deaths'
  fit = lm(deaths ~., data = reg_opt)
  summary(fit)
  coefs = as.data.frame(t(coef(summary(fit))[,'Estimate']))
  deaths_pred = reg_opt
  deaths_pred[,1] = rep(coefs[1,1],nrow(deaths_pred))
  for (i in 2:ncol(reg_lag)){
    deaths_pred[,i] = reg_lag[,i]*coefs[1,i]
  } 
  deaths_pred$x = rowSums(deaths_pred)
  predicted_deaths = cbind(predicted_deaths,deaths_pred$x)
  #colnames(predicted_deaths)[i] = region_num
  #plot(deaths_pred$x, type = 'l')
  #lines(deaths[,'deaths.national'], col = 'red')
}

names = c('National' ,paste('Region', seq(1,10)))
colnames(predicted_deaths) = c('date',names)

load("~/flu_surveil_data/ili_wide_cnty_zeros_FIPS.Rda")
all_dates = data.frame(date = ili_wide_cnty_zeros_FIPS[,'date'])
predicted_deaths_full = merge(predicted_deaths,all_dates, all.y = TRUE)

deaths_full =  merge(deaths,all_dates, all.y = TRUE)

start = 200800
start_yr = as.numeric(substr(start,1,4))
end = 201200
date <- as.Date(paste(start_yr, '-01-01' , sep = "")) + seq(from = 1, to = 1460, by = 7) - 1

pdf(file = '~/flu_surveil_data/lag_data/actual_vs_predict.pdf', onefile = TRUE, width = 8.5, height = 11)

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