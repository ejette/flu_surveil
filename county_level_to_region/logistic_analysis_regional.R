library(aod)
library(ggplot2)
library(RColorBrewer)
library(gplots)

source('~/repos/flu_surveil/logistic/format_fips.R')
load('~/flu_surveil_data/non_zero_counties.Rda')
setwd("~/Dropbox/122cities")
# load y data
load("~/flu_surveil_data/lag_data/y_lag.Rda")
y = y_lag
# load humidity difference date
load("~/flu_surveil_data/counties_hum_fips.Rda")
humidity_diff = counties_hum_fips

# load population difference data
load("~/flu_surveil_data/county_w_pop.Rda")
pop_diff = county_w_pop

# load hhs region data
load("~/Dropbox/122cities/reg mats/hhs.Rda")
# load hhs region data with adjacency information
load("~/Dropbox/122cities/reg mats/hhs_adj.Rda")
# load over 65 data, number of providers per FIPS code, reporting rate, and reporting rate during flu season
load("~/Dropbox/122cities/reg mats/per65.Rda")
load("~/Dropbox/122cities/reg mats/reportingFullYear.Rda")
load("~/Dropbox/122cities/reg mats/reportingRateFluSea.Rda")
load("~/Dropbox/122cities/reg mats/providers_per_FIPS.Rda")
load( "~/Dropbox/122cities/reg mats/ave_denom_per_FIPS.Rda")

colnames(y)[1] = 'FIPS_id'
y$FIPS_num = as.numeric(y$FIPS_id)
y = y[order(y$FIPS_num),]

colnames(humidity_diff)[2] = 'FIPS_id'
humidity_diff$FIPS_id = format_fips(humidity_diff$FIPS_id)
humidity_diff$FIPS_num = as.numeric(humidity_diff$FIPS_id)
hum = humidity_diff[order(humidity_diff$FIPS_id),]

colnames(pop_diff)[1] = 'FIPS_id'
pop_diff$FIPS_num = as.numeric(pop_diff$FIPS_id)
pop = pop_diff[order(pop_diff$FIPS_num),]

colnames(hhs)[1] = 'FIPS_id'
hhs$FIPS_id = format_fips(hhs$FIPS_id)
hhs$FIPS_num = as.numeric(hhs$FIPS_id)
hhs$region = as.factor(hhs$region)
hhs = hhs[order(hhs$FIPS_num),]

colnames(hhs_adj)[1] = 'FIPS_id'
hhs_adj$FIPS_id = format_fips(hhs_adj$FIPS_id)
hhs_adj$FIPS_num = as.numeric(hhs_adj$FIPS_id)
for (i in 2:ncol(hhs_adj)){
  hhs_adj[,i] = as.factor(hhs_adj[,i])  
}
hhs_adj = unique(hhs_adj)
hhs = hhs_adj[order(hhs_adj$FIPS_num),]

colnames(per65)[1] = 'FIPS_id'
per65$FIPS_num = as.numeric(as.character(per65$FIPS_id))
per65 = per65[order(per65$FIPS_num),]
per65 = unique(per65)

colnames(reportFluSea)[1] = 'FIPS_id'
reportFluSea$FIPS_num = as.numeric(as.character(reportFluSea$FIPS_id))
reportFluSea = reportFluSea[order(reportFluSea$FIPS_num),]
reportFluSea = unique(reportFluSea)

colnames(reportFullYear)[1] = 'FIPS_id'
reportFullYear$FIPS_num = as.numeric(as.character(reportFullYear$FIPS_id))
reportFullYear = reportFullYear[order(reportFullYear$FIPS_num),]
reportFullYear = unique(reportFullYear)

colnames(providers_per_FIPS)[1] = 'FIPS_id'
providers_per_FIPS$FIPS_num = as.numeric(as.character(providers_per_FIPS$FIPS_id))
providers_per_FIPS = aggregate(. ~ FIPS_id + FIPS_num, data = providers_per_FIPS,  FUN = sum)
providers_per_FIPS = providers_per_FIPS[order(providers_per_FIPS$FIPS_num),]
providers_per_FIPS = providers_per_FIPS[,c('FIPS_id','n_providers','FIPS_num')]

ave_denom_per_FIPS$FIPS_num = as.numeric(as.character(ave_denom_per_FIPS$FIPS_id))
denom = ave_denom_per_FIPS[order(ave_denom_per_FIPS$FIPS_num),]

# run one logistic regression
reg_df = data.frame(y[,3], hum[,1], as.numeric(pop[,2]), per65[,2], hhs[,2],
                    reportFluSea[,2], reportFullYear[,2], providers_per_FIPS[,2],y[,'non_zero_provider'], denom[,'total_pt'])

colnames(reg_df) = c('y','hum','pop', 'per65', 'region', 
                     'seasonal_reporting_rate', 'overall_reporting_rate',
                     'providers_per_FIPS', 'non_zero_provider', 'average_denom')
reg_df = reg_df[reg_df$non_zero_provider == 1,]
fit = glm(formula = y ~ hum + pop + per65 + region + seasonal_reporting_rate + overall_reporting_rate +
            providers_per_FIPS + average_denom, family = binomial(logit),  data = reg_df)
summary(fit)
coefs = as.data.frame(t(coef(summary(fit))[,'Estimate']))
coefs = cbind(as.character(colnames(y)[3]),coefs)
colnames(coefs) = c('city','intercept','hum','pop','per65', 'region_in', 'region_adjacent', 
                    'seasonal_reporting_rate', 'overall_reporting_rate',
                    'providers_per_FIPS','average_denom')
p_vals = as.data.frame(t(coef(summary(fit))[,'Pr(>|z|)']))
p_vals =  cbind(as.character(colnames(y)[3]), p_vals)
colnames(p_vals) = c('city','intercept','hum','pop', 'per65', 'region_in', 'region_adjacent', 
                     'seasonal_reporting_rate', 'overall_reporting_rate',
                     'providers_per_FIPS', 'average_denom')
sd_err = as.data.frame(t(coef(summary(fit))[,'Std. Error']))
sd_err =  cbind(as.character(colnames(y)[3]), sd_err)
colnames(sd_err) = c('city','intercept','hum','pop', 'per65', 'region_in', 'region_adjacent', 
                     'seasonal_reporting_rate', 'overall_reporting_rate',
                     'providers_per_FIPS','average_denom')

for (i in 3:(ncol(y)-3)){
#  print(colnames(y)[(i+1)])
  print(colnames(hhs)[i])
  reg_df = data.frame(y[,(i+1)], as.numeric(hum[,1]), as.numeric(pop[,2]),
                      per65[,2], hhs[,i],
                      reportFluSea[,2], reportFullYear[,2], providers_per_FIPS[,2], y[,'non_zero_provider'], denom[,'total_pt'])
  colnames(reg_df) = c('y','hum','pop', 'per65', 'region',
                       'seasonal_reporting_rate', 'overall_reporting_rate',
                       'providers_per_FIPS', 'non_zero_provider','average_denom')
  reg_df = reg_df[reg_df$non_zero_provider == 1,]
  fit = glm(formula = y ~ hum + pop + per65 + region + seasonal_reporting_rate + overall_reporting_rate +
              providers_per_FIPS + average_denom, family = binomial(logit),  data = reg_df)
  coefs_temp = cbind(as.character(colnames(y)[i+1]),t(coef(summary(fit))[,'Estimate']))
  colnames(coefs_temp) = c('city','intercept','hum','pop', 'per65', 'region_in', 'region_adjacent', 
                           'seasonal_reporting_rate', 'overall_reporting_rate',
                           'providers_per_FIPS', 'average_denom')
  coefs = rbind(coefs, coefs_temp)
  p_vals_temp = cbind(as.character(colnames(y)[i+1]),t(coef(summary(fit))[,'Pr(>|z|)']))
  colnames(p_vals_temp) = c('city','intercept','hum','pop', 'per65', 'region_in', 'region_adjacent', 
                            'seasonal_reporting_rate', 'overall_reporting_rate',
                            'providers_per_FIPS', 'average_denom')
  p_vals = rbind(p_vals,p_vals_temp)
  sd_err_temp = cbind(as.character(colnames(y)[i+1]),t(coef(summary(fit))[,'Std. Error']))
  colnames(sd_err_temp) = c('city','intercept','hum','pop', 'per65', 'region_in', 'region_adjacent', 
                            'seasonal_reporting_rate', 'overall_reporting_rate',
                            'providers_per_FIPS','average_denom')
  sd_err = rbind(sd_err, sd_err_temp)
  print(summary(fit))
}

p_vals_region = p_vals

save(p_vals, file = '~/flu_surveil_data/logistic_data/p_vals.Rda')
save(sd_err, file = '~/flu_surveil_data/logistic_data/sd_err.Rda')
save(coefs, file = '~/flu_surveil_data/logistic_data/coefs.Rda')

# make a heatmap of p-values
mat_data <- data.matrix(p_vals[,2:ncol(p_vals)])
#rnames <- p_vals[,1]
rnames <- c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5", "Region 6", "Region 7", "Region 8", "Region 9", "Region 10")
rownames(mat_data) <- rnames
colnames(mat_data) <- c('Intercept','Absolute Humidity', 'Population',
                       'Percentage of Patients \n of 65', 'Provider County in \n Region', 
                       'Provider County in \n Adjacent Region', 'Overall Reporting Rate',
                       'Flu Season \n Reporting Rate',
                       'Providers Per County', 'Average Patients \n Seen Per Week')

my_palette <- colorRampPalette(c("red", "orange","yellow","green","white"))(n = 499)
col_breaks = c(seq(0,.05,length=100), 
               seq(.05,.1,length=100), # for red
               seq(.1,.14,length=100),              # for yellow
               seq(.14,.2,length=100),
               seq(.2,1,length=100))

mat_data_r = round(mat_data, digits = 1)
#pdf(file = "~/flu_surveil_data/lag_data/heatmap_p_vals_mode.pdf", width = 10, height = 10)
heatmap.2(mat_data,
          cellnote = mat_data_r,  # same data set for cell labels
          #main = "P-values for logistic regression \n where response variable is whether \n county was picked in optimization", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,12),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Colv="NA",
          xlab = 'predictors',
          ylab = 'regions')            # turn off column clustering
#dev.off()
