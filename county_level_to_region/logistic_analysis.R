library( aod)
library(ggplot2)

source('~/repos/flu_surveil/logistic/format_fips.R')

setwd("~/Dropbox/122citites")
# load y data
#load("~/Dropbox/122citites/reg mats/y.Rda")
load("~/flu_surveil_data/y.Rda")
# load humidity difference date
#load("~/Dropbox/122citites/reg mats/humidity_diff.Rda")
load("~/flu_surveil_data/counties_hum_fips.Rda")
#humidity_diff = unique(humidity_diff)
humidity_diff = counties_hum_fips

# load population difference data
#load("~/Dropbox/122citites/reg mats/pop_diff.Rda")
load("~/flu_surveil_data/county_w_pop.Rda")
pop_diff = county_w_pop

# load geographic difference data
load("~/Dropbox/122citites/reg mats/northSouthDist.Rda")
# load geographic difference data
load("~/Dropbox/122citites/reg mats/eastWestDist.Rda")
# load geographic difference data
load("~/Dropbox/122citites/reg mats/greatCircleDist.Rda")
# load over 65 data, number of providers per FIPS code, reporting rate, and reporting rate during flu season
load("~/Dropbox/122citites/reg mats/per65.Rda")
load("~/Dropbox/122citites/reg mats/reportingFullYear.Rda")
load("~/Dropbox/122citites/reg mats/reportingRateFluSea.Rda")
load("~/Dropbox/122citites/reg mats/providers_per_FIPS.Rda")

colnames(y)[1] = 'FIPS_id'
#s = colnames(y)[2:123]
#s = s[order(s)]
#s = s[s != 'national']
#y = y[,c('FIPS_id',s)]
y$FIPS_num = as.numeric(y$FIPS_id)
y = y[order(y$FIPS_num),]

colnames(humidity_diff)[2] = 'FIPS_id'
humidity_diff$FIPS_id = format_fips(humidity_diff$FIPS_id)
#s = colnames(humidity_diff)[2:123]
#s = s[order(s)]
#s = s[s != "New_Orleans_LA"]
#humidity_diff = humidity_diff[,c('FIPS_id',s)]
humidity_diff$FIPS_num = as.numeric(humidity_diff$FIPS_id)
hum = humidity_diff[order(humidity_diff$FIPS_id),]

#s = colnames(pop_diff)[2:123]
#s[s == 'Urban_Honolulu_HI'] = 'Honolulu_HI'
#s[s == 'Fort_Worth_TX'] = 'Ft._Worth_TX'
#s[s == 'Colorado_Springs_CO'] = 'Colo._Springs_CO'
#s[s == 'New_York_NY'] = 'New_York_City_NY'
#s[s == 'Boise_City_ID'] = 'Boise_ID'

#colnames(pop_diff)[2:123] = s
#s = s[order(s)]
#s = s[s != "New_Orleans_LA"]
colnames(pop_diff)[1] = 'FIPS_id'
#pop_diff = pop_diff[,c('FIPS_id',s)]
pop_diff$FIPS_num = as.numeric(pop_diff$FIPS_id)
pop = pop_diff[order(pop_diff$FIPS_num),]

#s = colnames(EW)[2:123]
#s = s[order(s)]
#s = s[s != "New_Orleans_LA"]
#EW = EW[,c('FIPS_id',s)]
#EW_df = as.data.frame(EW, stringsAsFactors = FALSE)
#EW_df$FIPS_num = as.numeric(EW_df$FIPS_id)
#EW = EW_df[order(EW_df$FIPS_num),]
#EW = unique(EW)

#s = colnames(NS)[2:123]
#s = s[order(s)]
#s = s[s != "New_Orleans_LA"]
#NS = NS[,c('FIPS_id',s)]
#NS_df = as.data.frame(NS, stringsAsFactors = FALSE)
#NS_df$FIPS_num = as.numeric(NS_df$FIPS_id)
#NS = NS_df[order(NS_df$FIPS_num),]
#NS = unique(NS)

#s = colnames(GD)[2:123]
#s = s[order(s)]
#s = s[s != "New_Orleans_LA"]
#GD = GD[,c('FIPS_id',s)]
#GD_df = as.data.frame(GD, stringsAsFactors = FALSE)
#GD_df$FIPS_num = as.numeric(GD_df$FIPS_id)
#GD = GD_df[order(GD_df$FIPS_num),]
#GD = unique(GD)

colnames(per65)[1] = 'FIPS_id'
per65$FIPS_num = as.numeric(as.character(per65$FIPS_id))
per65 = per65[order(per65$FIPS_num),]
#per65$test = per65$perOver65Reported
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


# run one logistic regression
reg_df = data.frame(y[,2], hum[,1], as.numeric(pop[,2]), #as.numeric(NS[,2]), as.numeric(EW[,2]), as.numeric(GD[,2]), 
                    per65[,2],
                   reportFluSea[,2], reportFullYear[,2], providers_per_FIPS[,2])
colnames(reg_df) = c('y','hum','pop',#'ns','ew', 'gd', 
                     'per65',
                     'seasonal_reporting_rate', 'overall_reporting_rate',
                     'providers_per_FIPS')
fit = glm(formula = y ~ hum + pop + #ns + ew + gd 
            + per65 + 
            seasonal_reporting_rate + overall_reporting_rate +
                    providers_per_FIPS, family = binomial(logit),  data = reg_df)

coefs = as.data.frame(t(coef(summary(fit))[,'Estimate']))
coefs = cbind(as.character(colnames(y)[2]),coefs)
colnames(coefs) = c('city','intercept','hum','pop','ns','ew', 'gd','per65',
                    'seasonal_reporting_rate', 'overall_reporting_rate',
                    'providers_per_FIPS')
p_vals = as.data.frame(t(coef(summary(fit))[,'Pr(>|z|)']))
p_vals =  cbind(as.character(colnames(y)[2]), p_vals)
colnames(p_vals) = c('city','intercept','hum','pop','ns','ew', 'gd', 'per65',
                     'seasonal_reporting_rate', 'overall_reporting_rate',
                     'providers_per_FIPS')
sd_err = as.data.frame(t(coef(summary(fit))[,'Std. Error']))
sd_err =  cbind(as.character(colnames(y)[2]), sd_err)
colnames(sd_err) = c('city','intercept','hum','pop','ns','ew', 'gd','per65',
                     'seasonal_reporting_rate', 'overall_reporting_rate',
                     'providers_per_FIPS')

for (i in 3:(ncol(y)-1)){
  reg_df = data.frame(y[,i], hum[,i], pop[,i], as.numeric(NS[,i]), as.numeric(EW[,i]),as.numeric(GD[,i]),
                      per65[,2], 
                      reportFluSea[,2], reportFullYear[,2], providers_per_FIPS[,2])
  colnames(reg_df) = c('y','hum','pop','ns','ew', 'gd', 'per65',
                       'seasonal_reporting_rate', 'overall_reporting_rate',
                       'providers_per_FIPS')
  fit = glm(formula = y ~ hum + pop + ns + ew + gd + per65 
              + seasonal_reporting_rate + overall_reporting_rate +
              providers_per_FIPS, family = binomial(logit),  data = reg_df)
  coefs_temp = cbind(as.character(colnames(y)[i]),t(coef(summary(fit))[,'Estimate']))
  colnames(coefs_temp) = c('city','intercept','hum','pop','ns','ew', 'gd', 'per65',
                           'seasonal_reporting_rate', 'overall_reporting_rate',
                           'providers_per_FIPS')
  coefs = rbind(coefs, coefs_temp)
  p_vals_temp = cbind(as.character(colnames(y)[i]),t(coef(summary(fit))[,'Pr(>|z|)']))
  colnames(p_vals_temp) = c('city','intercept','hum','pop','ns','ew', 'gd', 'per65',
                            'seasonal_reporting_rate', 'overall_reporting_rate',
                            'providers_per_FIPS')
  p_vals = rbind(p_vals,p_vals_temp)
  sd_err_temp = cbind(as.character(colnames(y)[i]),t(coef(summary(fit))[,'Std. Error']))
  colnames(sd_err_temp) = c('city','intercept','hum','pop','ns','ew', 'gd', 'per65',
                            'seasonal_reporting_rate', 'overall_reporting_rate',
                            'providers_per_FIPS')
  sd_err = rbind(sd_err, sd_err_temp)
}

save(p_vals, file = '~/flu_surveil_data/logistic_data/p_vals.Rda')
save(sd_err, file = '~/flu_surveil_data/logistic_data/sd_err.Rda')
save(coefs, file = '~/flu_surveil_data/logistic_data/coefs.Rda')

pdf(file = "hist_p_vals.pdf")
## set up the new plotting device (pdf)
par(mfrow = c(2,2))
## draw the plot
hist(as.numeric(p_vals$pop), main = "P-Values for Population", xlab = "p-value")
hist(as.numeric(p_vals$hum), main = "P-Values for Humidity", xlab = "p-value")
hist(as.numeric(p_vals$ns), main = "P-Values for North South Distance", xlab = "p-value")
hist(as.numeric(p_vals$ew), main = "P-Values for East West Distance", xlab = "p-value")
hist(as.numeric(p_vals$gd), main = "P-Values for Linear Distance", xlab = "p-value")
hist(as.numeric(p_vals$per65), main = "P-Values for Over 65", xlab = "p-value")
hist(as.numeric(p_vals$overall_reporting_rate), main = "P-Values for Reporting Rate Overall",xlab = "p-value")
hist(as.numeric(p_vals$seasonal_reporting_rate), main = "P-Values for Reporting Rate During Flu Season",xlab = "p-value")
hist(as.numeric(p_vals$providers_per_FIPS), main = "P-Values for Number of Providers per County",xlab = "p-value")
## close the device to do the drawing
dev.off()

