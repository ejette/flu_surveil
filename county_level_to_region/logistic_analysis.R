library(aod)
library(ggplot2)
library(RColorBrewer)
library(gplots)

source('~/repos/flu_surveil/logistic/format_fips.R')

setwd("~/Dropbox/122cities")
# load y data
#load("~/flu_surveil_data/y.Rda")
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

# run one logistic regression
reg_df = data.frame(y[,2], hum[,1], as.numeric(pop[,2]), per65[,2], hhs[,'region'],
                   reportFluSea[,2], reportFullYear[,2], providers_per_FIPS[,2])
colnames(reg_df) = c('y','hum','pop', 'per65', 'region', 
                     'seasonal_reporting_rate', 'overall_reporting_rate',
                     'providers_per_FIPS')
fit = glm(formula = y ~ hum + pop + per65 + region + seasonal_reporting_rate + overall_reporting_rate +
                    providers_per_FIPS, family = binomial(logit),  data = reg_df)

coefs = as.data.frame(t(coef(summary(fit))[,'Estimate']))
coefs = cbind(as.character(colnames(y)[2]),coefs)
colnames(coefs) = c('city','intercept','hum','pop','per65', 'region2', 'region3', 'region4', 'region5', 'region6', 'region7', 'region8', 'region9', 'region10',
                    'seasonal_reporting_rate', 'overall_reporting_rate',
                    'providers_per_FIPS')
p_vals = as.data.frame(t(coef(summary(fit))[,'Pr(>|z|)']))
p_vals =  cbind(as.character(colnames(y)[2]), p_vals)
colnames(p_vals) = c('city','intercept','hum','pop', 'per65', 'region2', 'region3', 'region4', 'region5', 'region6', 'region7', 'region8', 'region9', 'region10',
                     'seasonal_reporting_rate', 'overall_reporting_rate',
                     'providers_per_FIPS')
sd_err = as.data.frame(t(coef(summary(fit))[,'Std. Error']))
sd_err =  cbind(as.character(colnames(y)[2]), sd_err)
colnames(sd_err) = c('city','intercept','hum','pop', 'per65', 'region2', 'region3', 'region4', 'region5', 'region6', 'region7', 'region8', 'region9', 'region10',
                    'seasonal_reporting_rate', 'overall_reporting_rate',
                    'providers_per_FIPS')

for (i in 3:(ncol(y)-1)){
  reg_df = data.frame(y[,i], as.numeric(hum[,2]), as.numeric(pop[,2]),
                      per65[,2], hhs[,'region'],
                      reportFluSea[,2], reportFullYear[,2], providers_per_FIPS[,2])
  colnames(reg_df) = c('y','hum','pop', 'per65', 'region',
                       'seasonal_reporting_rate', 'overall_reporting_rate',
                       'providers_per_FIPS')
  fit = glm(formula = y ~ hum + pop + per65 + region + seasonal_reporting_rate + overall_reporting_rate +
              providers_per_FIPS, family = binomial(logit),  data = reg_df)
  coefs_temp = cbind(as.character(colnames(y)[i]),t(coef(summary(fit))[,'Estimate']))
  colnames(coefs_temp) = c('city','intercept','hum','pop', 'per65', 'region2', 'region3', 'region4', 'region5', 'region6', 'region7', 'region8', 'region9', 'region10',
                           'seasonal_reporting_rate', 'overall_reporting_rate',
                           'providers_per_FIPS')
  coefs = rbind(coefs, coefs_temp)
  p_vals_temp = cbind(as.character(colnames(y)[i]),t(coef(summary(fit))[,'Pr(>|z|)']))
  colnames(p_vals_temp) = c('city','intercept','hum','pop', 'per65', 'region2', 'region3', 'region4', 'region5', 'region6', 'region7', 'region8', 'region9', 'region10',
                            'seasonal_reporting_rate', 'overall_reporting_rate',
                            'providers_per_FIPS')
  p_vals = rbind(p_vals,p_vals_temp)
  sd_err_temp = cbind(as.character(colnames(y)[i]),t(coef(summary(fit))[,'Std. Error']))
  colnames(sd_err_temp) = c('city','intercept','hum','pop', 'per65', 'region2', 'region3', 'region4', 'region5', 'region6', 'region7', 'region8', 'region9', 'region10',
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
hist(as.numeric(p_vals$pop), main = "P-Values for Population", xlab = "p-value", breaks = 20)
hist(as.numeric(p_vals$hum), main = "P-Values for Humidity", xlab = "p-value")
hist(as.numeric(p_vals$region1), main = "P-Values for region1 ('MA','CT','RI','ME','NH','VT')", xlab = "p-value", breaks = 20)
hist(as.numeric(p_vals$ew), main = "P-Values for East West Distance", xlab = "p-value")
hist(as.numeric(p_vals$gd), main = "P-Values for Linear Distance", xlab = "p-value")
hist(as.numeric(p_vals$per65), main = "P-Values for Over 65", xlab = "p-value")
hist(as.numeric(p_vals$overall_reporting_rate), main = "P-Values for Reporting Rate Overall",xlab = "p-value")
hist(as.numeric(p_vals$seasonal_reporting_rate), main = "P-Values for Reporting Rate During Flu Season",xlab = "p-value")
hist(as.numeric(p_vals$providers_per_FIPS), main = "P-Values for Number of Providers per County",xlab = "p-value")
## close the device to do the drawing
dev.off()

# make a heatmap of p-values
mat_data <- data.matrix(p_vals[,2:ncol(p_vals)])
rnames <- p_vals[,1]
rownames(mat_data) <- rnames

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
col_breaks = c(seq(-1,0,length=100),  # for red
               seq(0,0.8,length=100),              # for yellow
               seq(0.8,1,length=100))

mat_data_r = round(mat_data, digits = 2)
pdf(file = "heatmap_p_vals.pdf", width = 10, height = 10)
heatmap.2(mat_data,
          cellnote = mat_data_r,  # same data set for cell labels
          main = "p-values for logistic with y = county_picked", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Colv="NA",
          xlab = 'predictors',
          ylab = 'regions')            # turn off column clustering
dev.off()
