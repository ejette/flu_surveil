library(aod)
library(ggplot2)

source('~/flu_surveil/logistic/format_fips.R')

setwd("~/Dropbox/122citites")
# load y data
load("~/Dropbox/122citites/reg mats/y.Rda")
# load humidity difference date
load("~/Dropbox/122citites/reg mats/humidity_diff.Rda")
# load population difference data
load("~/Dropbox/122citites/reg mats/pop_diff.Rda")
# load geographic difference data
load("~/Dropbox/122citites/reg mats/northSouthDist.Rda")
# load geographic difference data
load("~/Dropbox/122citites/reg mats/eastWestDist.Rda")
# load geographic difference data
load("~/Dropbox/122citites/reg mats/greatCircleDist.Rda")

#y$FIPS_code = format_fips(as.character(y$FIPS_code))
colnames(y)[1] = 'FIPS_id'
y$FIPS_num = as.numeric(y$FIPS_id)
y = y[order(y$FIPS_num),]

humidity_diff$FIPS_num = as.numeric(humidity_diff$FIPS_id)
hum = humidity_diff[order(humidity_diff$FIPS_id),]

pop_diff$FIPS_num = as.numeric(pop_diff$FIPS_id)
pop = pop_diff[order(pop_diff$FIPS_num),]

EW_df = as.data.frame(EW, stringsAsFactors = FALSE)
EW_df$FIPS_num = as.numeric(EW_df$FIPS_id)
EW = EW_df[order(EW_df$FIPS_num),]

NS_df = as.data.frame(NS, stringsAsFactors = FALSE)
NS_df$FIPS_num = as.numeric(NS_df$FIPS_id)
NS = NS_df[order(NS_df$FIPS_num),]

GD_df = as.data.frame(GD, stringsAsFactors = FALSE)
GD_df$FIPS_num = as.numeric(GD_df$FIPS_id)
GD = GD_df[order(GD_df$FIPS_num),]

# run one logistic regression
reg_df = data.frame(y[,2], hum[,2], pop[,2], as.numeric(NS[,2]), as.numeric(EW[,2]), as.numeric(GD[,2]))
colnames(reg_df) = c('y','hum','pop','ns','ew', 'gd')
fit = glm(formula = y ~ hum + pop + ns + ew + gd, family = binomial(logit),  data = reg_df)

coefs = as.data.frame(t(coef(summary(fit))[,'Estimate']))
coefs = cbind(as.character(colnames(y)[2]),coefs)
colnames(coefs) = c('city','intercept','hum','pop','ns','ew','gd')
p_vals = as.data.frame(t(coef(summary(fit))[,'Pr(>|z|)']))
p_vals =  cbind(as.character(colnames(y)[2]), p_vals)
colnames(p_vals) = c('city','intercept','hum','pop','ns','ew','gd')
sd_err = as.data.frame(t(coef(summary(fit))[,'Std. Error']))
sd_err =  cbind(as.character(colnames(y)[2]), sd_err)
colnames(sd_err) = c('city','intercept','hum','pop','ns','ew', 'gd')

for (i in 3:(ncol(y)-1)){
  reg_df = data.frame(y[,i], hum[,i], pop[,i], as.numeric(NS[,i]), as.numeric(EW[,i]),as.numeric(GD[,2]))
  colnames(reg_df) = c('y','hum','pop','ns','ew','gd')
  fit = glm(formula = y ~ hum + pop + ns + ew + gd, family = binomial(logit),  data = reg_df)
  coefs_temp = cbind(as.character(colnames(y)[i]),t(coef(summary(fit))[,'Estimate']))
  colnames(coefs_temp) = c('city','intercept','hum','pop','ns','ew','gd')
  coefs = rbind(coefs, coefs_temp)
  p_vals_temp = cbind(as.character(colnames(y)[i]),t(coef(summary(fit))[,'Pr(>|z|)']))
  colnames(p_vals_temp) = c('city','intercept','hum','pop','ns','ew','gd')
  p_vals = rbind(p_vals,p_vals_temp)
  sd_err_temp = cbind(as.character(colnames(y)[i]),t(coef(summary(fit))[,'Std. Error']))
  colnames(sd_err_temp) = c('city','intercept','hum','pop','ns','ew','gd')
  sd_err = rbind(sd_err, sd_err_temp)
}

save(p_vals, file = '~/flu_surveil_data/logistic_data/p_vals.Rda')
save(sd_err, file = '~/flu_surveil_data/logistic_data/sd_err.Rda')
save(coefs, file = '~/flu_surveil_data/logistic_data/coefs.Rda')

pdf(file = "hist_p_vals.pdf")
## set up the new plotting device (pdf)
par(mfrow = c(3,2))
## draw the plot
hist(as.numeric(p_vals$pop), main = "P-Values for Population")
hist(as.numeric(p_vals$hum), main = "P-Values for Humidity")
hist(as.numeric(p_vals$ns), main = "P-Values for North South Distance")
hist(as.numeric(p_vals$ew), main = "P-Values for East West Distance")
hist(as.numeric(p_vals$gd), main = "P-Values for Linear Distance")
## close the device to do the drawing
dev.off()

