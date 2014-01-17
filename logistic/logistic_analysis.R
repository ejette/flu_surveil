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


#pop_diff$FIPS_id = as.numeric(as.character(pop_diff$FIPS_id))

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

# run one logistic regression
# save p-values, coefficients
