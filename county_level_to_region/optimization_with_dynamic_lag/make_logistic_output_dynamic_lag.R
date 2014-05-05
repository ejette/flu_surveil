library(plyr)
#source("~/flu_surveil/county_level/format_fips.R")
source("~/repos/flu_surveil/county_level/format_fips.R")

load("~/Dropbox/122cities/ili_reg.Rda")
non_zero_counties = colnames(ili_reg[colSums(ili_reg) > 0])[-1]
non_zero_counties = gsub('total.','',non_zero_counties)
save(non_zero_counties, file = '~/flu_surveil_data/non_zero_counties.Rda')

# load all possible FIPS code and extract FIPS codes
load("~/flu_surveil_data/county_FIPS_xwalk.Rda")
FIPS = unique(format_fips(county_FIPS_xwalk$FIPS_code))
#FIPS = FIPS[FIPS %in% non_zero_counties]

# read in optimization results
#ranks <- read.csv("~/flu_surveil_data/covars_cnty_w_zeros.csv", colClasses = c("numeric", rep("character",122)))
#ranks <- read.csv("~/flu_surveil_data/covars_cnty_to_region_w_zeros.csv", colClasses = c("numeric", rep("character",11)))
#ranks = ranks_no_season_max
colnames(ranks)[1] = 'rank'
# find the counties that appeared in the top 50 providers of the first optimization (this was the national level)
x = ranks[,2]
x = substr(x,1,5)
# assign a 1 if the county was chosen and a 0 if the county was not chosen as a provider
y = as.data.frame( as.numeric(FIPS %in% x))
region = strsplit(colnames(ranks)[2], split = "_")[[1]][1]
region = paste("deaths.",region)
colnames(y) = region

# repeat for the rest of the regions
for (i in 3:ncol(ranks)){
  region = strsplit(colnames(ranks)[i], split = "_")[[1]][1]
  region = gsub('region','deaths.',region)
  new_cols = c(colnames(y),region)
  x = ranks[,i]
  x = substr(x,1,5)
  print(x)
  y = cbind(y,as.numeric(FIPS %in% x))
  print(sum(FIPS %in% ranks[,i]))
  colnames(y) = new_cols
}

y = cbind(FIPS, y)
y$non_zero_provider = 0
y$non_zero_provider[y$FIPS %in% non_zero_counties] = 1
y_lag = y
colnames(y_lag)[1] = 'FIPS_code'
y_lag_dynamic = y_lag
save(y_lag_dynamic, file = '~/flu_surveil_data/lag_data/y_lag_dynamic.Rda')

