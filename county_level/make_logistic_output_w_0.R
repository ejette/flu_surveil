library(plyr)
#source("~/flu_surveil/county_level/format_fips.R")
source("~/repos/flu_surveil/county_level/format_fips.R")


# load all possible FIPS code and extract FIPS codes
load("~/flu_surveil_data/county_FIPS_xwalk.Rda")
FIPS = unique(format_fips(county_FIPS_xwalk$FIPS_code))

# read in optimization results
#ranks <- read.csv("~/flu_surveil_data/covars_cnty_w_zeros.csv", colClasses = c("numeric", rep("character",122)))
ranks <- read.csv("~/flu_surveil_data/covars_cnty_to_region_w_zeros.csv", colClasses = c("numeric", rep("character",11)))
colnames(ranks)[1] = 'rank'

# find the counties that appeared in the top 50 providers of the first optimization (this was the national level)
x = ranks[,2]
# assign a 1 if the county was chosen and a 0 if the county was not chosen as a provider
y = as.data.frame(as.numeric(FIPS %in% x))
colnames(y) = colnames(ranks)[2]

# repeat for the rest of the 122 cities
for (i in 3:ncol(ranks)){
  new_cols = c(colnames(y),colnames(ranks)[i])
  y = cbind(y,as.numeric(FIPS %in% ranks[,i]))
  print(sum(FIPS %in% ranks[,i]))
  colnames(y) = new_cols
}

y = cbind(FIPS,y)
colnames(y)[1] = 'FIPS_code'

save(y, file = '~/flu_surveil_data/y.Rda')
