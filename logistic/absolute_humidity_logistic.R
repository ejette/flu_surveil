library(plyr)
library(stringr)

# import datelookup table
source('~/flu_sandbox/datelookup.R')
source('~/flu_surveil/logistic/format_fips.R')


################
# weather data #
################

# raw bulb temp and relative humidity indexed by wban
#ncdc_raw <- read.csv('/Users/jj/flu_surveil_data/humidity/all_humidity_agg.csv', sep = ' ')
load('/Users/jj/flu_surveil_data/humidity/all_humidity_agg.Rda')

ncdc_raw = all_humidity_agg
# compute absolute humidity from raw bulb temp and relative humidity using method from:
# McDevitt et al., "Role of Absolute Humidity in the Inactivation of Inﬂuenza Viruses on Stainless Steel Surfaces at Elevated Temperatures,"
# APPLIED AND ENVIRONMENTAL MICROBIOLOGY, June 2010.
ncdc_raw$temp_kelvin <- 273.15 + (ncdc_raw$dry_bulb_temp - 32)/1.8
ncdc_raw$sat_vapor_pres <- exp(-5800/ncdc_raw$temp_kelvin + 1.391 - 0.04864*ncdc_raw$temp_kelvin + 4.176e-5*(ncdc_raw$temp_kelvin)^2 - 1.445e-8*(ncdc_raw$temp_kelvin)^3 + 6.456*log(ncdc_raw$temp_kelvin))
ncdc_raw$absolute_humidity <- 0.00217*ncdc_raw$sat_vapor_pres*ncdc_raw$relative_humidity/ncdc_raw$temp_kelvin

# convert dates to CDC week format
flu_weeks <- read.csv("~/cdc_predict/flu_weeks.csv", header=F)
colnames(flu_weeks) = c('year_week','date_raw')
ncdc_raw$date_raw = ncdc_raw$date
ncdc_raw$date <- as.Date(as.character(ncdc_raw$date),format='%Y%m%d')
ncdc_raw <- merge(ncdc_raw, dateWeekTable, by = 'date')
#ncdc_raw1 <- merge(ncdc_raw2, flu_weeks, by = 'date_raw')
#old way using sapply (too slow)
#ncdc_raw$CDCdate <- sapply(ncdc_raw$date, dailyToWeek)

# average weekly absolute humidity by wban
#ncdc <- ncdc_raw[,c('state','wban','year_week','absolute_humidity')]
ncdc <- ncdc_raw[,c('state','wban','CDCdate','absolute_humidity')]

ncdc$date_num = as.numeric(as.character(ncdc$CDCdate))

#get the subset of the data that is in the 2008 - 2012 flu seasons
seasons = ncdc[ncdc$date_num >= 200840 & ncdc$date_num <= 201239, c('state','absolute_humidity') ]

#weeklyhum_wban = aggregate(. ~ state + wban + year_week, data = ncdc, FUN = mean)
hum_state_ave = aggregate(. ~ state, data = seasons, FUN = mean)
#weeklyhum_state = aggregate(. ~ state + CDCdate, data = weeklyhum_wban[,-2], FUN = mean)

#weeklyhum_state = aggregate(. ~ state + year_week, data = weeklyhum_wban[,-2], FUN = mean)
# merge in counties
load('/Users/jj/flu_surveil_data/counties.Rda')
load('/Users/jj/flu_surveil_data/cities.Rda')
load('/Users/jj/flu_surveil_data/county_FIPS_xwalk.Rda')

counties_hum = merge(hum_state_ave, counties, by = 'state')
counties_hum_fips = merge(counties_hum, county_FIPS_xwalk, by = c('county','state'))
cities_hum = merge(hum_state_ave, cities, by = 'state')

# calculate the humidity difference between the counties and the cities
mat1 = as.data.frame(rep(counties_hum_fips[1,'absolute_humidity'],122))
colnames(mat1) = counties_hum_fips[1,'FIPS_code']
mat2 = as.data.frame(rep(cities_hum[1,'absolute_humidity'],nrow(counties_hum_fips)))
colnames(mat2) = paste(gsub(' ','_',cities_hum[1,'city']),'_',cities_hum[1,'state'], sep = '')

for (i in 2:nrow(counties_hum)){
  new_col = c(colnames(mat1),counties_hum_fips[i,'FIPS_code'] )
  mat1 = cbind(mat1, rep(counties_hum_fips[i,'absolute_humidity'],122))
   colnames(mat1) = new_col
}

for (i in 2:nrow(cities_hum)){
  new_col = c(colnames(mat2), paste(gsub(' ','_',cities_hum[i,'city']),'_',cities_hum[i,'state'], sep = ''))
  mat2 = cbind(mat2, rep(cities_hum[i,'absolute_humidity'],nrow(counties_hum_fips)))
  colnames(mat2) = new_col
}

mat1_transpose = t(as.matrix(mat1))
mat2_matrix = as.matrix(mat2)

humidity_diff = as.data.frame(mat2_matrix - mat1_transpose)
colnames(humidity_diff) = colnames(mat2)
humidity_diff = cbind(format_fips(colnames(mat1)), humidity_diff)
colnames(humidity_diff)[1] = 'FIPS_id'
humidity_diff$FIPS_id = as.character(humidity_diff$FIPS_id)
save(humidity_diff, file = '/Users/jj/flu_surveil_data/logistic_data/humidity_diff.Rda')
