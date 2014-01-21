library(reshape)
library(plyr)
library(stringr)

source('~/flu_surveil/logistic/format_fips.R')

ili <- read.csv("~/Downloads/ProviderILI.txt")
setwd("~/flu_surveil_data")

load("~/flu_surveil_data/county_FIPS_xwalk.Rda")
mmwr <- read.csv("mmwr.csv")

# create a total category in the ILI network data
attach(ili)
ili$cases <- rowSums(ili[,c('Age_0_4','Age_5_24','Age_25_64',
                            'Age_25_49','Age_50_64','Age_65_and_older')], na.rm = TRUE)
detach(ili)

ili = ili[order(ili$Phys_ID_Code, ili$datecode),]
ili$county_temp = tolower(str_trim((as.character(ili$County))))

ili = ili[order(ili$Phys_ID_Code, ili$datecode),]
ili$county_temp = tolower(str_trim((as.character(ili$County))))
ili$county_temp = gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", ili$county_temp, perl=TRUE)
ili$county_temp = gsub("\\.","",ili$county_temp)
ili$county_temp = gsub("St ","St\\. ",ili$county_temp)
#ili$county_temp = gsub("St\\.","Saint",ili$county_temp)
ili$county_temp = gsub("-"," ",ili$county_temp)
ili$county_temp[ili$County == ''] = 'New York'
ili$St = str_trim(ili$St)
ili$county_temp = str_trim(ili$county_temp)
# make counties more uniform
# replace periods with an empty string
ili$county_temp[ili$county_temp == 'Virginia Beach'] = 'Virginia Beach City'
ili$county_temp[ili$county_temp == 'Concoridia'] = 'Concordia'
ili$county_temp[ili$county_temp == 'Newport News'] = 'Newport News City'
ili$county_temp[ili$county_temp == 'Frederickburg City'] = 'Fredericksburg City'
ili$county_temp[ili$county_temp == 'Chesapeake'] = 'Chesapeake City'
ili$county_temp[ili$county_temp == 'Fairfax City'] = 'Fairfax'
ili$county_temp[ili$county_temp == 'Fairfiled'] = 'Fairfield'
ili$county_temp[ili$county_temp == 'Dekalb' & ili$St == 'AL'] = 'De Kalb'
ili$county_temp[ili$county_temp == 'Dekab' & ili$St == 'AL'] = 'De Kalb'
ili$county_temp[ili$county_temp == 'Lorian' & ili$St == 'OH'] = 'Lorain'
ili$county_temp[which(ili$county_temp == 'Cordova' & ili$St == 'AK')] = 'Valdez Cordova'
ili$county_temp[ili$county_temp == 'Miami Dade'] = 'Miami-Dade'
ili$county_temp[ili$county_temp == 'Cleveland' & ili$St == 'MS'] = 'Bolivar'
ili$county_temp[ili$county_temp == 'Dekalb' & ili$St == 'IL'] = 'De Kalb'
ili$county_temp[ili$county_temp == 'Dekalb' & ili$St == 'IN'] = 'De Kalb'
ili$county_temp[ili$county_temp == 'Dupage' & ili$St == 'IL'] = 'Du Page'
ili$county_temp[ili$county_temp == 'Lasalle' & ili$St == 'IN'] = 'La Salle'
ili$county_temp[ili$county_temp == 'Lasalle' & ili$St == 'IL'] = 'La Salle'
ili$county_temp[ili$county_temp == 'Desoto' & ili$St == 'MS'] = 'De Soto'
ili$county_temp[ili$county_temp == 'Desoto' & ili$St == 'FL'] = 'De Soto'
ili$county_temp[ili$county_temp == 'Ostego' & ili$St == 'NY'] = 'Otsego'
ili$county_temp[ili$county_temp == 'Dewitt' & ili$St == 'TX'] = 'De Witt'
ili$county_temp[ili$county_temp == 'Pleasant' & ili$St == 'WV'] = 'Pleasants'
ili$county_temp[ili$county_temp == 'Radford City' & ili$St == 'VA'] = 'Radford'
ili$county_temp[ili$county_temp == 'Salem City' & ili$St == 'VA'] = 'Salem'
ili$county_temp[ili$county_temp == 'Harrisonburg' & ili$St == 'VA'] = 'Harrisonburg City'
ili$county_temp[ili$county_temp == 'Harden' & ili$St == 'TX'] = 'Hardin'

ili$county = str_trim(ili$county_temp)
ili$state = str_trim(ili$St)

m = merge(ili, county_FIPS_xwalk, by = c('county','state'))
m$FIPS_id = format_fips(m$FIPS_code)
ili_with_FIPS = m[,-c(18:19)]

save(ili_with_FIPS, file = 'ili_with_FIPS.Rda')

ili_cnty = ili_with_FIPS
#save(ili_cnty, file = 'ili_cnty.Rda')

ili_trim_cnty_FIPS <- ili_cnty[,c('datecode','county','state','cases','totalpt','Age_65_and_older', 'FIPS_id')]
colnames(ili_trim_cnty_FIPS) <- c('date','county','state','cases','total_pt','age_65_and_older', 'FIPS_id')
save(ili_trim_cnty_FIPS, file = '~/Dropbox/122citites/ili_trim_cnty_FIPS.Rda')

# calculate the number of providers per FIPS code
provider_FIPS_cnt = count(ili_with_FIPS[,c('county','state','Phys_ID_Code','FIPS_id')])
providers_per_FIPS = count(provider_FIPS_cnt[,c('county','state','FIPS_id')])
providers_per_FIPS = providers_per_FIPS[,c('FIPS_id','freq')]
colnames(providers_per_FIPS) = c('FIPS_id','n_providers')
save(providers_per_FIPS, file = 'providers_per_FIPS.Rda')
