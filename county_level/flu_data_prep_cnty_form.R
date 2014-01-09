library(reshape)
library(plyr)
library(stringr)

ili <- read.csv("~/Downloads/ProviderILI.txt")
setwd("~/flu_surveil_data")

mmwr <- read.csv("mmwr.csv")

# create a total category in the ILI network data
attach(ili)
ili$cases <- rowSums(ili[,c('Age_0_4','Age_5_24','Age_25_64',
                            'Age_25_49','Age_50_64','Age_65_and_older')], na.rm = TRUE)
detach(ili)

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
counties = count(ili[,c('county', 'state')])
write.csv(counties[,1], file = 'counties.csv')

ili_cnty = ili
#save(ili_cnty, file = 'ili_cnty.Rda')

ili_trim_cnty <- ili_cnty[,c('datecode','county','cases','totalpt')]
colnames(ili_trim_cnty) <- c('date','county','cases','total_pt')

# aggregate to county level
ili_trim_cnty = ili_trim_cnty[order(ili_trim_cnty$county,ili_trim_cnty$date),]
#ili_cnty_agg <- aggregate(total + total_pt ~ date + county, data = ili_trim_cnty, FUN = sum)
ili_cnty_agg <- aggregate(. ~ date + county, data = ili_trim_cnty, FUN = sum)
ili_cnty_agg$total_pt[ili_cnty_agg$cases == 0 & ili_cnty_agg$total_pt == 0] = 1
ili_cnty_agg$total = ili_cnty_agg$cases/ifelse(ili_cnty_agg$total_pt == 0, NA, ili_cnty_agg$total_pt)
ili_cnty_agg = ili_cnty_agg[,c('date','county','total')]

# aggregate ili providers by how many reports they submit
#ili_county_states = unique(ili_cnty[,c(ili_cnty$county, ili_cnty$state)])
ili_cnty_counts <- as.data.frame(table(ili_cnty$county))
colnames(ili_cnty_counts) <- c('county','n_reports')

# reshape ili data so each date has only one row
# make sure not all cells for a provider are NA
ili_wide_cnty <- reshape(ili_cnty_agg, v.names = 'total', idvar = 'date', timevar = 'county', direction = 'wide')
ili_wide_cnty = ili_wide_cnty[order(ili_wide_cnty$date),]

# construct a dataframe with no missing values by replacing NA with 0
ili_wide_cnty_zeros <- ili_wide_cnty
for (i in 1:nrow(ili_wide_cnty_zeros)){
  ili_wide_cnty_zeros[i,is.na(ili_wide_cnty_zeros[i,])] = 0
}

save(ili_wide_cnty, file = 'ili_wide_cnty.Rda')
save(ili_cnty_counts, file = 'ili_cnty_counts.Rda')
save(ili_wide_cnty_zeros, file = 'ili_wide_cnty_zeros.Rda')

provider_obs = count(ili[,c('county','Phys_ID_Code')])
provider_counts = as.data.frame(table(provider_obs[,'county']))
provider_counts = provider_counts[order(provider_counts$Var1),]
sums = colSums(is.na(ili_wide_cnty[,-1]))
plot(sums,provider_counts$Freq, main = 'Percentage missing data vs. providers per county', xlab = 'percentage data missing', ylab = 'number of providers per county')