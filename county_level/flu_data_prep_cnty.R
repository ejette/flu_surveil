library(reshape)
library(plyr)
library(stringr)

setwd("~/flu_surveil_data")

ili <- read.csv("~/Downloads/ProviderILI.txt")
mmwr <- read.csv("mmwr.csv")

# create a total category in the ILI network data
attach(ili)
ili$total <- rowSums(ili[,c('Age_0_4','Age_5_24','Age_25_64',
                                      'Age_25_49','Age_50_64','Age_65_and_older')], na.rm = TRUE)
detach(ili)

ili = ili[order(ili$Phys_ID_Code, ili$datecode),]
ili$county = toupper(str_trim((as.character(ili$County))))
ili$county[ili$County == ''] = 'NEW YORK'

ili$city = str_trim((as.character(ili$City)))
ili$state = str_trim((as.character(ili$St)))
ili$city[ili$city == 'Lee\x92s Summit'] = 'Lee Summit'

# make counties more uniform
# replace periods with an empty string
ili$county = gsub("\\.", "", ili$county) 
ili$county[ili$county == 'VIRGINIA BEACH' | ili$county == 'VIRGINIA BEACH CITY'] = 'PRINCESS ANNE'
ili$county[ili$county == 'NEWPORT NEWS'] = 'NEWPORT NEWS CITY'
ili$county[ili$county == 'FREDERICKBURG CITY'] = 'FREDERICKSBURG CITY'
ili$county[ili$county == 'FAIRFAX CITY'] = 'FAIRFAX'
ili$county[ili$county == 'FAIRFILED'] = 'FAIRFIELD'
ili$county[ili$county == 'CHESAPEAKE CITY'] = 'CHESAPEAKE'

cnty = ili[,c('county','state','total')]
cnty$state = as.factor(cnty$state)
cnty$county = as.factor(cnty$county)

# extract unique state/county combinations
combos = count(cnty[,-3])
# count number of unique state/county combinations
combos2 = as.data.frame(table(combos[,'county']))
# extract the names of the counties that appear in more than one state
combos_sub <- subset(combos2, combos2$Freq > 1)

for (i in 1:nrow(combos_sub)){
  # divided ili into two data sets one set with the duplicate counties and the other without
  piece = ili[ili$county == combos_sub$Var1[i],]
  leftover = ili[!(ili$county == combos_sub$Var1[i]),]
  piece$county = paste(combos_sub$Var1[i], piece$St , sep = ' ')
  ili = rbind(piece, leftover)
}

counties = as.data.frame(table(ili$county))

ili_cnty = ili
save(ili_cnty, file = 'ili_cnty.Rda')
# test to see if county and state combinations are unique
test_df = ili[,c('county','state')]
test_combos = count(test_df)
county_state = test_combos
save(county_state, file = 'county_state.Rda')
test_combos2 = as.data.frame(table(test_combos[,'county']))
test_combos_sub <- subset(test_combos2, test_combos2$Freq > 1)

ili_trim_cnty <- ili_cnty[,c('datecode','county','total')]
colnames(ili_trim_cnty) <- c('date','county','total')

# aggregate to county level
ili_trim_cnty = ili_trim_cnty[order(ili_cnty_agg$county,ili_cnty_agg$date),]
ili_cnty_agg <- aggregate(total ~ date + county, data = ili_trim_cnty, FUN = sum)

# aggregate ili providers by how many reports they submit
ili_county_states = unique(ili_cnty[,c(ili_cnty$county, ili_cnty$state)])
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