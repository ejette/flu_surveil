library('ggplot2')
library(reshape)

setwd("~/flu_surveil_data")

ili <- read.csv("~/Downloads/ProviderILI.txt")
mmwr <- read.csv("mmwr.csv")

# mmrw is not broken down by age, so get rid of the age-related data in ILI
ili_trim <- ili[,c('Phys_ID_Code','datecode','St','City','County','ZipCode','TypeofPractice','total')]
colnames(ili_trim) <- c('phys_id','datecode','state','city','county','zipcode','type_of_practice','total')

flu <- mmwr[,c('mmrw_week_year','pneum_flu')]
colnames(flu) <- c('datecode', 'deaths')

# create a total category in the ILI network data
attach(ili_trim)
ili_trim$total <- rowSums(ili_trim[,c('Age_0_4','Age_5_24','Age_25_64',
                                         'Age_25_49','Age_50_64','Age_65_and_older')], na.rm = TRUE)
detach(ili_trim)

# aggregate 122 city data to be totals by date
flu_sum <- aggregate(. ~ datecode,  data = flu, FUN = sum)

# only keep observations in the right time window (based on the time window in the ILI data)
flu_gold <- flu_sum[which(flu_sum$datecode >= 200840 & flu_sum$datecode <= 201239), ]

# next step: reshape ili data so each provider has only one row
# make sure not all cells for a provider are NA

# the following code is for diagnostics later
x <- merge(ili, flu_gold, by.x = c('City','St','datecode'), by.y = c('city','state','mmrw_week_year'))


base_ILI <- ggplot(data = new_mmwr, 
                mapping = aes(x = datecode, y = totalpt))
base_ILI + geom_point()