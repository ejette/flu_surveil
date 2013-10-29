library(reshape)

setwd("~/flu_surveil_data")

ili <- read.csv("~/Downloads/ProviderILI.txt")
summmmwr <- read.csv("mmwr.csv")

# create a total category in the ILI network data
attach(ili)
ili$total <- rowSums(ili[,c('Age_0_4','Age_5_24','Age_25_64',
                                      'Age_25_49','Age_50_64','Age_65_and_older')], na.rm = TRUE)
detach(ili)

# mmrw is not broken down by age, so get rid of the age-related data in ILI
ili_trim <- ili[,c('Phys_ID_Code','datecode','St','City','County','ZipCode','TypeofPractice', 'total')]
colnames(ili_trim) <- c('phys_id','datecode','state','city','county','zipcode','type_of_practice','total')

flu <- mmwr[,c('mmrw_week_year','pneum_flu')]
colnames(flu) <- c('datecode', 'deaths')

# aggregate 122 city data to be totals by date
flu_sum <- aggregate(. ~ datecode,  data = flu, FUN = sum)

# aggregate ili providers by how many reports they submit
ili_counts <- as.data.frame(table(ili_trim$phys_id))

# only keep observations in the right time window (based on the time window in the ILI data)
flu_gold <- flu_sum[which(flu_sum$datecode >= 200840 & flu_sum$datecode <= 201239), ]
flu_gold_mat <- matrix(flu_gold[,2])

# reshape ili data so each date has only one row
# make sure not all cells for a provider are NA
ili_trim2 <- ili_trim[,c('phys_id','datecode','total')]
ili_wide <- reshape(ili_trim2, v.names = 'total', idvar = 'datecode', timevar = 'phys_id', direction = 'wide')

# construct a dataframe with no missing values by replacing NA with 0
ili_wide_no_na <- ili_wide
for (i in 1:nrow(ili_wide_no_na)){
  ili_wide_no_na[i,is.na(ili_wide_no_na[i,])] = 0
}

save(ili_wide, file = 'ili_wide.Rda')
save(ili_wide_no_na, file = 'ili_wide_no_na.Rda')
save(ili_counts, file = 'ili_counts.Rda')
save(flu_gold, file = 'flu_gold.Rda')
save(ili_trim, file = 'ili_trim.Rda')


