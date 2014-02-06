setwd("~/flu_surveil_data")
# load and format weekly report data
load('flu_gold.Rda')
load("~/flu_surveil_data/ili_wide_cnty_zeros_FIPS.Rda")
mmwr <- read.csv("mmwr.csv")

flu <- mmwr[which(mmwr$mmrw_week_year >= 200840 & mmwr$mmrw_week_year <= 201239),c('city','state','mmrw_week_year','pneum_flu')]
flu$state_full = state.name[match(flu$state,state.abb)]

# states in each HHS region
region1 = c('Connecticut', 'Maine','Massachusetts',
            'New Hampshire','Rhode Island','Vermont')
region2 = c('New Jersey', 'New York')
region3 = c('Delaware','District of Columbia','Maryland',
            'Pennsylvania','Virginia','West Virginia')
region4 = c('Alabama', 'Florida', 'Georgia','Kentucky','Mississippi','North Carolina',
            'South Carolina','Tennessee')
region5 = c('Illinois', 'Indiana', 'Michigan', 'Minnesota', 'Ohio', 'Wisconsin')
region6 = c('Arkansas', 'Louisiana', 'New Mexico', 'Oklahoma', 'Texas')
region7 = c('Iowa', 'Kansas', 'Missouri', 'Nebraska')
region8 = c('Colorado', 'Montana', 'North Dakota', 'South Dakota', 'Utah', 'Wyoming')
region9 = c('Arizona', 'California', 'Hawaii', 'Nevada')
region10 = c('Alaska', 'Idaho', 'Oregon', 'Washington')

all_regions = list(region1, region2, region3, region4, region5, region6, region7, region8, region9, region10)
loop_len = length(all_regions)

# assign each observation to an HHS region
flu$region = 0

for (i in 1:loop_len){
  flu$region[flu$state_full %in% all_regions[[i]]] = i
} 

# assign DC to region 3
flu[flu$region == 0, 'region'] = 3

# make a variable that contains week number
flu$week_num = as.numeric(substr(flu$mmrw_week_year,5,8))
# extract flu seasons
flu = flu[flu$week_num >= 40 | flu$week_num <= 20, c('mmrw_week_year','pneum_flu','region')]


# aggregate up to the region
region_agg = aggregate(. ~ mmrw_week_year + region, data = flu, FUN = sum)

colnames(region_agg) <- c('date','region','deaths')
 
# make data wide so there is column for each region
flu_wide <- reshape(region_agg, v.names = 'deaths', idvar = 'date', timevar = 'region', direction = 'wide')
flu_wide_sort = flu_wide[order(flu_wide$date),]

# extract the national deaths that occurred during flu season
flu_gold$week_num = as.numeric(substr(flu_gold$date,5,8))
# extract flu seasons
flu_gold = flu_gold[flu_gold$week_num >= 40 | flu_gold$week_num <= 20, ]

flu_gold_regions = cbind(flu_gold[,-3],flu_wide_sort[,-1])
colnames(flu_gold_regions)[2] = 'deaths.national'
save(flu_gold_regions, file = 'flu_gold_regions.Rda')

write.table(flu_gold_regions, file = 'flu_gold_regions.csv', row.names = FALSE)

# extract flu seasons from ilinet data
ili_wide_cnty_zeros_FIPS$week_num = as.numeric(substr(ili_wide_cnty_zeros_FIPS$date,5,8))
# extract flu seasons
ili_flu_seasons = ili_wide_cnty_zeros_FIPS[ili_wide_cnty_zeros_FIPS$week_num >= 40 | ili_wide_cnty_zeros_FIPS$week_num <= 20, -1359]
write.table(ili_flu_seasons, file = 'ili_flu_seasons.csv', row.names = FALSE)
