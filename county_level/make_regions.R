library(stringr)
setwd("~/flu_surveil_data")
load('ranks_long_format.Rda')

new_england = c('MA','CT','RI','ME','NH','VT')
mid_atlantic = c('NY','NJ','PA')
en_central = c('OH','IL','MI','IN','WI')
wn_central = c('IA','MN','NE','KS','MO','SD','ND','MT','WY')
es_central = c('AL','TN','KY') 
s_atlantic = c('MD','NC','SC','VA','GA','DC','DE','FL','WV')
ws_central = c('TX','LA','OK','AR','MS')
mountain = c('ID','NM','CO','NV','UT','AZ')
pacific = c('CA','HI','WA','OR','AK')

all_regions = list(new_england, mid_atlantic, en_central, es_central, wn_central, s_atlantic, ws_central, mountain, pacific)
regions = c('New England','Mid-Atlantic','Northeast Central','Southeast Central',
            'Northwest Central', 'South Atlantic','Southwest Central','Mountain','Pacific')

# assign each row in flu a region
ranks_long_format$region = 'all regions'
loop_len = length(all_regions)

#ranks_long_format$state = substr(ranks_long_format$x,1,2)
#dta = ranks_long_format
ranks_long_format$St = strsplit(as.character(ranks_long_format$x),split="_")
ranks_long_format$state = as.character(ranks_long_format$x)

for (i in 1:nrow(ranks_long_format)){ranks_long_format$State[i] = ranks_long_format$St[i][[1]][2]}

# assign each observation a region
for (i in 1:loop_len){
  ranks_long_format$region[ranks_long_format$State %in% all_regions[[i]]] = regions[i]
} 

ranks = ranks_long_format[,c('color','x','y','region')]
colnames(ranks) = c('rank','target_city','St-County','region')
write.csv(ranks, file = 'ranks.csv')