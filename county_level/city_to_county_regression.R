library(reshape)
library(plyr)
library(stringr)
library(ggplot2)
source("~/flu_surveil/county_level/var_select_cnty.R")

setwd("~/flu_surveil_data")
mmwr <- read.csv("mmwr.csv")
load("flu_gold.Rda")
load("flu_gold_all.Rda")
load("ili_wide_no_na_cnty.Rda")
load('county_state.Rda')


# make set of y's for the regression
# each column is a city in the 122-cities
# and there is one column for the aggregated values

# city is not a unique identifier
# # make a variable that is a combination of city and state
# mmwr$city_state = paste(mmwr$city,mmwr$state,sep="_")
# cities_temp = count(mmwr$city_state)
# cities = c('national',as.character(cities_temp[,1]))
# flu <- mmwr[which(mmwr$mmrw_week_year >= 200840 & mmwr$mmrw_week_year <= 201239),c('city_state','mmrw_week_year','pneum_flu')]
# 
# colnames(flu)[2:3] <- c('date', 'deaths')
# 
# # make data wide so there is column for each city
# flu_wide <- reshape(flu, v.names = 'deaths', idvar = 'date', timevar = 'city_state', direction = 'wide')
# flu_wide_sort = flu_wide[order(flu_wide$date),]
# 
# flu_gold_all = cbind(flu_gold,flu_wide_sort[,-1])
# colnames(flu_gold_all)[2] = 'deaths.national'
# save(flu_gold_all, file = 'flu_gold_all.Rda')

#cities = c('national',as.character(cities))
n_counties = 50
ranks = as.data.frame(1:n_counties)
colnames(ranks) = 'index'
r2_values = as.data.frame(1:n_counties)
colnames(ranks) = 'index'
save(r2_values, file = 'r2_values.Rda')
# remove new orleans from the cities because they have no data for the time period
flu_gold_all = flu_gold_all[,c(1:94,96:ncol(flu_gold_all))]
n = ncol(flu_gold_all)

for (i in 2:n){
   print(i)
   load('r2_values.Rda')
   ranks = var_select_cnty(obj = flu_gold_all[,i], vars = ili_wide_no_na_cnty[,2:ncol(ili_wide_no_na_cnty)], goal = n_counties, state_look_up = county_state, r2_values = r2_values, ranks = ranks)
   save(ranks,file = 'ranks.Rda')
 }

#ranks_temp = ranks[,1:123]
load('ranks.Rda')
ranks_temp = ranks
save(ranks_temp,file = 'ranks_temp.Rda')
load('r2_values.Rda')
#cities = c(cities[1:94],cities[96:n])
cols = substr(colnames(flu_gold_all),8,100)
cols2 = cols[c(2:94,96:124)]
#cities2 = cities[cities != 'New Orleans_LA']
colnames(ranks_temp)[2:ncol(ranks_temp)] = cols2

ranks_long = melt(ranks_temp, id.vars = 'index')
names(ranks_long)=c("color","x","y")
ranks_long$color=as.numeric(ranks_long$color)

ranks_long1 = ranks_long[1:1500,]
ranks_long2 = ranks_long[1501:nrow(ranks_long),]

pdf(file="regression_heatmap.pdf",width = 36, height = 36)
ggplot(data =  ranks_long, aes(x = x, y = y))  + 
  geom_tile(aes(fill = color), colour = "white") + 
  scale_fill_gradient(low = "steelblue", high = "ghostwhite") + 
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(#legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 300, hjust = 0),
        axis.text.y = element_text(size = 2)) + labs(title = 'Rankings of ILI counties picked to predict \n gold standard time series, the 122 cities data for \n Week 40 of 2008 to Week 20 of 2012',
                                                     x = '122 Cities',
                                                     y = 'ILI Counties') 


#ggplot(data =  ranks_long, aes(x = x, y = y))  + 
 # geom_tile(aes(fill = color), colour = "white") +
  #scale_fill_brewer(palette = "PRGn") +   theme_grey(base_size = 9) + 
  #theme(legend.position = "none",
   #     axis.ticks = element_blank(), 
    #    axis.text.x = element_text(angle = 300, hjust = 0))

dev.off()
attach(ranks_long)
ranks_long$score = 1/ranks_long$color
#j = 5
#for (i in seq(1,50,by=10)){
 # print(i)
#  ranks_long$score[color >= i & color <= (i + 9)] = j
 # j = j - 1
#}

ranks_pts <- aggregate(score ~ y, data = ranks_long[,c('score','y')], FUN = sum)
table(ranks_pts$score)