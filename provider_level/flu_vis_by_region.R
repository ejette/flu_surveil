library(zoo)
library(stringr)
library(ggplot2)
library(reshape)
library(scales)

setwd("~/flu_surveil_data")

# load and format weekly report data
mmwr <- read.csv("mmwr.csv")
flu <- mmwr[,c('city','state','mmrw_week_year','pneum_flu')]
colnames(flu) <- c('city','state','date','deaths')

# load ILInet data
load("~/flu_surveil_data/ili_trim.Rda")
load("~/flu_surveil_data/ili_wide_no_na.Rda")
load("~/flu_surveil_data/ili_wide.Rda")
load("flu_gold.Rda")

# load R2 from first run of optimization
load("~/flu_surveil_data/first_R2s.Rda")

# break up states into regions as defined by mmwr website
new_england = c('MA','CT','RI')
mid_atlantic = c('NY','NJ','PA')
en_central = c('OH','IL','MI','IN','WI')
wn_central = c('IA','MN','NE','KS','MO')
es_central = c('AL','TN','KY') 
s_atlantic = c('MD','NC','VA','GA','DC','DE','FL','GA')
ws_central = c('TX','LA','OK','AR')
mountain = c('ID','NM','CO','NV','UT','AZ')
pacific = c('CA','HI','WA','OR')

all = c(new_england, mid_atlantic, en_central, wn_central,es_central, s_atlantic, ws_central, mountain, pacific)
all_regions = list(new_england, mid_atlantic, en_central, es_central, wn_central, s_atlantic, ws_central, mountain, pacific,all)

# assign each row in flu a region
flu$region = 0
loop_len = length(all_regions)-1

# assign each observation a region
for (i in 1:loop_len){
  flu$region[flu$state %in% all_regions[[i]]] = i
} 

start = 200840
start_yr = as.numeric(substr(start,1,4))
end = 201220

counts <- aggregate(. ~ date,  flu[which((flu$date >= start & flu$date <= end )), c('date','deaths')], FUN = sum)
colnames(counts) = c('dates','national')

for(j in 1:loop_len){
  agg <- aggregate(. ~ date,  flu[which((flu$date >= start & flu$date <= end & flu$region == j )), c('date','deaths')], FUN = sum)
  counts = cbind(counts,agg[,2])
  colnames(counts)[j+2] = paste('region',j,sep = '')
}

date <- as.Date(paste(start_yr, '-09-29' , sep = "")) + seq(from = 1, to = 1324, by = 7) - 1

pdf(file="~/flu_surveil_data/flu_seasons_agg_regions.pdf",width = 8.5, height = 11)
counts_zoo = zoo(counts[,-c(1)],date)
counts_zoo_regional = zoo(counts[,-c(1,2)],date)

p_nat = autoplot(counts_zoo, facet = NULL) +  labs(title = 'Deaths Resulting from Flu \n or Pneumonia by Region of the US, \n Week 40 of 2008 to Week 20 of 2012',
          x = 'Year',
          y = 'Deaths (raw counts)') + 
  theme(#legend.position = c(1.25,0.75),
    legend.background = element_rect(fill='#ffffffaa',colour='black'),
    panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
    axis.text = element_text(colour='black',size=15),axis.title = element_text(colour='black',size='20'),panel.grid.minor=element_blank(),
    panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005)) #+ scale_x_discrete(expand=c(0.005,0.005))
print(p_nat)

p_region = autoplot(counts_zoo_regional, facet = NULL) +labs(title = 'Deaths Resulting from Flu \n or Pneumonia by Region of the US, \n Week 40 of 2008 to Week 20 of 2012',
  x = 'Year', y = 'Deaths (raw counts)') + theme(#legend.position = c(1.25,0.75), 
  legend.background = element_rect(fill='#ffffffaa',colour='black'),
  panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=15),axis.title = element_text(colour='black',size='20'),panel.grid.minor=element_blank(),
  panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005)) 

print(p_region)
#dev.off()

# Plot east coast vs. west coast
counts_coast <- aggregate(. ~ date,  flu[which((flu$date >= start & flu$date <= end & flu$region == 9)), c('date','deaths')], FUN = sum)
colnames(counts_coast) = c('dates','West Coast')

agg <- aggregate(. ~ date,  flu[which((flu$date >= start & flu$date <= end & (flu$region == 1 | flu$region == 2 ))), c('date','deaths')], FUN = sum)
counts_coast = cbind(counts_coast,agg[,2])
colnames(counts_coast)[3] = 'East Coast'

date <- as.Date(paste(start_yr, '-09-29' , sep = "")) + seq(from = 1, to = 1324, by = 7) - 1

pdf(file="~/flu_surveil_data/flu_seasons_agg_coast.pdf")
counts_coast_zoo = zoo(counts_coast[,-c(1)],date)

p_coast = autoplot(counts_coast_zoo, facet = NULL)+  labs(title = 'Deaths Resulting from Flu \n or Pneumonia by Region of the US, \n Week 40 of 2008 to Week 20 of 2012',
          x = 'Year', y = 'Deaths (raw counts)') + theme(#legend.position = c(1.25,0.75), 
            legend.background = element_rect(fill='#ffffffaa',colour='black'),
            panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
            axis.text = element_text(colour='black',size=15),axis.title = element_text(colour='black',size='20'),panel.grid.minor=element_blank(),
            panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))  
print(p_coast)

dev.off()

# Do just for the 11-12 flu season
counts_11_12 = counts[158:nrow(counts),]
flu_yrs = flu[which(flu$date >= start & flu$date <= end),]
dates_11_12 = date[106:length(date)]

date <- seq(as.Date("2011-10-02"), as.Date("2012-05-15"), by = "week") 

counts_11_12_zoo = zoo(counts_11_12[,-c(1),dates_11_12])
counts_df = cbind(fortify(counts_11_12_zoo),date)
counts_long <- melt(counts_df, id = "date", measure = c("national","region1", "region2", "region3","region4","region5","region6","region7","region8","region9"))
counts_long_regional <- melt(counts_df, id = "date", measure = c("region1", "region2", "region3","region4","region5","region6","region7","region8","region9"))

p_s2_nat = ggplot(counts_long, aes(date, value, colour = variable)) + 
  geom_line() +  labs(title = 'Deaths Resulting from Flu \n or Pneumonia by Region of the US, \n Week 40 of 2011 to Week 20 of 2012',
   x = 'Time', y = 'Deaths (raw counts)')  + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) + 
  theme(#legend.position = c(1.25,0.75),
    legend.background = element_rect(fill='#ffffffaa',colour='black'),
    panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
    axis.text = element_text(colour='black',size=15),axis.title = element_text(colour='black',size='20'),panel.grid.minor=element_blank(),
    panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005)) 
print(p_s2_nat)

p_s2 =  ggplot(counts_long_regional, aes(date, value, colour = variable)) + 
  geom_line() +  labs(title = 'Deaths Resulting from Flu \n or Pneumonia by Region of the US, \n Week 40 of 2011 to Week 20 of 2012',
                      x = 'Time', y = 'Deaths (raw counts)') + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) + 
  theme(#legend.position = c(1.25,0.75),
    legend.background = element_rect(fill='#ffffffaa',colour='black'),
    panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
    axis.text = element_text(colour='black',size=15),axis.title = element_text(colour='black',size='20'),panel.grid.minor=element_blank(),
    panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005)) 

print(p_s2)
# code that breaks dates into flu seasons
#counts_zoo$fac = 0
#counts_zoo$fac[1:33] = 1
#counts_zoo$fac[54:85] = 2
#counts_zoo$fac[158:nrow(counts_zoo)] = 3
dev.off()

pdf(file="~/flu_surveil_data/ILI_counts_national_08_12.pdf", width = 8.5, height = 11)
# Make plot of total number of ILInet cases across the US
counts_ili <- aggregate(. ~ date,  ili_trim[which((ili_trim$date >= start & ili_trim$date <= end )), c('date','total')], FUN = sum)
colnames(counts_ili) = c('dates','national')
date <- as.Date(paste(start_yr, '-09-29' , sep = "")) + seq(from = 1, to = 1324, by = 7) - 1
counts_ili_zoo <- zoo(counts_ili[,2],date)
p_ili = autoplot(counts_ili_zoo, facet = NULL) +  labs(title = 'Number of ILI cases reported in the US ILINet, \n Week 40 of 2008 to Week 20 of 2012',
          x = 'Year', y = 'Deaths (raw counts)')  + theme(#legend.position = c(1.25,0.75),
            legend.background = element_rect(fill='#ffffffaa',colour='black'),
            panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
            axis.text = element_text(colour='black',size=15),axis.title = element_text(colour='black',size='20'),panel.grid.minor=element_blank(),
            panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005)) 
print(p_ili)

dev.off()


# sort by r^2 value
load('first_R2s.Rda')
first_R2s$r2 <- as.numeric(first_R2s$r2)
r2s <- first_R2s[order(-first_R2s$r2),]

providers = r2s[c(1:3,99,222,981),]
#providers$row = 1:nrow(providers)
providers$rating = c('high','high','high','low','lower','lowest')
#providers_long =  melt(providers, id = "date", measure = c())

flu_gold$flu = "not flu season"
flu_gold$flu[substr(flu_gold$date,5,6) >= 40] = "flu season"
flu_gold$flu[substr(flu_gold$date,5,6) <= 20] = "flu season"
y = as.numeric(flu_gold[,2])


pdf(file="~/flu_surveil_data/ILI_providers_vs_deaths.pdf", width = 8.5, height = 11)
plots = list()
plots_na = list()

for (i in 1:nrow(providers)){
  
  x = ili_wide_no_na[,providers[i,'colname']]
  x_nas = ili_wide[,providers[i,'colname']]
  
  data = as.data.frame(cbind(cbind(x,x_nas),y))
  data$flu = flu_gold[,3]
  
  p = ggplot(data,aes(x = x, y = y, colour = factor(flu))) + geom_point() + labs(title = paste('Number of ILI cases reported by',providers[i,'id'],' (',providers[i,'city'],', ',providers[i,'state'],', \n rating = ', providers[i,'rating'],') vs. deaths for all 122 cities, \n imputed values', sep = ''),
                          x = 'ILI cases',
                          y = 'Deaths') + 
    theme(#legend.position = c(1.25,0.75),
      legend.background = element_rect(fill='#ffffffaa',colour='black'),
      panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
      axis.text = element_text(colour='black',size=15),axis.title = element_text(colour='black',size='20'),panel.grid.minor=element_blank(),
      panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))
 # print(p)
  #plots = c(plots,p)
  

  p_na <- ggplot(data,aes(x = x_nas, y = y, colour = factor(flu))) + geom_point() + labs(title = paste('Number of ILI cases reported by',providers[i,'id'],' (',providers[i,'city'],', ',providers[i,'state'],',\n rating = ', providers[i,'rating'],')  vs. deaths for all 122 cities, \n not using imputed values', sep = ''),
                          x = 'ILI cases',
                          y = 'Deaths') + 
    theme(#legend.position = c(1.25,0.75),
      legend.background = element_rect(fill='#ffffffaa',colour='black'),
      panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
      axis.text = element_text(colour='black',size=15),axis.title = element_text(colour='black',size='20'),panel.grid.minor=element_blank(),
      panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))
  #plots_na = c(plots_na, p_na)
  #print(p_na)
  grid.arrange(p,p_na)
}

dev.off()

library(gridExtra)

#plots_mmwr = list(p_nat, p_region), c(p_s2_nat,p_s2))

pdf("flu_seasons_agg_regions.pdf", onefile = TRUE, width = 8.5, height = 11)
grid.arrange(p_nat,p_region)
grid.arrange(p_s2_nat,p_s2) 
grid.arrange(p_coast)
dev.off()

pdf("ili_vs_deaths.pdf", onefile = TRUE, width = 8.5, height = 11)
#for (i in 1:length(plots)){
#grid.arrange(plots[[i]],plots_na[[i]])
#}
grid.arrange(p,p_na)
dev.off()

 #                     ncol=2, widths=c(1,1.2)) 
#dev.off()