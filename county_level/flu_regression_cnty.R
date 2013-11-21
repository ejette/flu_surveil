load("~/flu_surveil_data/ili_wide_cnty.Rda")
#load("~/flu_surveil_data/ili_wide_no_na.Rda")
load("~/flu_surveil_data/flu_gold.Rda")
load("county_state.Rda")
ili <- read.csv("~/Downloads/ProviderILI.txt")
library(pcaMethods)

setwd("~/flu_surveil_data")
source("~/flu_surveil/county_level/var_select_cnty.R")
load('ili_unique.Rda')

DATA <- t(ili_wide_cnty[,-1])

#running this commented command takes some time
#PPCA<-pca(DATA, nPcs=ncol(DATA),method='ppca',center=TRUE,scale='vector')

#it looks like 4 or 5 PCs is plenty, interestingly the first PC explains ~90% of the variance.  
PPCA<-pca(DATA, nPcs=14,method='ppca',center=TRUE,scale='vector')
#PPCA2 <- pca(t(ili_wide[,-1]),nPcs=5,method='ppca',center=TRUE,scale='vector')
#Impute missing data
imputed_data <-completeObs(PPCA)

#put everything back together
ili_wide_no_na_cnty <-data.frame(ili_wide_cnty[,1],t(imputed_data))
save(ili_wide_no_na_cnty, file = "~/flu_surveil_data/ili_wide_no_na_cnty.Rda")

# mins = apply(ili_wide_no_na_cnty, 2, function(x) min(x[x!=0]) )
# summary(mins)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -444.70     -5.43     -1.07    140.10      1.00 200800.00 
# maxes = apply(ili_wide_no_na_cnty, 2, function(x) max(x[x!=0]) )
# summary(maxes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1      23      46     253     108  201200 

# build a subset
n_counties = 100
ranks = as.data.frame(1:n_counties)
colnames(ranks) = 'index'
r2_values = as.data.frame(1:n_counties)
colnames(ranks) = 'index'
save(r2_values, file = 'r2_values.Rda')
# remove new orleans from the cities because they have no data for the time period
flu_gold_all = flu_gold_all[,c(1:94,96:ncol(flu_gold_all))]
n = ncol(flu_gold_all)

ranks = var_select_cnty(obj = flu_gold[,2], vars = ili_wide_no_na_cnty[,2:ncol(ili_wide_no_na_cnty)], goal = n_counties, state_look_up = county_state, r2_values = r2_values, ranks = ranks)

ranks_no_zero = var_select_cnty(obj = flu_gold[,2], vars = ili_wide_cnty_zeros[,2:ncol(ili_wide_cnty_zeros)], goal = n_counties, state_look_up = county_state, r2_values = r2_values, ranks = ranks)

load('r2_values.Rda')
colnames(r2_values)  = c('run','imputed','with_zeros')
melted = melt(r2_values, id.vars = 'run')

p = ggplot(melted, aes(x = run, y = value,colour = variable))
p + geom_point() + labs(title = 'Cumulative R^2 for Each Step of Optimization Run',
     x = 'Step of the Optimization',
     y = 'Cumulative R^2') + theme(#legend.position = c(1.25,0.75),
    legend.background = element_rect(fill='#ffffffaa',colour='black'),
    panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
    axis.text = element_text(colour='black',size=15),axis.title = element_text(colour='black',size='20'),panel.grid.minor=element_blank(),
    panel.grid.major = element_blank()) 
#test_no_na <- var_select_cnty(obj = flu_gold[,2], vars = ili_wide_no_na_cnty[,2:ncol(ili_wide_no_na_cnty)], goal = 100, phys_look_up = ili_unique)
#test_zeros <- varSelect(obj = flu_gold[,2], vars = ili_wide_cnty_zeros[,2:ncol(ili_wide_cnty_zeros)], goal = 100, phys_look_up = ili_unique)

save(test_no_na, file = 'test_no_na.Rda')
save(test_zeros, file = 'test_zeros.Rda')