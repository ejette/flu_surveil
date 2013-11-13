load("~/flu_surveil_data/ili_wide_cnty.Rda")
#load("~/flu_surveil_data/ili_wide_no_na.Rda")
load("~/flu_surveil_data/flu_gold.Rda")
ili <- read.csv("~/Downloads/ProviderILI.txt")
library(pcaMethods)

setwd("~/flu_surveil_data")
source("~/flu_surveil/varSelect.R")
load('ili_unique.Rda')
load('ili_wide.Rda')

DATA <- t(ili_wide_cnty[,-1])

#running this commented command takes some time
#PPCA<-pca(DATA, nPcs=ncol(DATA),method='ppca',center=TRUE,scale='vector')

#it looks like 4 or 5 PCs is plenty, interestingly the first PC explains ~90% of the variance.  
PPCA<-pca(DATA, nPcs=10,method='ppca',center=TRUE,scale='vector')
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
test_no_na <- varSelect(obj = flu_gold[,2], vars = ili_wide_no_na_cnty[,2:ncol(ili_wide_no_na_cnty)], goal = 100, phys_look_up = ili_unique)


test_zeros <- varSelect(obj = flu_gold[,2], vars = ili_wide_cnty_zeros[,2:ncol(ili_wide_cnty_zeros)], goal = 100, phys_look_up = ili_unique)

save(test_no_na, file = 'test_no_na.Rda')
save(test_zeros, file = 'test_zeros.Rda')