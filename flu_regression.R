load("~/flu_surveil_data/ili_wide.Rda")
load("~/flu_surveil_data/ili_wide_no_na.Rda")
load("~/flu_surveil_data/flu_gold.Rda")
ili <- read.csv("~/Downloads/ProviderILI.txt")
library(pcaMethods)

setwd("~/flu_surveil_data")
source("~/flu_surveil/varSelect.R")

ili_unique <- as.data.frame(as.matrix(unique(ili[,c('Phys_ID_Code','St','City')])), stringsAsFactors = FALSE)
save(ili_unique, file = 'ili_unique.Rda')
DATA <- t(ili_wide[,-1])

#running this commented command takes some time
#PPCA<-pca(DATA, nPcs=ncol(DATA),method='ppca',center=TRUE,scale='vector')

#it looks like 4 or 5 PCs is plenty, interestingly the first PC explains ~90% of the variance.  
PPCA<-pca(DATA, nPcs=5,method='ppca',center=TRUE,scale='vector')

#Impute missing data
imputed_data <-completeObs(PPCA)

#put everything back together
ili_wide_no_na <-data.frame(ili_wide[,1],t(imputed_data))
save(ili_wide_no_na, file = "~/flu_surveil_data/ili_wide_no_na.Rda")

# build a subset
test <- varSelect(obj = flu_gold[,2], vars = ili_wide_no_na[,2:ncol(ili_wide_no_na)], goal = 100, phys_look_up = ili_unique)

save(test, file = 'test.Rda')