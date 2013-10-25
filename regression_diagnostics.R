setwd("~/flu_surveil_data")
source("~/flu_surveil/first_r2s.R")
source("~/flu_surveil/varSelect.R")
library(pcaMethods)

load("~/flu_surveil_data/flu_gold.Rda")
load("~/flu_surveil_data/ili_unique.Rda")
load("~/flu_surveil_data/ili_wide_no_na.Rda")
load("~/flu_surveil_data/ili_trim.Rda")
load("~/flu_surveil_data/ili_counts.Rda")
colnames(ili_counts) <- c('ids','n_reports')

# read in the results from the regression
flu_reg <- read.csv("file_R2s.csv")

# 1. Rank order all the providers by number of reports and see how that maps onto order selected.
ili_counts <- ili_counts[order(-ili_counts$n_reports),]
flu_reg$ids <- substr(flu_reg$phys_ids,7,100)

counts <- merge(flu_reg, ili_counts, by.x = 'ids', by.y = 'ids')
counts_sorted <- counts[order(counts$X),]
plot(counts_sorted$X,counts_sorted$n_reports, xlab = "order added to regression", ylab = 'number of reports', main = 'Order provider was added to regression \n vs. number of provider reports')

# 2. Plot the initial R^2 for all of the providers, it's possible that essentially it's a 2000 way tie.
first_R2s = first_r2s(obj = flu_gold[,2], vars = ili_wide_no_na[,2:ncol(ili_wide_no_na)], phys_look_up = ili_unique)
plot(first_R2s[,'r2s'], xlab = "Provider ID", ylab = "R^2", main = "R^2 for all providers in regression with \n only one covariate")

# 3. Run a model with just providers in CA and see how that performs compared to the full model.
# reshape ili data so each date has only one row
# make sure not all cells for a provider are NA
ili_trim2_ca <- ili_trim[ili_trim$state == 'CA',c('phys_id','datecode','total')]
ili_wide_ca <- reshape(ili_trim2_ca, v.names = 'total', idvar = 'datecode', timevar = 'phys_id', direction = 'wide')

DATA <- t(ili_wide_ca[,-1])

#running this commented command takes some time
#PPCA<-pca(DATA, nPcs=ncol(DATA),method='ppca',center=TRUE,scale='vector')

#it looks like 4 or 5 PCs is plenty, interestingly the first PC explains ~90% of the variance.  
PPCA <-pca(DATA, nPcs=5,method='ppca',center=TRUE,scale='vector')

#Impute missing data
imputed_data_ca <-completeObs(PPCA)

#put everything back together
ili_wide_no_na_ca <-data.frame(ili_wide_ca[,1],t(imputed_data_ca))

# build a subset
test_ca <- varSelect(obj = flu_gold[,2], vars = ili_wide_no_na_ca[,2:ncol(ili_wide_no_na_ca)], goal = 100, phys_look_up = ili_unique)

save(test_ca, file = 'test_ca.Rda')

# examine the final regression
fit <- lm(flu_gold[,2] ~ test)
summary(fit)
hist(fit$residuals)
plot(rstudent(fit))
plot(fitted(fit),rstudent(fit))
qqnorm(rstudent(fit))
