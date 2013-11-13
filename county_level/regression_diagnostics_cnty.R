setwd("~/flu_surveil_data")
source("~/flu_surveil/first_r2s.R")
source("~/flu_surveil/varSelect.R")
library(pcaMethods)

load("flu_gold.Rda")
load("ili_unique.Rda")
load("ili_wide_no_na_cnty.Rda")
load('ili_wide_cnty.Rda')
load("ili_cnty_counts.Rda")
colnames(ili_counts) <- c('ids','n_reports')

# read in the results from the regression
flu_reg <- read.csv("file_R2s_county_no_na2.csv")

# 1. Rank order all the providers by number of reports and see how that maps onto order selected.
ili_cnty_counts <- ili_cnty_counts[order(-ili_cnty_counts$n_reports),]
flu_reg$county <- substr(flu_reg$phys_ids,7,100)
flu_reg$county = gsub("\\.", " ", flu_reg$county) 

counts <- merge(flu_reg, ili_cnty_counts, by.x = 'county', by.y = 'county')
counts_sorted <- counts[order(counts$X),]
plot(counts_sorted$X,counts_sorted$n_reports, xlab = "order added to regression", ylab = 'number of reports', main = 'Order provider was added to regression \n vs. number of provider reports')

hist(counts$n_reports)

# 2. Plot the initial R^2 for all of the providers, it's possible that essentially it's a 2000 way tie.
ili_unique$Phys_ID_Code <- str_trim(ili_unique$Phys_ID_Code)
first_R2s = first_r2s(obj = flu_gold[,2], vars = ili_wide_no_na[,2:ncol(ili_wide_no_na)], phys_look_up = ili_unique)
save(first_R2s,file = 'first_R2s.Rda')
plot(first_R2s[,'r2'], xlab = "Provider ID", ylab = "R^2", main = "R^2 for all providers in regression with \n only one covariate")

# 3. Run a model with just providers in CA and see how that performs compared to the full model.
# reshape ili data so each date has only one row
# make sure not all cells for a provider are NA
ili_trim2_ca <- ili_trim[ili_trim$state == 'CA',c('phys_id','datecode','total')]
# get a list of all the Californian providers
ids_ca <- as.character(unique(ili_trim2_ca$phys_id))
# subset the data
ili_wide_no_na_ca = ili_wide_no_na[,substr(colnames(ili_wide_no_na),7,100) %in% ids_ca]

# the following commented code imputes the values just using the california providers
#ili_wide_ca <- reshape(ili_trim2_ca, v.names = 'total', idvar = 'datecode', timevar = 'phys_id', direction = 'wide')
#DATA <- t(ili_wide_ca[,-1])

#running this commented command takes some time
#PPCA<-pca(DATA, nPcs=ncol(DATA),method='ppca',center=TRUE,scale='vector')

#it looks like 4 or 5 PCs is plenty, interestingly the first PC explains ~90% of the variance.  
#PPCA <-pca(DATA, nPcs=5,method='ppca',center=TRUE,scale='vector')

#Impute missing data
#imputed_data_ca <-completeObs(PPCA)

# build a subset
test_ca <- varSelect(obj = flu_gold[,2], vars = ili_wide_no_na_ca, goal = 100, phys_look_up = ili_unique)

save(test_ca, file = 'test_ca.Rda')

# examine the final regression
fit <- lm(flu_gold[,2] ~ test_ca)
summary(fit)
hist(fit$residuals)
plot(rstudent(fit))
plot(fitted(fit),rstudent(fit))
qqnorm(rstudent(fit))
