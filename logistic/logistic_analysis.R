library(aod)
library(ggplot2)

setwd("~/Dropbox/122citites")
# load y data
load("~/flu_surveil_data/logistic_data/y.Rda")
# load humidity difference date
load("~/flu_surveil_data/logistic_data/humidity_diff.Rda")
# load population difference data
load("~/flu_surveil_data/logistic_data/pop_diff.Rda")
# load geographic difference data
load("~/Dropbox/122citites/reg mats/northSouthDist.Rda")
# load geographic difference data
load("~/Dropbox/122citites/reg mats/eastWestDist.Rda")

load("~/Dropbox/122citites/reg mats/humidity_diff.Rda")
load("~/Dropbox/122citites/y.Rda")
load("~/Dropbox/122citites/reg mats/pop_diff.Rda")

#pop_diff$FIPS_id = as.numeric(as.character(pop_diff$FIPS_id))

# format FIPS code to be five characters long
format_FIPS_id <- function(data){
  for (i in 1:length(data)){
    if (nchar(data[i]) < 5){
      len = nchar(data[i])
      data[i] = paste(paste(rep('0', 5 - len), collapse =''),data[i],sep='') 
    }
  }
  return(data)
}

y$FIPS_id = format_FIPS_id(as.character(y$FIPS_code))
humidity_diff$FIPS_id = format_FIPS_id(as.character(humidity_diff$FIPS_id))
pop_diff$FIPS_id = format_FIPS_id(as.character(pop_diff$FIPS_id))
NS[,1] = humidity_diff$FIPS_id
EW[,1] = humidity_diff$FIPS_id

y$FIPS_num = as.numeric(y$FIPS_id)
humidity_diff$FIPS_num = as.numeric(humidity_diff$FIPS_id)
pop_diff$FIPS_num = as.numeric(pop_diff$FIPS_id)
EW = cbind(EW,as.numeric(EW[,1]))
NS = cbind(NS,as.numeric(NS[,1]))

y = y[order(as.numeric(y$FIPS_num)),]
humidity_diff = humidity_diff[order(humidity_diff$FIPS_num),]
pop_diff = pop_diff[order(pop_diff$FIPS_num)),]
NS = NS[order(NS[,124]),]
EW = EW[order(EW[,124]),]

y$FIPS_id = format_FIPS_id(as.character(y$FIPS_code))
humidity_diff$FIPS_id = format_FIPS_id(as.character(humidity_diff$FIPS_id))
pop_diff$FIPS_id = format_FIPS_id(as.character(pop_diff$FIPS_id))
# There is something wrong with the way NS and EW FIPS_id was converted, but the rows are in the same 
# order as humidity_diff, so we can use that




# run one logistic regression
# save p-values, coefficients
