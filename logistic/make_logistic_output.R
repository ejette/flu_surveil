library(plyr)
library(stringr)
source('~/flu_surveil/logistic/format_fips.R')
load("~/flu_surveil_data/m_county.Rda")
load("~/flu_surveil_data/ranks.Rda")
load("~/flu_surveil_data/ranks_long_format.Rda")
load("~/flu_surveil_data/county_FIPS_xwalk.Rda")

#ranks_long_format$y[ranks_long_format$y == 'NM- Hidalgo'] = 'NM-Hidalgo'
df = as.data.frame(str_trim(str_split_fixed(ranks_long_format$y, '-', 2)),stringsAsFactors = FALSE)
df[df[,2] == 'Dekalb' & df[,1] == 'AL',2] = 'De Kalb'
df[df[,2] == 'Dekalb' & df[,1] == 'IN',2] = 'De Kalb'
df[df[,2] == 'Dekalb' & df[,1] == 'IL',2] = 'De Kalb'
df[df[,2] == 'Desoto' & df[,1] == 'FL',2] = 'De Soto'
df[df[,2] == 'Desoto' & df[,1] == 'MS',2] = 'De Soto'
df[df[,2] == 'Miami Dade' & df[,1] == 'FL',2] = 'Miami-Dade'
df[df[,2] == 'Dupage' & df[,1] == 'IL',2] = 'Du Page'
df[df[,2] == 'Lasalle' & df[,1] == 'IL',2] = 'La Salle'
df[df[,2] == 'Concoridia' & df[,1] == 'LA',2] = 'Concordia'
df[df[,2] == 'Cleveland' & df[,1] == 'MS',2] = 'Bolivar'
df[df[,2] == 'Ostego' & df[,1] == 'NY',2] = 'Otsego'
df[df[,2] == 'Chesapeake' & df[,1] == 'VA',2] = 'Chesapeake City'
df[df[,2] == 'Harrisonburg' & df[,1] == 'VA',2] = 'Harrisonburg City'
df[df[,2] == 'Radford City' & df[,1] == 'VA',2] = 'Radford'
df[df[,2] == 'Salem City' & df[,1] == 'VA',2] = 'Salem'
df[df[,2] == 'Pleasant' & df[,1] == 'WV',2] = 'Pleasants'
df[df[,2] == 'Pleasant' & df[,1] == 'WV',2] = 'Pleasants'

ranks_long_format$y = paste(df[,1],'-',df[,2],sep = '')

county_FIPS_xwalk$state_county = paste(county_FIPS_xwalk$state,'-',county_FIPS_xwalk$county,sep = '')

m = merge(county_FIPS_xwalk, ranks_long_format, by.x = "state_county", by.y = 'y', all.y = TRUE)
ranks_with_fips_long = m[,c('x','FIPS_code','color')]

ranks_with_fips_temp  = reshape(ranks_with_fips_long, timevar =c('x'), idvar = 'color', direction = 'wide')
ranks_with_fips = ranks_with_fips_temp[order(ranks_with_fips_temp$color),]


for (i in 2:length(colnames(ranks_with_fips))){
  colnames(ranks_with_fips)[i] = gsub(' ','_',substr(colnames(ranks_with_fips)[i],11,100))
}

colnames(ranks_with_fips)[1] = 'rank'

save(ranks_with_fips, file = '~/flu_surveil_data/ranks_with_fips.Rda')

x = ranks_with_fips[,2]
y = as.data.frame( as.numeric(county_FIPS_xwalk[,'FIPS_code'] %in% x))
colnames(y) = colnames(ranks_with_fips)[2]

for (i in 3:ncol(ranks_with_fips)){
  new_cols = c(colnames(y),colnames(ranks_with_fips)[i])
  y = cbind(y, as.numeric(county_FIPS_xwalk[,'FIPS_code'] %in% ranks_with_fips[,i]))
  print(sum(as.numeric(county_FIPS_xwalk[,'FIPS_code'] %in% ranks_with_fips[,i])))
  colnames(y) = new_cols
}

y = cbind(county_FIPS_xwalk[,'FIPS_code'],y)
colnames(y)[1] = 'FIPS_code'
y$FIPS_code = format_fips(y$FIPS_code)

save(y, file = '~/flu_surveil_data/logistic_data/y.Rda')
