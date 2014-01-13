setwd("~/flu_surveil_data")
library(plyr)
# get list of counties
load('counties.Rda')

# get list of cities
mmwr <- read.csv("mmwr.csv",stringsAsFactors = FALSE )
cities = count(mmwr[,c('city','state')])[,1:2]
save(cities, file = '~/flu_surveil_data/cities.Rda')
cities$city[cities$city == 'Boise' & cities$state == 'ID'] = 'Boise City'
cities$city[cities$city == 'Colo. Springs' & cities$state == 'CO'] = 'Colorado Springs'
cities$city[cities$city == 'Ft. Worth' & cities$state == 'TX'] = 'Fort Worth'
cities$city[cities$city == 'Honolulu' & cities$state == 'HI'] = 'Urban Honolulu'
cities$city[cities$city == 'New York City' & cities$state == 'NY'] = 'New York'

# need "Elizabeth, NJ", newark, mj 124,969

# load county information
county_pop <- read.csv("~/Downloads/PEP_2012_PEPANNRES (1)/PEP_2012_PEPANNRES.csv")
city_pop <- read.csv("~/Downloads/PEP_2011_PEPANRGCT.US23PR/PEP_2011_PEPANRGCT.US23PR.csv")

# load FIPS look up table
FIPS <- read.csv("~/Downloads/Census/FIPS_CountyName.csv", header=F)
colnames(FIPS) = c('FIPS_code', 'county_fips')

# make a variable in county that is formatted the same way as the FIPS county variable
counties$fips_format = paste(counties$county, ' County, ', counties$state, sep = '')
counties$fips_format= gsub('City County',"city", counties$fips_format)
counties$fips_format= gsub('De Kalb',"DeKalb", counties$fips_format)
counties$fips_format= gsub('Dekalb',"DeKalb", counties$fips_format)
counties$fips_format= gsub('De Witt County, TX',"DeWitt County, TX", counties$fips_format)
counties$fips_format= gsub('De Soto',"DeSoto", counties$fips_format)



for (i in 1:nrow(counties)){
  counties$fips_format[i][counties$state[i] == 'LA'] = paste(counties$county[i], ' Parish, ', counties$state[i], sep = '')
}

counties$fips_format = toupper(counties$fips_format)


FIPS$county_form = as.character(FIPS$county_fips)
FIPS$county_form[FIPS$county_form == 'Do\xb1a Ana County, NM'] = 'Dona Ana County, NM'
FIPS$county_form = toupper(FIPS$county_form)

counties$fips_format = counties$fips_format
counties$fips_format[counties$fips_format == 'ANCHORAGE COUNTY, AK'] = "ANCHORAGE MUNICIPALITY, AK"
counties$fips_format[counties$fips_format == 'DADE COUNTY, FL'] = "MIAMI-DADE COUNTY, FL"
counties$fips_format[counties$fips_format == 'DISTRICT OF COLUMBIA COUNTY, DC'] = "DISTRICT OF COLUMBIA, DC"
counties$fips_format[counties$fips_format == 'DU PAGE COUNTY, IL'] = "DUPAGE COUNTY, IL"
counties$fips_format[counties$fips_format == 'FAIRBANKS NORTH STAR COUNTY, AK'] = "FAIRBANKS NORTH STAR BOROUGH, AK"
counties$fips_format[counties$fips_format == 'JAMES CITY, VA'] = "JAMES CITY COUNTY, VA"
counties$fips_format[counties$fips_format == 'JUNEAU COUNTY, AK'] = "JUNEAU CITY AND BOROUGH, AK"
counties$fips_format[counties$fips_format == 'KENAI PENINSULA COUNTY, AK'] = "KENAI PENINSULA BOROUGH, AK"
counties$fips_format[counties$fips_format == 'LA SALLE COUNTY, IL'] = "LASALLE COUNTY, IL"
counties$fips_format[counties$fips_format == 'RADFORD COUNTY, VA'] = "RADFORD CITY, VA"
#counties$fips_format[counties$fips_format == 'SAINT CROIX ISLAND COUNTY, VI'] = 
counties$fips_format[counties$fips_format == 'SALEM COUNTY, VA'] = "SALEM CITY, VA"
#counties$fips_format[counties$fips_format == 'UNORGANIZED COUNTY, AK'] = "
counties$fips_format[counties$fips_format == 'VALDEZ CORDOVA COUNTY, AK'] = "VALDEZ-CORDOVA CENSUS AREA, AK"

county_FIPS = merge(counties, FIPS, by.x = "fips_format", by.y = "county_form")

cnt = counties$fips_format
f = FIPS$county_form
missing = cnt[!(cnt %in% f)]
# only two are missing now...one in alaska and the other in the V.I.

m_county = merge(county_pop, county_FIPS, by.x = 'GEO.id2', by.y = 'FIPS_code')

# cleaned 2010-2012 census data indexed by cbsa
census_2010_to_2012_raw<-read.csv("~/repos/cdc_flu_contest/data/humidity/raw/CBSA-EST2012-01.csv", header=F, stringsAsFactors = FALSE)
foo <- grepl('^[0-9]{5}$', census_2010_to_2012_raw[,1]) & (census_2010_to_2012_raw[,2]=='')
census_2010_to_2012 <- census_2010_to_2012_raw[foo,-c(2,4:5)]
colnames(census_2010_to_2012) <- c('cbsa','metro_name','2010','2011','2012')

# split up metro_name into cities and state
census_2010_to_2012$metro_name[463] = 'Canon City, CO'
census_2010_to_2012$metro_name[537] = 'Espanola, NM'

# split up further into separate cities and states
list <- strsplit(census_2010_to_2012$metro_name, ",")
df <- ldply(list)
colnames(df) <- c("cities", "state")
states_wide = as.data.frame(str_trim(str_split_fixed(df$state, '-', 4)), stringsAsFactors = FALSE)
colnames(states_wide) = c('state1','state2','state3','state4')

df$cities = gsub("--","-",df$cities)
cities_wide = as.data.frame(str_trim(str_split_fixed(df$cities, '-', 4)),stringsAsFactors = FALSE)
colnames(cities_wide) = c('city1','city2','city3','city4')
wide = cbind(states_wide, cities_wide)
census_city = cbind(census_2010_to_2012[,-2], wide)

# make data long
k = melt(census_city, id.vars = c('cbsa','2010','2011','2012','state1','state2','state3','state4'),measure.vars = c('city1','city2','city3','city4'))
l = melt(k, id.vars = c('cbsa','2010','2011','2012','variable','value'), measure.vars =  c('state1','state2','state3','state4'))
l = l[,-c(5,7)]
colnames(l) = c('cbsa', '2010', '2011','2012','city','state')
l$metro_name = paste(l$city, ', ', l$state, sep = '')

cities$metro_name = paste(cities$city,', ', cities$state, sep = '')

missing_cities = c("Elizabeth, NJ",
                   "Lowell, MA",
                   "Lynn, MA",
                   "Pasadena, CA",
                   "Paterson, NJ",
                   "Somerville, MA",
                   "Waterbury, CT",
                   "Yonkers, NY",
                   "Berkeley, CA",
                   "Fall River, MA",
                   "Gary, IN",
                   "Glendale, CA",
                   "New Bedford, MA")

missing_pops = c(124969,
                 106519,
                 90329,
                 137122,
                 146199,
                 75754,
                 110366,
                 195976,
                 112580,
                 88857,
                 80294,
                 191719,
                 95072)

empty = rep("",n)
missing_obs = cbind(cbind(cbind(cbind(cbind(cbind(empty, missing_pops),empty),empty),empty),empty),missing_cities)
colnames(missing_obs) = c('cbsa','2010','2011','2012','city','state','metro_name')
l = rbind(l,missing_obs)

m_cities = merge(cities, l, by = 'metro_name')

c = cities$metro_name
f = l$metro_name
missing = c[!(c %in% f)]


# calculate the humidity difference between the counties and the cities
mat1 = as.data.frame(rep(m_county[1,'respop72010'],122))
m_cities[,'2010'] = as.numeric(gsub(",","", m_cities[,'2010']))
mat2 = as.data.frame(rep(m_cities[1,'2010'],nrow(m_county)))

for (i in 2:nrow(m_county)){
  mat1 = cbind(mat1, rep(m_county[i,'respop72010'],122))
}

for (i in 2:nrow(m_cities)){
  mat2 = cbind(mat2, rep(m_cities[i,'2010'],nrow(m_county)))
}

mat1 = t(as.matrix(mat1))
mat2 = as.matrix(mat2)

pop_diff = mat2 - mat1

save(pop_diff, file = '/Users/jj/flu_surveil_data/humidity/pop_diff.Rda')
# extract population for counties
#x = c('NY', 'AL','CT','DC')
#state.name[match(x, state.abb)]


# extract population for cities