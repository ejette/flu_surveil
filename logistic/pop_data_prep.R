setwd("~/flu_surveil_data")
library(plyr)
library(reshape)
library(stringr)
source('~/flu_surveil/logistic/format_fips.R')
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

# load county information
county_pop <- read.csv("~/Downloads/PEP_2012_PEPANNRES (1)/PEP_2012_PEPANNRES.csv", colClasses=c(rep("character",3),  rep("numeric",5)))
city_pop <- read.csv("~/Downloads/PEP_2011_PEPANRGCT.US23PR/PEP_2011_PEPANRGCT.US23PR.csv")

# load FIPS look up table
FIPS <- read.csv("~/Downloads/Census/FIPS_CountyName.csv", header=F, colClasses = c(rep("character",2)))
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
county_FIPS$county_fips = as.character(county_FIPS$county_fips)
#county_FIPS = rbind(c('UNORGANIZED COUNTY, AK','Unorganized','AK','','00998','Unorganized County', 'AK'), county_FIPS)
#county_FIPS = rbind(c('SAINT CROIX ISLAND COUNTY','Saint Croix Island','VI','','00999','Saint Croix Island County', 'VI'), county_FIPS)
county_FIPS_xwalk = county_FIPS[,c('county','state','FIPS_code')]
#save(county_FIPS_xwalk, file = '~/flu_surveil_data/county_FIPS_xwalk.Rda')

cnt = county_FIPS$fips_format
f = FIPS$county_form
missing = cnt[!(cnt %in% f)]
# only two are missing now...one in alaska and the other in the V.I.
county_FIPS$FIPS_code = format_fips(county_FIPS$FIPS_code)
#m_county = merge(county_pop, county_FIPS, by = 'num')

m_county = merge(county_pop, county_FIPS, by.x = 'GEO.id2', by.y = 'FIPS_code', all.y = TRUE)
m_county = m_county[,c('GEO.id2','county','state','respop72010')]

unorg_ak_2010_pop = 19621 # from a report by Alaska Labor Statistics
virgin_islands_2010_pop = 106405 # from the census website
m_county = rbind(m_county, c('00998','Unorganized','AK', unorg_ak_2010_pop))
m_county = rbind(m_county, c('00999','Saint Croix Island','VI', virgin_islands_2010_pop))

save(m_county, file = '~/flu_surveil_data/m_county.Rda')

# Extract City population data
# cleaned 2010-2012 census data indexed by cbsa
census_2010_to_2012_raw<-read.csv("~/repos/cdc_flu_contest/data/humidity/CBSA-EST2012-01.csv", header=F, stringsAsFactors = FALSE)
foo <- grepl('^[0-9]{5}$', census_2010_to_2012_raw[,1]) & (census_2010_to_2012_raw[,2]=='')
census_2010_to_2012 <- census_2010_to_2012_raw[foo,-c(2,4:5)]
colnames(census_2010_to_2012) <- c('cbsa','metro_name','2010','2011','2012')

# split up metro_name into cities and state
#census_2010_to_2012$metro_name[463] = 'Canon City, CO'
census_2010_to_2012$metro_name[census_2010_to_2012$metro_name == "Ca\xf1on City, CO"] = 'Canon City, CO'
census_2010_to_2012$metro_name[census_2010_to_2012$metro_name == "Espa\xf1ola, NM"]  = 'Espanola, NM'
#census_2010_to_2012$metro_name[537] = 'Espanola, NM'

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

n = length(missing_pops)
empty = rep("",n)
missing_obs = cbind(cbind(cbind(cbind(cbind(cbind(empty, missing_pops),empty),empty),empty),empty),missing_cities)
colnames(missing_obs) = c('cbsa','2010','2011','2012','city','state','metro_name')
l = rbind(l,missing_obs)

m_cities = merge(cities, l, by = 'metro_name')

c = cities$metro_name
f = l$metro_name
missing = c[!(c %in% f)]


# calculate the population difference between the counties and the cities
mat1 = as.data.frame(rep(as.numeric(m_county[1,'respop72010'],122)))
m_cities[,'2010'] = as.numeric(gsub(",","", m_cities[,'2010']))
colnames(mat1) = m_county[1,'GEO.id2']
mat2 = as.data.frame(rep(m_cities[1,'2010'],nrow(m_county)))

colnames(mat2) = paste(m_cities[1,'city.x'],'_',m_cities[1,'state.x'], sep = '')

for (i in 2:nrow(m_county)){
  new_cols = c(colnames(mat1), m_county[i,'GEO.id2'])
  mat1 = cbind(mat1, as.numeric(rep(m_county[i,'respop72010'],122)))
  colnames(mat1) = new_cols
}

for (i in 2:nrow(m_cities)){
  new_cols = c(colnames(mat2), paste(gsub(' ','_',m_cities[i,'city.x']),'_',m_cities[i,'state.x'], sep = ''))
  mat2 = cbind(mat2, rep(m_cities[i,'2010'],nrow(m_county)))
  colnames(mat2) = new_cols
}

mat1_transpose = t(as.matrix(mat1))
mat2_matrix = as.matrix(mat2)

pop_diff = as.data.frame(mat2_matrix - mat1_transpose)
colnames(pop_diff) = colnames(mat2)
fips = as.character(colnames(mat1))
pop_diff = cbind(format_fips(fips), pop_diff)
colnames(pop_diff)[1] = 'FIPS_id'
pop_diff$FIPS_id = as.character(pop_diff$FIPS_id)

save(pop_diff, file = '/Users/jj/flu_surveil_data/logistic_data/pop_diff.Rda')

