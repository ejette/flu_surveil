setwd("~/flu_surveil_data")

# get list of counties
load('counties.Rda')

# get list of cities
mmwr <- read.csv("mmwr.csv")
cities = count(mmwr[,c('city','state')])[,1:2]

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

# extract population for counties
#x = c('NY', 'AL','CT','DC')
#state.name[match(x, state.abb)]


# extract population for cities