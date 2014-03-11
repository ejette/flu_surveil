load("~/flu_surveil_data/county_FIPS_xwalk.Rda")
county = county_FIPS_xwalk

county$region = 0

region1 = c('MA','CT','RI','ME','NH','VT')
region2 = c('NY','NJ','VI')
region3 = c('DE','DC','MD','PA','VA','WV')
region4 = c('AL','FL','GA','KY','MS','NC','SC','TN')
region5 = c('OH','IL','MI','IN','WI','MN')
region6 = c('TX','LA','OK','AR','NM')
region7 = c('IA','KS','MO','NE')
region8 = c('CO','MT','ND','SD','UT','WY')
region9 = c('AZ','CA','HI','NV')
region10 = c('WA','OR','AK','ID')

all_regions = list(region1, region2, region3, region4, region5, region6, region7, region8, region9, region10)
all_regions_list = c(region1, region2, region3, region4, region5, region6, region7, region8, region9, region10)
st = data.frame(all_regions_list, data.frame(matrix(0,ncol = 10, nrow = length(all_regions_list))))
loop_len = length(all_regions)
colnames(st) = c('st','region1', 'region2', 'region3', 'region4', 'region5', 'region6', 'region7', 'region8', 'region9', 'region10')

county_FIPS_xwalk$region = 0
for (i in 1:loop_len){
  st[st$st %in% all_regions[[i]], i+1] = 1
  county_FIPS_xwalk$region[county_FIPS_xwalk$state %in% all_regions[[i]]] = i
} 

hhs = county_FIPS_xwalk[,c('FIPS_code','region')]
#hhs = merge(county_FIPS_xwalk, st, by.x = 'state', by.y = 'st')
#hhs = hhs[,3:12]
#hhs = unique(hhs)
save(hhs, file = '~/Dropbox/122citites/hhs.Rda')

# code adjacent regions as 2
st$region1[st$region2 == 1] = 2
st$region2[st$region1 == 1 | st$region3 == 1] = 2
st$region3[st$region2 == 1 | st$region4 == 1 | st$region5 == 1] = 2
st$region4[st$region3 == 1 | st$region5 == 1 | st$region6 == 1 | st$region7 == 1] = 2
st$region5[st$region3 == 1 | st$region4 == 1 | st$region7 == 1 | st$region8 == 1] = 2
st$region6[st$region4 == 1 | st$region7 == 1 | st$region8 == 1 | st$region9 == 1] = 2
st$region7[st$region4 == 4 | st$region5 == 1 | st$region6 == 1 | st$region8 == 1] = 2
st$region8[st$region5 == 4 | st$region7 == 1 | st$region6 == 1 | st$region9 == 1 | st$region10 ==1] = 2
st$region9[st$region6 == 1 | st$region8 == 1 | st$region10 == 1] = 2
st$region10[st$region8 == 1 | st$region9 == 1] = 2

# merge onto xwwalk
hhs_adj = merge(county_FIPS_xwalk, st, by.x = 'state', by.y = 'st')
hhs_adj = hhs_adj[,c(3,5:14)]
hhs_adj = unique(hhs_adj)
save(hhs_adj, file = '~/Dropbox/122citites/hhs_adj.Rda')
