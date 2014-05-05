import time
start_time = time.time()
import pandas as pd 
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score

wd = '/Users/jette/flu_surveil_data/'

# ILInet data
candidates_df = pd.read_csv(wd+"ili_flu_seasons.csv", sep=" ")
del candidates_df['date']
# Predicand
deaths_regions = pd.read_csv(wd+"flu_gold_regions.csv", sep = " ")
del deaths_regions['date']
# Optimization results
covars_df = pd.read_csv(wd+"covars_cnty_to_region_w_zeros.csv", sep=",", dtype = object)
n_provider = 10
# take out the top ten providers for each region
top_providers = covars_df.ix[0:(n_provider-1),1:]

# get colnames for the possible counties,regions, a gold standard timeseries 
counties = candidates_df.columns.values
fips = [x[6:] for x in counties]
candidates_df.columns = fips
regions = covars_df.columns.values
deaths_names = deaths_regions.columns.values

output = open(wd + 'heatmap_validation_10_providers.csv','w')

for i in range(1,len(regions)):
    print 'i ',i
    # build a df of ILINet for the top ten providers
    # initialize the dataframe
    candidates_df[top_providers.ix[0,regions[i]]]
    df = pd.DataFrame(candidates_df[top_providers.ix[0,regions[i]]], columns = [top_providers.ix[0,regions[i]]])
    
    # add the rest of the providers
    for j in range(1,n_provider):
        print 'j ', j
        df[top_providers.ix[j,regions[i]]] = candidates_df[top_providers.ix[j,regions[i]]]

    # do the national
    for k in range(0,len(deaths_names)):
        print 'k ', k
        # extract the gold time series
        deaths = deaths_regions[deaths_names[k]]
        # run the regression
        lr = LinearRegression()
        fit = lr.fit(df, deaths)
        print regions[i], deaths_names[k], lr.score(df[:], deaths[:])
        line = regions[i] + ',' + deaths_names[k] + ',' + str(lr.score(df[:], deaths[:])) + '\n'
        output.write(line)

output.close()
