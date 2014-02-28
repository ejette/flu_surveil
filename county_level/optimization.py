import time
start_time = time.time()
import pandas as pd
import numpy as np
import re
import os
 
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score

os.chdir('/Users/jette/repos/flu_surveil/county_level/')
from optimization_regression import *
os.chdir('/Users/jette/')

wd = '/Users/jj/flu_surveil_data/'

#candidates_df = pd.read_csv(wd+"ili_wide_cnty_zeros_FIPS.csv", sep=" ")
candidates_df = pd.read_csv(wd+"ili_flu_seasons.csv", sep=" ")
del candidates_df['date']
#del candidates_df['row_num']

# read in predicand
deaths_cities = pd.read_csv(wd+"flu_gold_regions.csv", sep = " ")
# remove the date column
del deaths_cities['date']
#del deaths_cities['row_num']

cities = deaths_cities.columns.values
city_names = ['region_' + x[7:] for x in cities]
#city_names = [re.sub('\\.\\.', '\\.', x) for x in city_names]
#city_names = [re.sub(' ', '_', x) for x in city_names]

deaths = deaths_cities[cities[0]]
covar_n = 50
results = optimize(deaths, candidates_df, covar_n)

# create a dataframe with the cumulative r^2 values
r2_df = pd.DataFrame({city_names[0]: results['r2_list']})
# create a dataframe with the county FIPS codes that the optimization picked
covar_cols = results['covars'].columns.values
covar_names = [x[6:] for x in covar_cols]
covars_all_df = pd.DataFrame({city_names[0]: covar_names})

n = len(city_names)
for i in range(1,n):
    print i
    deaths = deaths_cities[cities[i]]
    # check to see if there are missing values and remove them from the 
    # predictor and predicand
    present_indices = deaths.index[deaths.isnull() == False]
    deaths = deaths.ix[present_indices]
    #candidates_df = pd.read_csv(wd+"ili_wide_cnty_zeros_FIPS.csv", sep=" ")
    candidates_df = pd.read_csv(wd+"ili_flu_seasons.csv", sep=" ")
    del candidates_df['date']
    #del candidates_df['row_num']
    candidates_df = candidates_df.ix[present_indices,:]
    results = optimize(deaths, candidates_df, covar_n)
    r2_df[city_names[i]] = results['r2_list']
    covar_cols = results['covars'].columns.values
    covar_names = [x[6:] for x in covar_cols]
    covars_all_df[city_names[i]] = covar_names
    covars_all_df.to_csv(wd + 'covars_cnty_to_region_w_zeros.csv')
    r2_df.to_csv(wd + 'r2s_cnty_w_zeros.csv')



#covars_all_df.to_csv(wd + 'covars_cnty_w_zeros.csv')
#r2_df.to_csv(wd + 'r2s_cnty_w_zeros.csv')

print "My program took", time.time() - start_time, "to run"