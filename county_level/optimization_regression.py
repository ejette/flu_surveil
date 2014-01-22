import pandas as pd
import numpy as np
#import matplotlib.pylab as plt
 
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score

wd = '/Users/jj/flu_surveil_data/'

# read in predictor data
X = pd.read_csv(wd+"ili_wide_cnty_zeros_FIPS.csv", sep=" ")
# get a list of the columns 
cols = list(X.columns.values)

# read in predicand
y = pd.read_csv(wd+"flu_gold.csv", sep = " ")

ili1 = X['total.00998'][:, np.newaxis]
deaths = y['deaths']
ili2 = X[cols[2]][:, np.newaxis]

r2 = []

lr = LinearRegression()
lr.fit(ili1, deaths)
r2.append(lr.score(ili1[:], deaths[:]))

lr = LinearRegression()
lr.fit(ili2, deaths)
r2.append(lr.score(ili2[:], deaths[:]))

r2.index(max(r2))

def pick_next_covar(y, covar_df, candidates_df):
    # this function runs regression for the formula deaths ~ covar_df + 
    # one variable from candidates_df at a time
    # then it finds the candidate covariate that gave the highest R^2
    # and returns the R^2 value associated with the highest R^2 
    # and the index of the candidate that one
    
    return {'r2':max_r2 ,'r2_index': r2_index}

def make_covar_df(r2_index, covar_df, candidates_df):
    
    return updated_covar_df

def delete_col(r2_index, candidates_df):
    
    return updated_candidates_df

def optimize(deaths, candidate_covars_df, covar_n):
    
    return {'covars':covar_df, 'r2_list' : r2_list}