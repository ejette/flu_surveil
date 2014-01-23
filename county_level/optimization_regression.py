import time
start_time = time.time()
import pandas as pd
import numpy as np
 
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score

def find_max_r2(deaths, covar_df, candidates_df):
    # this function runs regression for the formula deaths ~ covar_df + 
    # one variable from candidates_df at a time
    # then it finds the candidate covariate that gave the highest R^2
    # and returns the R^2 value associated with the highest R^2 
    # and the index of the candidate that one
    import pandas as pd
    import numpy as np
 
    from sklearn.linear_model import LinearRegression
    from sklearn.metrics import r2_score
    
    cols = candidates_df.columns.values
    candidates_r2 = []
    for i in range(0, len(cols)):
        if len(covar_df) != 0:
            test_df = pd.DataFrame.copy(covar_df)
            # if there are covariates in the covar_df, add the candidate covariate
            # as a new column
            # add a column with the candidate covariate
            test_df[cols[i]] = candidates_df[cols[i]]
            #test_cols = test_df.columns.values
            #print len(test_cols)
        else:
            # if the covar_df is empty, make the covariate dataframe just equal
            # to the candidate covariate
            test_df = candidates_df[cols[i]][:, np.newaxis]
            # run the regression
        lr = LinearRegression()
        fit = lr.fit(test_df, deaths)
        # get the R^2 value and add it to the list of candidate R^2s
        candidates_r2.append(lr.score(test_df[:], deaths[:]))
        # find the maximum R^2, the index associated with that covariate, and the
        # name of the column
    max_r2 = max(candidates_r2)
    max_r2_index = candidates_r2.index(max(candidates_r2))
    max_r2_name = cols[max_r2_index]
    return {'max_r2':max_r2 ,'max_r2_index': max_r2_index, 'max_r2_name' : max_r2_name}

def optimize(deaths, candidates_df, covar_n):
    # initilize empty dataframe to put the winning covariates in
    import pandas as pd
    import numpy as np
    from find_max_r2 import find_max_r2
 
    from sklearn.linear_model import LinearRegression
    from sklearn.metrics import r2_score

    covar_df = pd.DataFrame()
    # keep a list of the R^2 values
    r2_list = []
    # find the first covariate
    r2_dict = find_max_r2(deaths, covar_df, candidates_df)
    r2_list.append(r2_dict['max_r2'])
    r2_index = r2_dict['max_r2_index']
    max_r2_name = r2_dict['max_r2_name']
    # initialize covar_df with the first chosen covariate
    covar_df = pd.DataFrame(candidates_df[max_r2_name])
    # delete the covariate from the candidates dataframe   
    del candidates_df[max_r2_name] 

    # find the rest of the covariates
    for i in range(1,covar_n):
        #print i
        r2_dict = find_max_r2(deaths, covar_df, candidates_df)
        r2_list.append(r2_dict['max_r2'])
        r2_index = r2_dict['max_r2_index']
        max_r2_name = r2_dict['max_r2_name']
        # initialize covar_df with the first chosen covariate
        covar_df[max_r2_name] = candidates_df[max_r2_name]
        # delete the covariate from the candidates dataframe   
        del candidates_df[max_r2_name] 

    return {'covars':covar_df, 'r2_list' : r2_list}
    
wd = '/Users/jj/flu_surveil_data/'

candidates_df = pd.read_csv(wd+"ili_wide_cnty_zeros_FIPS.csv", sep=" ")
del candidates_df['date']
# read in predicand
deaths = pd.read_csv(wd+"flu_gold.csv", sep = " ")
deaths = deaths['deaths']

covar_n = 50

results = optimize(deaths, candidates_df, covar_n)

#for i in len(results['covars']):
#    print results['covars'][i], results['r2_list'][i]
print results['covars']
print results['r2_list']

print "My program took", time.time() - start_time, "to run"