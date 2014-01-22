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