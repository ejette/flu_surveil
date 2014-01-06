import pandas as pd
import csv

def build_wban_dict(input_file):
    input = open(input_file,'r')

    dict = {}
    line = input.readline()

    # index = 1 has WBAN
    # index = -1 has state

    line = input.readline()

    while line:
        line_strip = line.rstrip()
        line_ls = line_strip.rsplit()
        # if state is already in the dictionary, append the wban to the list of that state's wbans
        if line_ls[-1] in dict.keys():
            dict[line_ls[-1]].append(line_ls[1])
        # if it isn't in the dictionary, add it and start a list of wbans
        else:
            dict[line_ls[-1]] = [line_ls[1]]
        line = input.readline()
    
    input.close()
    return(dict)
    
wd = '/Users/jj/cdc_predict/climate_data/'
wbans = build_wban_dict(wd + 'us_wbans.csv')

def format_data(input_str):
    # process climate data
    import re
    # this will need to be changed if you are running this on a different machine of course :)
    wd = '/Users/jj/cdc_predict/climate_data/'

    input = open(wd + input_str +'.txt','r')
    output = open(wd + input_str +'_all.csv','w')

    line = input.readline()
    # remove new line character
    line_trim = line.rstrip()
    # remove spaces and special characters
    line_formatted = re.sub(r' |%|\\.|\\(|\\)','',line_trim)
    # split into list
    line_list = line_formatted.rsplit(',')

    # extract the indices in the list that we are interested in
    # comment the following line if you are processing 200708 to 2013 data
    #indices = [line_list.index('WbanNumber'), line_list.index('YearMonthDay'), line_list.index('DryBulbTemp'),line_list.index('RelativeHumidity')]
    # comment the following line if you are processing 2003 - 200708 data
    indices = [line_list.index('WBAN'), line_list.index('Date'), line_list.index('DryBulbFarenheit'),line_list.index('RelativeHumidity')]
    # take subset of line_list using the extracted indices
    output_list = list(line_list[i] for i in indices)
    output_line = ','.join(output_list) + '\n'
    output.write(output_line)

    line = input.readline()
    # read through the rest of the file and extract the variables of interest
    while line:   
        line = input.readline()
        line_trim = line.rstrip()
        # replace '-' with 'NA'
        line_na = re.sub('-','NA',line_trim)
        # trim white space
        line_format = re.sub(' ','',line_na)
        line_list = line_format.rsplit(',')
        #print line_list
        if len(line_list) > 1:
            output_list = list(line_list[i] for i in indices)
        # keep observations associated with Wban numbers (i.e., the weather stations in the regions we want)
        output_line = ','.join(output_list) + '\n'
        output.write(output_line)
        line = input.readline()

    # check if all weather stations are present in the data set
    input.close()
    output.close()
    
d = format_data('hourly200810')
f = pd.read_csv(wd + 'hourly200810_all.csv')

# list of all possible wbans in the data
data_wbans = pd.DataFrame(f['WBAN'])
# drop duplicates so it is easier to look through
no_dupes = data_wbans.drop_duplicates()

# first week we are looking at ended October 4, 2008

wban_ls = wbans['"VA"']
random_ls = random.sample(wban_ls,len(wban_ls))

wban_sub = []
j = 0

states = wbans.keys()
present_wbans = {}

for i in range(0,len(wbans)):
    wban_ls = wbans[states[i]]
    random_ls = random.sample(wban_ls,len(wban_ls))

    wban_sub = []
    j = 0
    while (len(wban_sub) != 5 and j < len(wban_ls)):
        if no_dupes.applymap(lambda x: x == int(random_ls[j])).any().any():
            wban_sub.append(random_ls[j])    
        j = j + 1
    present_wbans[states[i]] = wban_sub
    print states[i] + str(len(wban_sub))