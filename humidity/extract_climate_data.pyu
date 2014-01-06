wban_dict = {'"AK"': ['25616', '46407', '26425', '26514', '46403'],
 '"AL"': ['63847', '53843', '93806', '63862', '63898'],
 '"AR"': ['23904', '13977', '53954', '13964', '3918'],
 '"AZ"': ['3103', '3185', '23160', '93084', '3196'],
 '"CA"': ['4222', '23161', '3167', '23188', '23254'],
 '"CO"': ['3016', '94082', '3069', '3068', '93009'],
 '"CT"': ['64707', '14752', '14707', '14740', '94702'],
 '"DC"': ['13705', '13743'],
 '"DE"': ['13707', '13781', '13764'],
 '"FL"': ['12873', '12895', '12842', '12897', '92822'],
 '"GA"': ['3832', '3821', '63848', '53817', '13860'],
 '"HI"': ['21504', '22501', '22536', '22516', '22521'],
 '"IA"': ['94908', '4908', '4904', '4977', '14972'],
 '"ID"': ['24131', '4135', '94182', '94194', '24106'],
 '"IL"': ['4890', '63814', '4894', '54831', '54811'],
 '"IN"': ['3893', '53866', '54809', '4807', '14827'],
 '"KS"': ['13996', '23064', '13940', '23065', '3997'],
 '"KY"': ['53841', '3849', '53803', '63815', '93820'],
 '"LA"': ['93915', '12936', '12968', '53917', '13957'],
 '"MA"': ['94723', '54768', '54777', '4780', '64708'],
 '"MD"': ['93720', '13701', '3732', '13752', '3749'],
 '"ME"': ['14616', '4836', '94644', '14607', '94709'],
 '"MI"': ['54813', '94836', '54826', '14847', '54904'],
 '"MN"': ['4941', '4939', '4943', '94963', '4999'],
 '"MO"': ['53901', '53916', '14938', '3938', '3935'],
 '"MS"': ['23803', '63852', '3996', '13865', '63808'],
 '"MT"': ['94028', '94055', '24146', '4140', '4130'],
 '"NC"': ['3810', '13748', '93796', '93783', '63821'],
 '"ND"': ['94014', '14914', '24012', '94925', '94011'],
 '"NE"': ['14905', '14949', '94079', '14939', '4935'],
 '"NH"': ['54791', '94765', '14745', '54736', '14710'],
 '"NJ"': ['54779', '94741', '54743', '54785', '14792'],
 '"NM"': ['23048', '93092', '23078', '3048', '93045'],
 '"NV"': ['23169', '93102', '24172', '3160', '53138'],
 '"NY"': ['14757', '64758', '64756', '14714', '14748'],
 '"OH"': ['14895', '4848', '14821', '14825', '93812'],
 '"OK"': ['13969', '93940', '53933', '93986', '93950'],
 '"OR"': ['24162', '4128', '24221', '24285', '24152'],
 '"PA"': ['14777', '14793', '14751', '14737', '54723'],
 '"RI"': ['14787', '64710', '94793', '14765', '54752'],
 '"SC"': ['13880', '13849', '13744', '53850', '93846'],
 '"SD"': ['94993', '14929', '24025', '94056', '94037'],
 '"TN"': ['3811', '13891', '3847', '13897', '13827'],
 '"TX"': ['3058', '53910', '12980', '13904', '93985'],
 '"UT"': ['94128', '53149', '24126', '23159', '4134'],
 '"VA"': ['93797', '63806', '3710', '93728', '93738'],
 '"VT"': ['54740', '54771', '54781', '14742', '94705'],
 '"WA"': ['24233', '94282', '24141', '94129', '94263'],
 '"WI"': ['94854', '54908', '14837', '4803', '94818'],
 '"WV"': ['3872', '13736', '63879', '3860', '53801'],
 '"WY"': ['24027', '24062', '94053', '24057', '24166']}
 
keys = wban_dict.keys()
 
# this file extracts relevant station data from hourlyYYYYMM.txt files
# It extracts: wban (station id), date, dry bulb temperature, and relative humidity (this is a percentage)

def list_diff(a, b):
    # this function finds items in list a that are not in list b
    b = set(b)
    return [aa for aa in a if aa not in b]
        
def format_data(input_str, dict):
    # process climate data
    import re
    # this will need to be changed if you are running this on a different machine of course :)
    wd = '/Users/jj/cdc_predict/climate_data/'
    out_dir = '/Users/jj/flu_surveil_data/humidity/'
    # station codes
    keys = dict.keys()
    
    dict_ls = []
    for i in range(0,len(dict)):
        for j in range(0,len(dict[keys[i]])):
            dict_ls.append(dict[keys[i]][j])
    station_dict = {}

    input = open(wd + input_str +'.txt','r')
    output = open(out_dir + input_str +'.csv','w')

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
    output_line = ','.join(output_list) +',state' + '\n'
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
        if output_list[0] in dict_ls:
            station_dict[output_list[0]] = 1
            output_line = ','.join(output_list) + ',' + [k for k, v in wban_dict.iteritems() if output_list[0] in v][0] + '\n'
            output.write(output_line)
        line = input.readline()

    # check if all weather stations are present in the data set
    #print input_str + ' ' + str(len(station_dict))
    #if len(station_dict) < 250:
     #   print list_diff(dict_ls, station_dict.keys())

    station_dict = {}
    input.close()
    output.close()
    
# year range you want to process
format_data('hourly200810', wban_dict)


start_yr = 2008
end_yr = 2014

# months you want to process for each year
# Note: this needs to be manipulated a bit if you're processing 2007 data because they switched variable names around
#MM = ['01','02','03','04','05','06','07','08','09','10','11','12']
#MM = ['08','09','10','11','12']

# run preprocessing on each of the text files
#for YYYY in range(start_yr,end_yr):
 #   for i in range(0,len(MM)):
  #      format_data('hourly'+str(YYYY)+str(MM[i]))

format_data('hourly201312')# this file extracts relevant station data from hourlyYYYYMM.txt files
# It extracts: wban (station id), date, dry bulb temperature, and relative humidity (this is a percentage)
