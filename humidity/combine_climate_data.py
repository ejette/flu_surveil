# this program combines all the preprocessed csvs into one file
# open output
wd = '/Users/jj/flu_surveil_data/humidity/'

output = open(wd + 'all_humidity.csv','w')

# write variable names in first line
output.write('wban,date,dry_bulb_temp,relative_humidity,state\n')

start_yr = 2008
end_yr = 2013

MM = ['01','02','03','04','05','06','07','08','09','10','11','12']
# we only need 10, 11, and 12 months for 2008
MM2 = ['10','11','12']

for YYYY in range(start_yr,end_yr):
    if YYYY == 2008:
        for i in range(0,len(MM2)):
            # open input
            input_str = 'hourly'+str(YYYY)+str(MM2[i])+'.csv'
            input = open(wd + input_str,'r')
            # skip first line
            line = input.readline()
            line = input.readline()
            while line:
            # write to output
                if line != 'NA,NA,NA,NA':
                    output.write(line)
                    line = input.readline()
            input.close()
    else:
        for i in range(0,len(MM)):
            # open input
            input_str = 'hourly'+str(YYYY)+str(MM[i])+'.csv'
            input = open(wd + input_str,'r')
            # skip first line
            line = input.readline()
            line = input.readline()
            while line:
            # write to output
                if line != 'NA,NA,NA,NA':
                    output.write(line)
                    line = input.readline()
            input.close()
        
output.close()