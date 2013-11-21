import re

wd = '/Users/jj/flu_surveil_data/'

input = open(wd +'county_dictionary.csv','r')
ranks_all = open(wd +'ranks_all.csv','r')
output = open(wd +'ranks_all_form.csv','w')

county_dict = {}

line = input.readline().rstrip()

while(line):
    line_list = line.split(',')
    county_dict[line_list[2]] = line_list[1]
    line = input.readline().rstrip()
county_dict['"VALDEZ CORDOVA"'] = '"CA-VALDEZ CORDOVA"'
county_dict['"VALDEZ-CORDOVA"'] = '"CA-VALDEZ CORDOVA"'
county_dict['"SANTA CRUZ AZ"'] = '"AZ-SANTA CRUZ"'
county_dict['"MIAMI DADE"'] = '"FL-MIAMI DADE"'
county_dict['"EL PASO TX"'] = '"TX-EL PASO"'
county_dict['"EL PASO CO"'] = '"CO-EL PASO"'
county_dict['"ST JOSEPH MI"'] = '"MI-ST JOSEPH"'
county_dict['"ST JOSEPH IN"'] = '"IN-ST JOSEPH"'
county_dict['"ST CLAIR MO"'] = '"MO-ST CLAIR"'
county_dict['"ST CLAIR IL"'] = '"IL-ST CLAIR"'
county_dict['"ST CLAIR MI"'] = '"MI-ST CLAIR"'
county_dict['"SANTA CRUZ CA"'] = '"CA-SANTA CRUZ"'
    
# output the column names
ranks_line = ranks_all.readline()
output.write(ranks_line)
 
ranks_line = ranks_all.readline().rstrip()
#ranks_line = re.sub(r'VALDEZ CORDOVA','VALDEZ-CORDOVA',ranks_line)
while(ranks_line):
    ranks_list = ranks_line.split(',')
    for i in range(2,len(ranks_list)):
        s = county_dict[ranks_list[i]]
        #print s
        q = s.split('-')
        q[1] = q[1].title()
        ranks_list[i] = '-'.join(q)
    out = ",".join(ranks_list)      
    ranks_line = ranks_all.readline().rstrip()
  #  ranks_line = re.sub(r'VALDEZ CORDOVA','VALDEZ-CORDOVA',ranks_line)
    output.write(out+'\n')
        
input.close()
ranks_all.close()
output.close()