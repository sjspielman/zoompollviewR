import re
import pprint

date_pattern = re.compile(r'"[A-Za-z]{3,4} \d{1,2}, \d{4} \d{2}:\d{2}:\d{2}",') #"Sep 14, 2020 14:13:31"


def clean_raw_zoom_csv(input_file, num_questions):

    with open(input_file, "r") as f:
        raw_poll = f.readlines()[1:]

    single_lines = []
    single_line = ""
    for line in raw_poll:
        if line == "\n":
            continue
        else:
            line_nodate = re.sub(date_pattern, "", line).strip()
            if re.search("^\d+,", line_nodate):
                at_start = True
            else:
              at_start = False
            if at_start is True:
                if single_line != "":
                    single_lines.append(single_line)
                    single_line = ""
                single_line = line_nodate
            else:
                single_line += line_nodate

    # Some of the names have to be reformatted to remove a comma
    ## "Last, First M"

    qa_header = ""
    for i in range(1, num_questions + 1):
        qa_header += "q"+str(i) + ",a" + str(i) + ","
    final_string = "name,email," + qa_header.rstrip(",") + "\n"

    for single_line in single_lines:

        # Remove the opening number
        no_num_line = re.sub("^\d+,", "", single_line)
    
        # Search for student names w/ comma
        find_comma_name = re.search('^"([\w, ]+)",', no_num_line)
        if find_comma_name:
            comma_name = find_comma_name.group(1).split(",")
            new_name = comma_name[1].strip() + " " + comma_name[0].strip() + ","
            no_num_line = re.sub('^"[\w, ]+",', new_name, no_num_line)
        #single_line_clean_names.append(no_num_line.rstrip(","))
        #print(no_num_line.rstrip(",") + "\n")
        final_string += no_num_line.rstrip(",") + "\n"
    #print(single_line_clean_names[1])
    
    
    return(final_string.strip())

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
