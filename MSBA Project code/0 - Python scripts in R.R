## set working directory
setwd('D:/Group Folder/SEQUENTIAL FILES')


## Libraries used
library(reticulate)


## Academic csv transformation
py_run_file('0 - Code/Read academic.py')

## Membership csv transformation
py_run_file('0 - Code/Read membership.py')

## Individual_info csv transformation
py_run_file('0 - Code/Read individual_info.py')

## Events csv transformation
py_run_file('0 - Code/Read events.py')


