#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd


# ### 1. Read file in a list variable called `data`. Each line is read as a string.

# In[2]:

# set work directory
path = 'D:/Group Folder/SEQUENTIAL FILES/'

with open(path+"1 Data Initial Files/t201903854_individual_info.csv",encoding='UTF-8') as file:
    data = file.readlines()


# ### 2. Strip `\n` from the end of each line in `data`. Append the modified line to a new list variable called `new_data_temp`.

# In[3]:


new_data_temp = []
for line in data:
    line = line.rstrip("\n")
    new_data_temp.append(line)


# ### 3. Split each line in `new_data_temp` using `||` as separator. Append the split values in a list variable called `new_data`. This variable is a list of list. 

# In[4]:


new_data = []


# In[5]:


for line in new_data_temp:
    abc = line.split("||")
    new_data.append(abc)


# ### 4. Convert the list of lists `new_data` into a dataframe `df`. Mention the first row as header.

# In[6]:


df = pd.DataFrame(new_data[1:], columns=new_data[0])


# In[7]:


df.head()


# In[8]:


df.shape


# In[9]:


df.to_csv(path+"1 Data Initial Files/individual_info.csv",encoding='UTF-8', index=False)


# ### NOTE:
# 1. Install miniconda in R
# 2. Install reticulate package in R
# 3. Run `pip install pandas` in R terminal
# 4. Run `library(reticulate)`
# 5. Run `py_run_file('D:/Group Folder/Code/Read individual_info.py')` in R code. A new file will be created: `D:/Group Folder/Data/individual_info.csv`. If it already exists, it will be overwritten.
# 6. Read the file `individual_info = read.csv('D:/Group Folder/Data/individual_info.csv')`
