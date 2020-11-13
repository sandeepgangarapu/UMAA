#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import re


# ### 1. Read file in a list variable called `data`. Each line is read as a string.

# In[2]:

# set work directory
path = 'D:/Group Folder/SEQUENTIAL FILES/'


with open(path+"1 Data Initial Files/t201903854_academic.csv",encoding='UTF-8') as file:
    data = file.readlines()


# ### 2. Strip `\n` from the end of each line in `data`. Append the modified line to a new list variable called `new_data_temp`.

# In[3]:


new_data_temp = []


# In[4]:


for line in data:
    line = line.rstrip("\n")
    new_data_temp.append(line)


# ### 3. Split each line in `new_data_temp` using regular expression `',(?=\S)'` as separator. Append the split values in a list variable called `new_data`. This variable is a list of list. 

# In[5]:


new_data = []
for line in new_data_temp:
    abc = re.split(',(?=\S)',line)
    new_data.append(abc)


# ### 4. Convert the list of lists `new_data` into a dataframe `df`.

# In[6]:


df = pd.DataFrame(new_data)


# ### 5. Data Manipulation

# In[7]:


df = df.iloc[:,:9]


# In[8]:


df.columns = df.iloc[0]


# In[9]:


df = df[1:]


# In[10]:


df.shape


# In[16]:


df.to_csv(path+"1 Data Initial Files/academic.csv", index=False)


# ### NOTE:
# 1. Install miniconda in R
# 2. Install reticulate package in R
# 3. Run `pip install pandas` in R terminal
# 4. Run `library(reticulate)`
# 5. Run `py_run_file('D:/Group Folder/Code/Read academic.py')` in R code. A new file will be created: `D:/Group Folder/Data/academic.csv`. If it already exists, it will be overwritten.
# 6. Read the file `academic = read.csv('D:/Group Folder/Data/academic.csv')`
