#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd


# ### 1. Read file in a list variable called `data`. Each line is read as a string.

# In[2]:
# set work directory
path = 'D:/Group Folder/SEQUENTIAL FILES/'

with open(path+"1 Data Initial Files/t201903854_membership.csv",encoding='UTF-8') as file:
    data = file.readlines()


# ### 2. Split each line in `data` using `||` as separator. Append the split values in a list variable called `new_data`. This variable is a list of list. 

# In[3]:


new_data = []


# In[4]:


for line in data:
    abc = line.split("||")
    new_data.append(abc)


# ### 3. Convert the list of lists `new_data` into a dataframe `df`. Mention the first row as header.

# In[5]:


df = pd.DataFrame(new_data[1:], columns=new_data[0])


# In[6]:


df.head()


# In[7]:


df.shape


# In[8]:


df.to_csv(path+"1 Data Initial Files/membership.csv",encoding='UTF-8', index=False)


# ### NOTE:
# 1. Install miniconda in R
# 2. Install reticulate package in R
# 3. Run pip install pandas in R terminal
# 4. Run `library(reticulate)`
# 5. Run `py_run_file('D:/Group Folder/Code/Read membership.py')` in R code. A new file will be created: `D:/Group Folder/Data/membership.csv`. If it already exists, it will be overwritten.
# 6. Read the file `membership = read.csv('D:/Group Folder/Data/membership.csv')`
