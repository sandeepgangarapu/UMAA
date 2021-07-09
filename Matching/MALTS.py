import pymalts2
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
np.random.seed(0)
sns.set()


df = pd.read_csv('files/input_malts.csv',index_col=0)
print(df.shape)
df.head()
