#!/usr/bin/env python
# coding: utf-8

# In[16]:


import itertools
import numpy
import pandas as pd

G = 256
k = 6

results = itertools.combinations(range(1,G+1),2)

# convert the combination iterator into a numpy array
comb_mat = numpy.array(list(results))

data = pd.DataFrame(comb_mat)

#data.to_excel("C:/Users/Usuario/Dropbox/Angeles-Gisela-Andrea/Data/output.xlsx")


# In[ ]:




