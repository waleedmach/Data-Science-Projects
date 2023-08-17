#!/usr/bin/env python
# coding: utf-8

# In[23]:


import pandas as pd


# In[24]:


import sklearn


# In[25]:


import numpy as np
from IPython.core.interactiveshell import InteractiveShell


# In[26]:


import matplotlib.pyplot as plt


# In[27]:


import seaborn as sns


# In[28]:


from matplotlib import gridspec


# Import data and begin Exploratory Data Analysis

# In[29]:


data = pd.read_csv(r"C:\Users\Admin\Documents\creditcard.csv")


# In[30]:


#view data 
data.head()


# In[31]:


#examine data shape
print(data.shape)


# In[32]:


#describe variables and variances of the data's attributes
print(data.describe())


# In[33]:


#Check for imbalances in the data - especially the "Class" indicating whether fraud was present or not. 

fraud = data[data['Class'] == 1]
valid = data[data['Class'] == 0]

print(len(fraud))
print(len(fraud) + len(valid))

print((len(fraud)/float(len(valid))))


# Only 0.17% of our incidences are fraud, whereas over 99.82% are valid transactions. 
# 

# In[34]:


#Examine what the distribution of transactions of fraud transactions behave like
print('Amount details of the fraudulent transaction')
fraud.Amount.describe()


# Compare this with the distributions of valid transactions

# In[35]:


#Examine what the distribution of transactions of fraud transactions behave like
print('Amount details of the valid transaction')
valid.Amount.describe()


# In[36]:


# Correlation matrix - Lets analyze which features are more correlated with each other. 
corrmatrix = data.corr()
fig = plt.figure(figsize = (10, 8))
sns.heatmap(corrmatrix, vmax = .9, square = False)
plt.show()


# From this we can tell that most features do no correlate to each other for the most part, with a couple of exceptions. 
# For example, V2 and V5 are highly negatively correlated with the feature called Amount, whereas V20 is positively correlated.

# Now divide variables and the class features so that we may begin training our model. 

# In[37]:


# dividing the X and the Y from the dataset
X = data.drop(['Class'], axis = 1)
Y = data["Class"]
print(X.shape)
print(Y.shape)
# getting just the values for the sake of processing 
# (its a numpy array with no columns)
xData = X.values
yData = Y.values


# Now we split the data into two groups, for training our model, and then testing it. The Scikitlearn library has functions that this can be done with extremely easy. 

# In[38]:


from sklearn.model_selection import train_test_split
# Split the data into training and testing sets
xTrain, xTest, yTrain, yTest = train_test_split(
        xData, yData, test_size = 0.2, random_state = 42)


# In[39]:


print(xTrain.shape)


# In[40]:


print(yTrain.shape)


# In[41]:


print(xTest.shape)


# In[42]:


print(yTest.shape)


# In[45]:


# Building the Random Forest Classifier (RANDOM FOREST)
from sklearn.ensemble import RandomForestClassifier
# random forest model creation
rfc = RandomForestClassifier()
rfc.fit(xTrain, yTrain)
# predictions
yPred = rfc.predict(xTest)


# In[47]:


# Evaluating the classifier
# printing every score of the classifier
# scoring in anything
from sklearn.metrics import classification_report, accuracy_score 
from sklearn.metrics import precision_score, recall_score
from sklearn.metrics import f1_score, matthews_corrcoef
from sklearn.metrics import confusion_matrix


# In[48]:


n_outliers = len(fraud)
n_errors = (yPred != yTest).sum()
print("The model used is Random Forest classifier")


# In[49]:


acc = accuracy_score(yTest, yPred)
print("The accuracy is {}".format(acc))


# In[50]:


prec = precision_score(yTest, yPred)
print("The precision is {}".format(prec))


# In[51]:


rec = recall_score(yTest, yPred)
print("The recall is {}".format(rec))


# In[52]:


f1 = f1_score(yTest, yPred)
print("The F1-Score is {}".format(f1))


# In[55]:


# printing the confusion matrix
LABELS = ['Normal', 'Fraud']
conf_matrix = confusion_matrix(yTest, yPred)
sns.heatmap(conf_matrix, xticklabels = LABELS, 
            yticklabels = LABELS, annot = True, fmt ="d");
plt.title("Confusion matrix")
plt.ylabel('True class')
plt.xlabel('Predicted class')
plt.show()


# In[ ]:





# In[ ]:





# In[ ]:




