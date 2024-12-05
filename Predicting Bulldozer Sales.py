#!/usr/bin/env python
# coding: utf-8

# # Predicting the Sale Price of Bulldozers Using Machine Learning
# 
# ## 1. Problem Definition 
# 
# How well can we predict the sale price of a bulldozer given its charecteristics, and past sale prices? 
# 
# ## 2. Data 
# 
# Downloaded from Bluebook from Kaggle Bulldozer competition. 
# 
# ## 3. Evaluation 
# 
# RMLSE - Root Mean Squared Log Error 
# 
# ## 4. Features
# 
# Kaggle provides a data dictionary detailing all of the features. 

# In[2]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import sklearn


# In[4]:


# Import training and validation sets

df = pd.read_csv("Bulldozer ML Project/TrainAndValid.csv")


# In[5]:


df.head()


# In[6]:


df.info()


# In[8]:


df.isna().sum()


# In[10]:


fig, ax = plt.subplots()
ax.scatter(df["saledate"][:1000], df["SalePrice"][:1000])


# In[11]:


# Use a histogram instead
df.SalePrice.plot.hist()


# #### Parsing Dates
# When we work with time series data, we want to enrich the time and date component as much as possible. We can do that by telling pandas which of our columns has dates in it using the `parse_dates` parameter.  

# Import data again but this time parse dates 

# In[17]:


df = pd.read_csv("Bulldozer ML Project/TrainAndValid.csv",
                low_memory=False, 
                parse_dates=["saledate"])


# In[18]:


df


# In[19]:


df.saledate.dtype


# In[20]:


df.saledate[:1000]


# In[21]:


fig, ax = plt.subplots()
ax.scatter(df["saledate"][:1000], df["SalePrice"][:1000])


# In[22]:


df.head()


# In[24]:


df.head().T # Can see all columns this way. 


# In[25]:


df.saledate.head(25)


# ### Sort DF by saledate
# 
# When working with Timeseries data, it is a good idea to sort it by date. 

# In[27]:


df.sort_values(by=["saledate"], inplace=True, ascending=True)
df.saledate.head(20)


# In[28]:


df.head()


# ### Make a copy of the orignal DataFrame
# We make a copy of the orignal dataframe, so when we manipulate the copy, we still have the orignal data

# In[30]:


df_tmp=df.copy()


# In[31]:


df_tmp


# ### Feature Engineering - Add datetime 
# 

# In[33]:


df_tmp["saleYear"] = df_tmp.saledate.dt.year
df_tmp["saleMonth"] = df_tmp.saledate.dt.month
df_tmp["saleDay"] = df_tmp.saledate.dt.day
df_tmp["saleDayOfWeek"] = df_tmp.saledate.dt.dayofweek
df_tmp["saleDayOf Year"] =  df_tmp.saledate.dt.dayofyear


# In[34]:


df_tmp.head().T


# In[37]:


# Now we've enriched our DataFrame with date time features, we can remove 'saledate'
df_tmp.drop("saledate", axis = 1, inplace = True)


# In[38]:


# Check values of different columns
df_tmp.state.value_counts()


# # 5. Modelling 
# - Model driven EDA
# - Convert strings to numbers by first converting them into pandas categories.
# 

# In[40]:


pd.api.types.is_string_dtype(df_tmp["UsageBand"])


# In[41]:


# Find columns which contain strings 
for label, content in df_tmp.items():
    if pd.api.types.is_string_dtype(content):
        print(label)


# In[42]:


# This will turn all of the string values into category values
for label,content in df_tmp.items():
    if pd.api.types.is_string_dtype(content):
        df_tmp[label] = content.astype("category").cat.as_ordered()


# In[43]:


df_tmp.info()


# In[44]:


df_tmp.state.cat.categories


# In[45]:


df_tmp.state.value_counts()


# In[46]:


df_tmp.state.cat.codes # Checks which state corresponds to which category value. E.g. Alabama = 1


# We now have a way to access all our data in the form of numbers. 
# 
# Now, lets attack missing data. 

# In[49]:


# Check columns with most missing data 
df_tmp.isnull().sum()/len(df_tmp)


# ### Save Preprocessed Data 
# 

# In[50]:


# Export to current tmp dataframe
df_tmp.to_csv("Bulldozer ML Project/train_tmp.csv", 
             index = False)


# In[51]:


# Import preprocessed data
df_tmp = pd.read_csv("Bulldozer ML Project/train_tmp.csv",
                    low_memory=False)

df_tmp.head()


# ## Fill Missing Values
# 
# ## Fill Missing Numerical Values First

# In[52]:


for label, content in df_tmp.items():
    if pd.api.types.is_numeric_dtype(content):
        print(label)


# In[53]:


df_tmp.ModelID


# In[73]:


# Check for which numeric columns have null values
for label, content in df_tmp.items():
    if pd.api.types.is_numeric_dtype(content):
        if pd.isnull(content).sum():
            print(label)


# In[74]:


# Fill numeric rows with the median
for label, content in df_tmp.items():
    if pd.api.types.is_numeric_dtype(content):
        if pd.isnull(content).sum():
            # Add a binary column which tells us if the data is missing 
            df_tmp[label+"_is_missing"] = pd.isnull(content)
            # Fill missing numeric values with median
            df_tmp[label] = content.fillna(content.median())


# In[97]:


# Check if there's any null numerican values
for label,content in df_tmp.items():
    if pd.api.types.is_numeric_dtype(content):
        if pd.isnull(content).sum():
            print(label)


# In[80]:


# Check to see how many examples were missing 
df_tmp["auctioneerID-is_missing"].value_counts()


# In[81]:


df_tmp.isna().sum()


# ### Filling Missing Categorical Variables 
# Turn them into numbers first

# In[84]:


# Check for columns which aren't numeric 
for label,content in df_tmp.items():
    if not pd.api.types.is_numeric_dtype(content):
        print(label)


# In[85]:


df_tmp.isna().sum()


# In[87]:


pd.Categorical(df_tmp["state"]).codes


# In[88]:


# Turn categorical variables into numbers and fill missing values
for label, content in df_tmp.items():
    if not pd.api.types.is_numeric_dtype(content):
        # Add binary column to indicate whether sample had missing values
        df_tmp[label+"_is_missing"] = pd.isnull(content)
        # Turn categories into numbers and add +1
        df_tmp[label] = pd.Categorical(content).codes + 1


# In[89]:


pd.Categorical(df_tmp["UsageBand"]).codes+1


# In[90]:


df_tmp.info()


# In[92]:


df_tmp.info()


# In[95]:


df_tmp.head().T


# In[99]:


df_tmp.isna().sum()


# Now that all data is numeric, with no missing values, we should be able to build a ML model. 

# In[100]:


model = RandomForestRegressor(n_jobs=-1,
                             random_state=42)

# Fit model
model.fit(df_tmp.drop("SalePrice",axis = 1),df_tmp["SalePrice"])


# In[101]:


model.score(df_tmp.drop("SalePrice", axis =1), df_tmp["SalePrice"])


# ### Splitting Data into Train/Validation Set

# In[102]:


df_tmp.saleYear


# In[103]:


# Create validation set
df_tmp.saleYear.value_counts()


# In[105]:


# Split data into training and validation, where validation set is 2012
df_val = df_tmp[df_tmp.saleYear == 2012]
df_train = df_tmp[df_tmp.saleYear != 2012]

len(df_val), len(df_train)


# In[106]:


# Split data into X & y
X_train, y_train = df_train.drop("SalePrice", axis = 1),df_train.SalePrice
X_valid, y_valid =  df_val.drop("SalePrice", axis = 1),df_val.SalePrice

X_train.shape, y_train.shape, X_valid.shape, y_valid.shape


# In[107]:


y_train


# ### Building an Evaulation Function - RMLSE
# 

# In[117]:


from sklearn.metrics import mean_squared_log_error, mean_absolute_error
from sklearn.metrics import r2_score

def rmsle(y_test, y_preds):
    """
    Calculates root mean squared log error
    between predictions and true labels.
    """
    return np.sqrt(mean_squared_log_error(y_test, y_preds))


# In[118]:


# Create function to evaluate model on a few different level 
def show_scores(model):
    train_preds = model.predict(X_train)
    val_preds = model.predict(X_valid)
    scores = {"Training MAE": mean_absolute_error(y_train, train_preds),
              "Valid MAE": mean_absolute_error(y_valid, val_preds),
              "Training MLSE": rmsle(y_train, train_preds),
              "Valid RMSLE": rmsle(y_valid, val_preds),
              "Training R^2": r2_score(y_train, train_preds),
              "Valid R^2": r2_score(y_valid, val_preds)}
    return scores


# ## Testing Our Model on a Subset (to tune HyperParameters)

# In[119]:


# This takes far too long for experimenting 

model = RandomForestRegressor(n_jobs=-1,
                             random_state=42)

#model.fit(X_train, y_train)


# In[120]:


# Change max samples value 
model = RandomForestRegressor(n_jobs=-1, 
                              random_state=42,
                              max_samples=10000)


# In[122]:


model.fit(X_train,y_train)


# In[123]:


show_scores(model)


# #### HyperParameter Tuning with RandomizedSearchCV

# In[124]:


from sklearn.model_selection import RandomizedSearchCV

# Different RandomForestRegressor hyperparameters
rf_grid = {"n_estimators": np.arange(10,100, 10),
          "max_depth": [None, 3, 5, 10], 
          "min_samples_split": np.arange(2,20,2),
          "min_samples_leaf": np.arange(1, 20, 2),
          "max_features":[0.5,1,"sqrt", "auto"],
          "max_samples":[10000]}

# RandomizedSearchCV
rs_model = RandomizedSearchCV(RandomForestRegressor(n_jobs=-1,
                                                    random_state=42),
                             param_distributions=rf_grid,
                             n_iter=2,
                             cv=5,
                             verbose=True)

# Fit model to training data
rs_model.fit(X_train, y_train)


# In[125]:


rs_model.best_params_


# In[126]:


# Evaluate RandomizedSearch model 
show_scores(rs_model)


# #### Train A Model With Best Hyperparameters
# 
# - Found after 100 iterations (which took 2 hours) of `RandomizedSearchCV`

# In[127]:


# Most ideal HyperParameters
ideal_model=RandomForestRegressor(n_estimators=40,
                                 min_samples_leaf=1,
                                 min_samples_split=14, 
                                 max_features=0.5,
                                 n_jobs=-1,
                                 max_samples=None) #forgot random_state=42

# Fit the ideal model 
ideal_model.fit(X_train, y_train)


# In[128]:


show_scores(ideal_model)


# ### Make Predictions On Test Data

# In[161]:


# Import test data
df_test=pd.read_csv("Bulldozer ML Project/Test.csv",
                   low_memory=False,
                   parse_dates=["saledate"])

df_test.head()


# Making predictions using the ideal_model will not work, as the test data has not been preprocessed (i.e. have the categorical variables changed to numerical, and the missing values fixed).

# ### PreProcessing Test Data

# In[162]:


def preprocess_data(df):
    """
    Performs transformations on df and returns the new and improved df. 
    """
    df["saleYear"] = df.saledate.dt.year
    df["saleMonth"] = df.saledate.dt.month
    df["saleDay"] = df.saledate.dt.day
    df["saleDayOfWeek"] = df.saledate.dt.dayofweek
    df["saleDayOfYear"] =  df.saledate.dt.dayofyear
    
    df.drop("saledate", axis=1, inplace=True)
    
    # Fill numeric rows with the median
    for label, content in df.items():
        if pd.api.types.is_numeric_dtype(content):
            if pd.isnull(content).sum():
            # Add a binary column which tells us if the data is missing 
                df[label+"_is_missing"] = pd.isnull(content)
            # Fill missing numeric values with median
                df[label] = content.fillna(content.median())
    
    # Fill categorical missing data and convert categories into numbers
        if not pd.api.types.is_numeric_dtype(content):
            df[label+"_is_missing"]=pd.isnull(content)
        # We add +1 to the category code because pandas encodes missing values
            df[label] = pd.Categorical(content).codes+1
    
    return df


# In[163]:


# Process test data
df_test = preprocess_data(df_test)
df_test.head()


# In[165]:


# Make predictions on updated test data 
# test_preds=ideal_model.predict(df_test) 



# The above code will not work as the number of columns in the training set is more than the test set as shown below: 

# In[167]:


set(X_train.columns) - set(df_test.columns)


# In[170]:


df_test["auctioneerID-is_missing"]=False
df_test["MachineHoursCurrentMeter-is_missing"]=False
df_test.head()


# In[171]:


test_preds = ideal_model.predict(df_test)


# ## Feature Importance 

# In[ ]:


# Find feature importance of our best model. 
ideal_model.feature_importances_


# In[148]:


# Helper Function for Plotting Feature Importance
def plot_features(columns, importances, n=20):
    df=(pd.DataFrame({"features":columns, 
                      "feature_importances": importances}), 
                .sort_values("feature_importances", ascending=False)
                .reset_index(drop= True))
    # Plot df
    fig, ax = plt.subplots()
    ax.barh(df["features"][:n], df["feature_importance"][:20])
    ax.set_ylabel("Features")
    ax.set_xlable("Feature importance")
    ax.invert_yaxis()


# In[147]:


plot_features(X_train.columns, ideal_model.feature_importances_)


# In[ ]:




