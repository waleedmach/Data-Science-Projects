#!/usr/bin/env python
# coding: utf-8

# # MileStone Project 1 - Predicting Heart Disease
# 
# Using various ML based libraries to build a model capable of predicting whether or not a patient has heart disease, given certain medical attributes.
# Steps:
# 1. Problem Definition
# 2. Data
# 3. Evaluation
# 4. Features
# 5. Modelling
# 6. Experimentation
# 
# ## 1. Problem Definition 
# 
# Can we predict heart disease presence, given mdeical attributes for patients? 
# 
# ## 2. Data - Data Dictionary
# 
# 1. age
# 2. sex
# 3. chest pain type (4 values)
# 4. resting blood pressure
# 5. serum cholestoral in mg/dl
# 6. fasting blood sugar > 120 mg/dl
# 7. resting electrocardiographic results (values 0,1,2)
# 8. maximum heart rate achieved
# 9. exercise induced angina
# 10. oldpeak = ST depression induced by exercise relative to rest
# 11. the slope of the peak exercise ST segment
# 12. number of major vessels (0-3) colored by flourosopy
# 13. thal: 0 = normal; 1 = fixed defect; 2 = reversable defect
# 14. target 1 or 0
# 
# ## 3. Evaluation
# 
# If we can reach 95% accuracy at predicting whether or not a person has heart disease or not, we'll pursue the project. 
# 
# ## 4. Features
# 
# See step 2. This is where you'll get different information about each of the features of the data. 
# 
# 
# 

# In[1]:


import pandas as pd


# ### Preparing tools

# In[8]:


import pandas as pd
import numpy as np
import sklearn
import matplotlib.pyplot as plt
import seaborn as sns
 # appear inside notebok
get_ipython().run_line_magic('matplotlib', 'inline')

from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier

# Model Evaluation
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV
from sklearn.metrics import confusion_matrix, classification_report
from sklearn.metrics import precision_score, recall_score, f1_score
from sklearn.metrics import RocCurveDisplay


# ## Load Data

# In[54]:


df = pd.read_csv("heart-disease.csv")
df.shape


# ## Exploratory Data Analysis 
# 
# The goal here is to find out more about the data, and become a subject matter expert on the dataset. 
#  
# 1. What questions are you trying to solve?
# 2. What kind of data do we have and how do we treat different types?
# 3. What's missing from the data and how do you deal with it?
# 4. Where are the outliers and why you should care about them? 
# 5. How can you add, change, or remove features to get more out of your data? 

# In[55]:


df.head()


# In[56]:


df.tail()


# In[13]:


df["target"].value_counts()


# In[15]:


df["target"].value_counts().plot(kind="bar", color=["salmon", "lightblue"])


# In[16]:


df.info()


# In[17]:


# Missing data? 
df.isna().sum()


# In[18]:


df.describe()


# ### Finding Patterns 

# In[20]:


### Heart Disease Frequency according to Sex 

df.sex.value_counts()


# In[21]:


# Compare target column with sex columb
pd.crosstab(df.target, df.sex)


# In[27]:


# Create a plot of crosstab
pd.crosstab(df.target, df.sex).plot(kind="bar", 
                                    figsize=(10,6)
                        )

plt.title("Heart Disease Frequency by Sex")
plt.xlabel("0=No disease, 1 = Disease")
plt.ylabel("Amount")
plt.legend(["Female", "Male"])
plt.xticks(rotation=0);


# In[29]:


df["thalach"].value_counts() # what values showed up how many times. Therefore, 162 was the score 11 times.


# ### Age vs Max Heart Rate for Heart Disease

# In[32]:


plt.figure(figsize=(10,6))

# Scatter with positive examples
plt.scatter(df.age[df.target==1],
           df.thalach[df.target==1],
           c="salmon")

# Scatter with negative examples
plt.scatter(df.age[df.target==0],
           df.thalach[df.target==0],
           c="lightblue")


# Add some helpful info 
plt.title("Age vs Max heart rate")
plt.legend(["Disease", "No Disease"])


# Above shows that the younger someone is, the higher their max heart rate. 

# In[35]:


# Check the distribution of the age column with a histogram
df.age.plot.hist()


# In[36]:


## Heart Disease Frequency per Chest Pain type

pd.crosstab(df.cp, df.target)


# In[39]:


# Make the crosstab more visual 
pd.crosstab(df.cp, df.target).plot(kind = "bar",
                                  figsize=(10,6),
                                  color=["salmon","lightblue"]);

plt.title("Heart Disease Frequency per Chest Pain Type")
plt.xticks(rotation=0)


# In[40]:


df.head()


# In[41]:


## Correlation Matrix - How independent variables are related to each other
df.corr()


# In[43]:


# Make it better
corr_matrix= df.corr()
fig, ax = plt.subplots(figsize=(15,10))
ax= sns.heatmap(corr_matrix, 
               annot = True, 
               linewidths=0.5,
               fmt=".2f")
              # cmap="Y1GnBu");
    


# # 5. Modelling

# In[44]:


df.head()


# In[69]:


# Split data into X and Y 
X = df.drop("target", axis = 1)
y = df["target"]



# In[70]:


X


# In[71]:


y


# In[72]:


np.random.seed(42)

# Split into train & test set
X_train, X_test, y_train, y_test = train_test_split(X, # independent variables 
                                                    y, # dependent variable
                                                    test_size = 0.2)


# In[73]:


X_train, len(X_train)


# In[74]:


y_train, len(y_train)


# ## Now time to build a ML model on training sets, and test on test sets.
# 1. Logistic Regression 
# 2. K-Nearest Neighbors
# 3. Random Forest Classifier

# In[75]:


# Put models in a dictionary

models = {"Logistic Regression": LogisticRegression(),
         "KNN": KNeighborsClassifier(),
         "Random Forest": RandomForestClassifier()}

# Create a function to fit and score models
def fit_and_score(models, X_train, X_test, y_train, y_test):
    """
    Fits and evaluates given machine learning models. 
    models: a dictionary of different Sckkit-Learn ML models
    X_train: training data (no labels)
    X_test: testing data
    y_train: training labels
    y_test: test labels
    """
    
    # Set random seed
    np.random.seed(42)
    # Make a dictionary to keep model scores
    model_scores = {}
    # Loop through models
    for name, model in models.items():
        # Fit the model to the data
        model.fit(X_train, y_train)
        # Evaluate model, and append score to model_scores dictionary
        model_scores[name] = model.score(X_test, y_test)
    return model_scores
        


# In[77]:


model_scores=fit_and_score(models = models,
                          X_train = X_train, 
                          X_test = X_test,
                          y_train = y_train,
                          y_test = y_test)
model_scores


# ### Model Comparison 

# In[79]:


model_compare = pd.DataFrame(model_scores, index=["accuracy"])
model_compare.T.plot.bar();


# Now we've got a baseline model, and we know this likely isn't the best possible model, we now should look at:
# 
# - Hyperparameter Tuning
# - Feature importance
# - Confusion matrix
# - Cross-Validation
# - Precision
# - Recall
# - F1 Score
# - Classification Report
# - ROC Curve
# - Area under curve (AUC)
# 
# ### HyperParameter Tuning 

# In[82]:


# KNN

train_scores = []
test_scores = []

# Create a list of default values for n_neighbors
neighbors = range(1,21)

# Setup KNN instance 
knn = KNeighborsClassifier()

# Loop through different n_neighbors
for i in neighbors:
    knn.set_params(n_neighbors=i)
    
    # Fit algorith
    knn.fit(X_train, y_train)
    
    # Update training scores list
    train_scores.append(knn.score(X_train, y_train))
    
    # Update the test scores list
    test_scores.append(knn.score(X_test, y_test))
    


# In[83]:


train_scores


# In[84]:


test_scores


# In[86]:


plt.plot(neighbors, train_scores, label ="Train score")
plt.plot(neighbors, test_scores, label="Test score")
plt.xticks(np.arange(1,21,1))
plt.xlabel("Number of neighbors")
plt.ylabel("Model score")
plt.legend()

print (f"Maximum KNN score on the test data: {max(test_scores)*100:.2f}%")


# Even now, it only produces a highest score of .75, which is far lower than our LR model, and far below our 95% accuracy requirement. 

# ### Hyperparameter Tuning with RandomizedSearchCV 
# 
# We are going to tune our LR and RFC models using RandomizedSearchCV. 

# In[97]:


# Create a hyperparameter grid for Logistic Regression 
log_reg_grid = {"C": np.logspace(-4, 4, 20),
               "solver":["liblinear"]}

# Create a hyperparameter grid for RandomForestClassifier
rf_grid = {"n_estimators":np.arange(10, 1000, 50),
          "max_depth":[None, 3, 5, 10],
          "min_samples_split": np.arange(2,20,2),
          "min_samples_leaf": np.arange(1,20,2)}


# Now we've got hyperparameter grids set up for each of our models, lets tune them using RandomizedSearchCV

# In[98]:


# Tune LR
np.random.seed(42)

# Setup random hyperparameter search for LR
rs_log_reg =RandomizedSearchCV(LogisticRegression(), 
                  param_distributions=log_reg_grid,
                  cv=5, 
                  n_iter=20, 
                  verbose=True)

# Fit random hyperparameter search model for LogisiticRegression 
rs_log_reg.fit(X_train,y_train)


# In[99]:


rs_log_reg.best_params_


# In[100]:


rs_log_reg.score(X_test,y_test)


# Now we will tune our RFC

# In[102]:


# Tune LR
np.random.seed(42)

# Setup random hyperparameter search for RFC
rs_rf =RandomizedSearchCV(RandomForestClassifier(), 
                          param_distributions=rf_grid,
                          cv=5, 
                          n_iter=20, 
                          verbose=True)
    
# Fit random hyperparameter search model for LogisiticRegression 
rs_rf.fit(X_train,y_train)


# In[103]:


# Find the best hyperparameters 
rs_rf.best_params_


# In[104]:


# Evaluate the randomized search RandomForestClassifier
rs_rf.score(X_test,y_test)


# ### Hyperparameter Tuning with RandomizedSearchCV 

# Since our LogisticRegression model provides best scores so far, we'll try and improve them again using GridSearchCV. 

# In[106]:


# Different hyperparameters for our Logistic Regression
log_reg_grid = {"C": np.logspace(-4,4,30),
               "solver":["liblinear"]}

# Setup grid hyperparameter search for LogisticRegression 
gs_log_reg = GridSearchCV(LogisticRegression(), 
                         param_grid=log_reg_grid, 
                         cv= 5, 
                         verbose=True)

# Fit grid hyperparameter search model 
gs_log_reg.fit(X_train, y_train)


# In[107]:


gs_log_reg.best_params_


# In[108]:


gs_log_reg.score(X_test, y_test)


# # Evaluating our tuned machine learning classifier, beyond accuracy 
# 
# - ROC Curve and AUC Score
# - Confusion Matrix
# - Classification Report
# - Precision 
# - Recall
# - F1 Score
# 
# ...using cross-validation where possible
#  
# To make comparisons, we need to make predictions first. 

# In[109]:


y_preds= gs_log_reg.predict(X_test)


# In[110]:


y_preds


# In[111]:


y_test


# In[117]:


# ROC Curve and AUC Score: 

from sklearn.metrics import RocCurveDisplay



RocCurveDisplay.from_predictions(y_test,y_preds)


# In[118]:


# Confusion matrix
print(confusion_matrix(y_test, y_preds))


# In[119]:


sns.set(font_scale=1.5)

def plot_conf_mat(y_test, y_preds):
    """
    Plots a nice looking confusion matrix using Seaborn 
    """
    fig, ax = plt.subplots(figsize=(3,3))
    ax = sns.heatmap(confusion_matrix(y_test, y_preds),
                     annot=True,
                     cbar=False)
    plt.xlabel("True label")
    plt.ylabel("Predicted label")
    
    bottom, top = ax.get_ylim()
    ax.set_ylim(bottom + 0.5, top - 0.5)

plot_conf_mat(y_test, y_preds)
                


# Now we need a Classification Report, and a cross-validated precision, recall, and f1-score

# In[120]:


print(classification_report(y_test, y_preds)) # This is the total classification report, but not a cross-validated one. 


# ### Cross Validated Classification Report

# In[154]:


# Check best HyperParameters
gs_log_reg.best_params_


# In[155]:


# Create a new classifier with best parameters
clf = LogisticRegression(C=0.20433597178569418, 
                        solver = "liblinear")


# In[156]:


# Cross validated score
cv_acc = cross_val_score(clf, 
                        X, 
                        y, 
                        cv = 5, 
                        scoring = "accuracy")

cv_acc


# In[157]:


cv_acc = np.mean(cv_acc)


# In[149]:


cv_acc


# In[158]:


cv_precision = cross_val_score(clf, 
                        X, 
                        y, 
                        cv = 5, 
                        scoring = "precision")

cv_precision = np.mean(cv_precision)
cv_precision


# In[159]:


cv_recall = cross_val_score(clf, 
                        X, 
                        y, 
                        cv = 5, 
                        scoring = "recall")

cv_recall = np.mean(cv_recall)
cv_recall


# In[160]:


cv_f1 = cross_val_score(clf, 
                        X, 
                        y, 
                        cv = 5, 
                        scoring = "f1")

cv_f1 = np.mean(cv_f1)
cv_f1


# In[161]:


# Visualize Cross-Validated Metrics 
cv_metrics = pd.DataFrame({"Accuracy": cv_acc, 
                           "Precision": cv_precision, 
                           "Recall": cv_recall, 
                           "F1 Score": cv_f1},
                           index=[0])

cv_metrics.T.plot.bar(title = "Cross Validated Classification Scores", legend = False);


# # Feature Importance 
# - Which features contributed most to the outcomes of the model and how did they contribute? 
# - Finding this is different for each ML model
# - For LR Model: 

# In[165]:


# Fit an instance of LogisticRegression 
clf = LogisticRegression(C=0.20433597178569418, 
                         solver="liblinear")

clf.fit(X_train,y_train )


# In[166]:


# Check coefficients 
clf.coef_


# In[167]:


# Match coef's of features to columns
feature_dict=dict(zip(df.columns,list(clf.coef_[0])))
feature_dict


# In[168]:


# Visualize Feature Importance
feature_df = pd.DataFrame(feature_dict, index=[0])
feature_df.T.plot.bar(title="Feature Importance", legend = False)


# In[169]:


pd.crosstab(df["slope"],df["target"])


# # 6. Experimentation 
# - If you haven't hit your evaluation metric yet, then try and see if you can obtain more data. 
# - Or if you could try a better model. (Like CatBoost, or XGBoost). 
# - Could you improve the current models? 
# - Export it and share model with others? 
