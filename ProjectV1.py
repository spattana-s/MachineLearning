
# coding: utf-8

# In[3]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
get_ipython().run_line_magic('matplotlib', 'inline')


# In[4]:


df = pd.read_csv('us_perm_visas.csv', low_memory = False, parse_dates=['decision_date', 'case_received_date'])


# In[5]:


# Displaying number of rows and columns\n",
print('Number of Visa Applications:', len(df))
print('Number of Columns:', len(df.columns))


# In[6]:


print(df.columns.values)


# In[7]:


print("Length of 'case_number' column is: ", len(df['case_number'])," with",df.case_number.isnull().sum(), "missing values")
print("Length of 'case_no' column is: ", len(df['case_no'])," with", df.case_no.isnull().sum(),"missing values")
print("First 2 values of case_number column are :\n", df['case_number'].head(2))
print("Last 2 values of case_number column are :\n", df['case_number'].tail(2) )
print("First 2 values of case_no column are :\n", df['case_no'].head(2))
print("Last 2 values of case_no column are :\n", df['case_no'].tail(2))


# In[8]:


casenoindex = df.columns.get_loc("case_no")
casenumberindex = df.columns.get_loc("case_number")
casenumberlist = []
for value in df.iloc[0:135269,casenoindex]:
       casenumberlist.append(value)
for value in df.iloc[135269:374363,casenumberindex]:
       casenumberlist.append(value)
df['casenumber'] = casenumberlist
df.drop(df.columns[[casenoindex,casenumberindex]], axis=1, inplace=True)


# In[9]:


df.head()


# In[10]:


for value in df.case_status.unique():
       print(len(df[df['case_status'] == value])," occurrences of status {}".format(value))


# In[11]:


#Removing all withdrawn applications\n",
df = df[df.case_status != 'Withdrawn']
#Combining certified-expired and certified applications and displaying distribution of \"case_status\" variable
df.loc[df.case_status == 'Certified-Expired', 'case_status'] = 'Certified'
df.case_status.value_counts()


# In[12]:


#Dropping all empty columns\n",
df = df.dropna(axis=1, how='all')
#Dropping all empty rows\n",
df = df.dropna(axis=0, how='all')
df.shape


# In[13]:


# Displaying number of missing values in each column
for column in df.columns:
    print("Attribute '{}' contains ".format(column),  df[column].isnull().sum().sum(), " missing values")


# In[14]:


#Converting the date to contain just the year of application submission
df['year'] = df['decision_date'].dt.year
#Setting plot parameters\n",
fig, ax = plt.subplots()
fig.set_size_inches(12.7, 8.27)
sns.set_context(rc={"font.size":12})
sns.countplot(x="year", hue="case_status", data=df)
ax.set(xlabel='Visa application year', ylabel='Number of Visa applicatons')


# In[15]:


# Displaying 15 most popular cities
df['employer_city'] = df['employer_city'].str.upper()
df['employer_city'].value_counts().head(15)


# In[16]:


# Setting plot parameters
fig, ax = plt.subplots()
fig.set_size_inches(13.7, 8.27)
sns.set_context("paper", rc={"font.size":12,"axes.titlesize":12,"axes.labelsize":12})
sns.countplot(x='employer_city', hue='year', data=df, order=df.employer_city.value_counts().iloc[:10].index)
plt.xticks(rotation=90)
ax.set(xlabel='Employer city', ylabel='Number of Visa applications')


# In[17]:


#Setting plot parameters
fig, ax = plt.subplots()
fig.set_size_inches(12.7, 8.27)
sns.set_context(rc={"font.size":12,"axes.labelsize":13})
sns.countplot(x='employer_name', data=df, palette = sns.cubehelix_palette(8, start=.5, rot=-.75), order=df.employer_name.value_counts().iloc[:10].index)
plt.xticks(rotation=90)
#Iterating over elements in \"employer_name\" column and displaying counts above bars \n",
for i, v in df.employer_name.value_counts().head(10).reset_index().iterrows():
        ax.text(i,v.employer_name,v.unique()[1], horizontalalignment='center',verticalalignment='bottom')
        ax.set(xlabel='Employer name', ylabel='Number of Visa applications')


# In[18]:


#Creating empty dictionary
us_economic_counts = {}
#Iterating over \"us_economic_sector\" column and appending values to the \"us_economic_counts\" dictionary\n",
for value in df['us_economic_sector'].dropna():
        if value in us_economic_counts:
            us_economic_counts[value] += 1
        else:
            us_economic_counts[value] = 1


# In[19]:


#Creating lists for us economic sectors and their counts
usecolabels = []
usecovalues = []
explode = (0.035, 0, 0, 0,0,0,0,0,0,0)
for key, value in us_economic_counts.items():
        usecolabels.append(key)
        usecovalues.append(value)
#Setting plot parameters\n",
plt.figure(figsize=(13,13))
sns.set_context(rc={"font.size":10,"axes.labelsize":11,"xtick.labelsize" : 11})
plt.pie(usecovalues[:10], labels=usecolabels[:10], explode = explode, autopct='%1.1f%%', pctdistance = 0.9,rotatelabels = 90, startangle=140, labeldistance = 1.05)


# In[20]:


#Converting values to lower case
df['job_info_job_title'] = df['job_info_job_title'].str.lower()
#Splitting job titles by '-'\n",
df['job_info_job_title'] = df['job_info_job_title'].astype(str).str.split('-').str[0]
#Splitting job titles by 'ii'
df['job_info_job_title'] = df['job_info_job_title'].astype(str).str.split('ii').str[0]
#Splitting job titles by '/'
df['job_info_job_title'] = df['job_info_job_title'].astype(str).str.split('/').str[0]
#Removing leading and ending spaces
df['job_info_job_title'] = df['job_info_job_title'].astype(str).str.strip()
#Replacing \"sr.\" values with \"senior\"
df['job_info_job_title'] = df['job_info_job_title'].str.replace('sr.', 'senior')
#Replacing \"NaN\", \"NaT\" and \"nan\" values with np.nan\n"
df['job_info_job_title'].replace(["NaN", 'NaT','nan'], np.nan, inplace = True)
df['job_info_job_title'].value_counts(dropna=True)[:10]


# In[21]:


#Setting plot parameters
fig, ax = plt.subplots()
fig.set_size_inches(12.7, 8.27)
sns.set_context(rc={"font.size":14, "axes.labelsize":12})
sns.countplot(x='job_info_job_title',data=df, 
              palette = sns.diverging_palette(255, 133, l=60, n=10, center="dark"), 
              order=df.job_info_job_title.value_counts().iloc[:10].index)
plt.xticks(rotation=90)
#Iterating over elements in \"job_info_job_title\" column and displaying counts above bars \n",
for i, v in df.job_info_job_title.value_counts().head(10).reset_index().iterrows():
           ax.text(i,v.job_info_job_title,v.unique()[1], horizontalalignment='center',verticalalignment='bottom')
#Setting label titles 
ax.set(xlabel='Job Title', ylabel='Number of Visa applications')


# In[22]:


#Setting plot parameters\n",
fig, ax = plt.subplots()
fig.set_size_inches(12.7, 8.27)
sns.set_context(rc={"font.size":14, "axes.labelsize":12})
sns.countplot(x='country_of_citizenship',hue='case_status',data=df, 
        palette = sns.diverging_palette(255, 133, l=60, n=7, center="dark"),
        order=df.country_of_citizenship.value_counts().iloc[:7].index)
plt.xticks(rotation=90)
#Iterating over elements in \"country_of_citizenship\" column and displaying counts above bars \n",
for i, v in df.country_of_citizenship.value_counts().head(7).reset_index().iterrows():
       ax.text(i,v.country_of_citizenship,v.unique()[1], horizontalalignment='right',verticalalignment='bottom')
#Setting label titles    \n",
ax.set(xlabel='Country of citizenship', ylabel='Number of Visa applications')


# In[23]:


#Setting plot parameters
fig, ax = plt.subplots()
fig.set_size_inches(12.7, 8.27)
sns.countplot(x='class_of_admission',data=df, 
        order=df.class_of_admission.value_counts().iloc[:10].index)
plt.xticks(rotation=90)
#Iterating over elements in \"class_of_admission\" column and displaying counts above bars \n",
for i, v in df.class_of_admission.value_counts().head(10).reset_index().iterrows():
        ax.text(i,v.class_of_admission,v.unique()[1], horizontalalignment='center',verticalalignment='bottom')
ax.set(xlabel='Visa type', ylabel='Number of Visa applications')


# In[24]:


#Setting plot parameters\n"
fig, ax = plt.subplots()
fig.set_size_inches(10.7, 7.27)
sns.countplot(x='application_type', data=df, palette = sns.color_palette("GnBu_d"), order=df.application_type.value_counts().iloc[:10].index)
#Iterating over elements in \"application_type\" column and displaying counts above bars \n",
for i, v in df.application_type.value_counts().head(10).reset_index().iterrows():
       ax.text(i,v.application_type,v.unique()[1], horizontalalignment='center',verticalalignment='bottom')
ax.set(xlabel='Application type', ylabel='Number of Visa applications')


# In[25]:


#Setting plot parameters\n",
fig, ax = plt.subplots()
fig.set_size_inches(12.7, 8.27)
#sns.set_context(rc={\"font.size\":14, \"axes.labelsize\":12})\n",
sns.countplot(x='foreign_worker_info_education',data=df,
        palette = sns.color_palette("Paired"),
        order=df.foreign_worker_info_education.value_counts().iloc[:10].index)
#Iterating over elements in \"job_info_job_title\" column and displaying counts above bars \n",
for i, v in df.foreign_worker_info_education.value_counts().head(10).reset_index().iterrows():
        ax.text(i,v.foreign_worker_info_education,v.unique()[1], horizontalalignment='center',verticalalignment='bottom')
#Setting label titles    \n",
ax.set(xlabel='Education level', ylabel='Number of Visa applications')


# In[26]:


df[['pw_amount_9089','pw_unit_of_pay_9089']].head(10)


# In[30]:


#Replacing commas with whitespace character\n",
df['pw_amount_9089'] = df['pw_amount_9089'].str.replace(",","")
for unit in df.pw_unit_of_pay_9089.unique():
        if unit == "hr" or unit == "Hour":
            df.loc[df['pw_unit_of_pay_9089'] == unit, 'pw_amount_9089'] = df['pw_amount_9089'].apply(lambda x: float(x) * 8 * 250)
            df.loc[df['pw_unit_of_pay_9089'] == unit, 'pw_unit_of_pay_9089'] = df['pw_unit_of_pay_9089'].replace(to_replace = unit, value = "Year")
        elif unit == "wk" or unit == "Week":
            df.loc[df['pw_unit_of_pay_9089'] == unit, 'pw_amount_9089'] = df['pw_amount_9089'].apply(lambda x: float(x) * 50)
            df.loc[df['pw_unit_of_pay_9089'] == unit, 'pw_unit_of_pay_9089'] = df['pw_unit_of_pay_9089'].replace(to_replace = unit, value = "Year")
        elif unit == "mth" or unit == "Month":
            df.loc[df['pw_unit_of_pay_9089'] == unit, 'pw_amount_9089'] = df['pw_amount_9089'].apply(lambda x: float(x) * 12)
            df.loc[df['pw_unit_of_pay_9089'] == unit, 'pw_unit_of_pay_9089'] = df['pw_unit_of_pay_9089'].replace(to_replace = unit, value = "Year")
        elif unit == "bi" or unit == "Bi-Weekly":
            df.loc[df['pw_unit_of_pay_9089'] == unit, 'pw_amount_9089'] = df['pw_amount_9089'].apply(lambda x: float(x) * 25)
            df.loc[df['pw_unit_of_pay_9089'] == unit, 'pw_unit_of_pay_9089'] = df['pw_unit_of_pay_9089'].replace(to_replace = unit, value = "Year")
        elif unit =="yr":
            df.loc[df['pw_unit_of_pay_9089'] == unit ,'pw_unit_of_pay_9089'] = df['pw_unit_of_pay_9089'].replace(to_replace = unit, value = "Year")
        else:
            continue
        #Filling missing values with median \n",
df['pw_amount_9089']= df['pw_amount_9089'].fillna((df['pw_amount_9089'].median()))
        #Changing format from string to float\n",
df['pw_amount_9089'] = df.pw_amount_9089.astype(float)
        #Displaying 10 first values\n",
df[['pw_amount_9089','pw_unit_of_pay_9089']].head(10)


# In[31]:


#Since running \"describe\" method on \"pw_amount_9089\" column returned exponential values, I decided to \n",
#convert them to floats so that they are easier to understand\n",
with pd.option_context('float_format', '{:.2f}'.format): print(df.pw_amount_9089.describe())


# In[32]:


#Dividing our continuous income values into some categories to facilitate their visualization\n",
df['remuneration'] = pd.cut(df['pw_amount_9089'], [0, 30000, 60000,90000,120000,150000,180000,210000,240000,270000,495748000], right=False, 
                            labels=["0-30k", "30-60k","60-90k","90-120k","120-150k","150-180k","180-210k","210-240k","240-270k","270k+"])
salary = df['remuneration'].value_counts()
salary.iloc[np.argsort(salary.index)]


# In[33]:


# Draw a count plot to show the distribution of remunerations\n",
g = sns.factorplot(x='remuneration', data=df, kind="count",
        palette="BuPu", size=9, aspect=1.2)
g.set(xlabel='Remuneration', ylabel='Number of applicants')


# In[34]:


#Displaying percentage of non-null values for each feature\n",
i = 0;
for col in df.columns:
        i = i+1;
        print (i-1,"Column: '{}'".format(col),"contains ", np.round(100*df[col].count()/len(df['case_status']),decimals=2),"% non-null values" )


# In[35]:


#Leaving columns which have more than 330000 non-missing observations\n",
df = df.loc[:,df.count() >= 330000]
df.info()


# In[36]:


#Indices of selected features\n",
chosen_attrs = [0,1,2,5,6,8,12,14,17,18]
df = df.iloc[:,chosen_attrs]


# In[37]:


df.info()


# In[38]:


#Assigning Labels to Case Status\n",
df.loc[df.case_status == 'Certified', 'case_status'] = 1
df.loc[df.case_status == 'Denied', 'case_status'] = 0
#Filling missing values in \"employer_state\" column with mode\n",
df['employer_state'] = df['employer_state'].fillna(df['employer_state'].mode()[0]);
#Mapping from state name to abbreviation\n",
state_abbrevs = {
        'Alabama': 'AL',
        'Alaska': 'AK',
        'Arizona': 'AZ',
        'Arkansas': 'AR',
        'California': 'CA',
        'Colorado': 'CO',
        'Connecticut': 'CT',
        'Delaware': 'DE',
        'Florida': 'FL',
        'Georgia': 'GA',
        'Hawaii': 'HI',
        'Idaho': 'ID',
        'Illinois': 'IL',
        'Indiana': 'IN',
        'Iowa': 'IA',
        'Kansas': 'KS',
        'Kentucky': 'KY',
        'Louisiana': 'LA',
        'Maine': 'ME',
        'Maryland': 'MD',
        'Massachusetts': 'MA',
        'Michigan': 'MI',
        'Minnesota': 'MN',
        'Mississippi': 'MS',
        'Missouri': 'MO',
        'Montana': 'MT',
        'Nebraska': 'NE',
        'Nevada': 'NV',
        'New Hampshire': 'NH',
        'New Jersey': 'NJ',
        'New Mexico': 'NM',
        'New York': 'NY',
        'North Carolina': 'NC',
        'North Dakota': 'ND',
        'Ohio': 'OH',
        'Oklahoma': 'OK',
        'Oregon': 'OR',
        'Pennsylvania': 'PA',
        'Rhode Island': 'RI',
        'South Carolina': 'SC',
        'South Dakota': 'SD',
        'Tennessee': 'TN',
        'Texas': 'TX',
        'Utah': 'UT',
        'Vermont': 'VT',
        'Virginia': 'VA',
        'Washington': 'WA',
        'West Virginia': 'WV',
        'Wisconsin': 'WI',
        'Wyoming': 'WY',
        'Northern Mariana Islands':'MP',
        'Palau': 'PW',
        'Puerto Rico': 'PR',
        'Virgin Islands': 'VI', 
        'District of Columbia': 'DC',
        }
#Capitalizing Keys\n",
us_state_abbrev = {k.upper(): v for k, v in state_abbrevs.items()}
df['employer_state'].replace(us_state_abbrev, inplace=True)
df.employer_state = df.employer_state.astype(str)


# In[39]:


df.head()


# In[40]:


df['pw_soc_code'] = df['pw_soc_code'].str.replace('.','')
df['pw_soc_code'] = df['pw_soc_code'].str.replace('-','')
df['pw_soc_code'] = df['pw_soc_code'].astype(str).str[0:6]
df['pw_soc_code'].value_counts()
#Finding \"nan\" values in \"pw_soc_code\" column and filling them with mode\n",
df.loc[df['pw_soc_code'] == "nan",'pw_soc_code'] = df['pw_soc_code'].mode()[0]
#Finding \"None\" values in \"pw_soc_code\" column and filling them with mode\n",
df.loc[df['pw_soc_code'] == "None",'pw_soc_code'] = df['pw_soc_code'].mode()[0]
#Changing type from string to int\n",
df['pw_soc_code'] = df['pw_soc_code'].astype(int)
df['case_status'] = df['case_status'].astype(int)


# In[41]:


df.head()


# In[42]:


#Replacing missing values with mode\n",
df['class_of_admission']=df['class_of_admission'].fillna((df['class_of_admission'].mode()[0]))
df['country_of_citizenship']=df['country_of_citizenship'].fillna((df['country_of_citizenship'].mode()[0]))
df['employer_city']=df['employer_city'].fillna((df['employer_city'].mode()[0]))
df['employer_name']=df['employer_name'].fillna((df['employer_name'].mode()[0]))
df['employer_name']=df['employer_name'].astype(str).str.upper()
df['pw_source_name_9089']=df['pw_source_name_9089'].fillna((df['pw_source_name_9089'].mode()[0]))
df['remuneration']=df['remuneration'].fillna((df['remuneration'].mode()[0]))


# In[43]:


df.head(10)


# In[44]:


df.info()


# In[45]:


from sklearn.preprocessing import LabelEncoder
categorical_variables = {}
#Creating categories denoted by integers from column values\n",
for col in df.columns:
        cat_var_name = "cat_"+ col
        cat_var_name = LabelEncoder()
        cat_var_name.fit(df[col])
        df[col] = cat_var_name.transform(df[col])
        categorical_variables[col] = cat_var_name

df.info()


# In[46]:


df.head()


# In[159]:


#Dividing our final dataset into features(explanatory variables) and labels(target variable)\n",
X = df.loc[:, df.columns != 'case_status']
y = df.case_status
print("The shape of X is: {}".format(X.shape))
print("The shape of y is: {}".format(y.shape))


# In[163]:


#Importing Logistic Regression Classifier, GridSearchCV, train_test_split and accuracy metrics from sklearn\n",
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn.metrics import precision_score, recall_score, f1_score, confusion_matrix
#Defining fit_algorithm function\n",
def fit_algorithm(alg, X, y, parameters, cv = 5):
#This function will split our dataset into training and testing subsets, fit cross-validated
#GridSearch object, test it on the holdout set and return some statistics
        X_train, X_test, y_train, y_test = train_test_split(X, y, random_state = 123)
        grid = GridSearchCV(alg, parameters, cv = cv)
        grid.fit(X_train, y_train)
        y_pred = grid.predict(X_test)
        confmat = confusion_matrix(y_test,y_pred)
        return pd.Series({
            "Train_ACC": np.around(grid.best_score_, decimals=2).astype(str),
            "Test_ACC": np.around(grid.score(X_test, y_test), decimals=2).astype(str),
            "P": np.around(precision_score(y_pred, y_test), decimals=2).astype(str),
            "R": np.around(recall_score(y_pred, y_test),decimals=2).astype(str),
            "F1": np.around(f1_score(y_pred, y_test),decimals=2).astype(str),
            "Best_params": [grid.best_params_],
            "True negatives": confmat[0,0],
            "False negatives": confmat[1,0],
            "True positives": confmat[1,1],
            "False positives": confmat[0,1]
            })


# In[164]:


#To perform hyper parameter optimisation a list of multiple elements will be entered and the optimal \n",
#value in that list will be picked using Grid Search object\n",
logreg_params = {'C': [0.001, 0.01, 0.1, 1, 10, 100] }
logreg = fit_algorithm(LogisticRegression(),X,y,logreg_params)
logreg


# In[165]:


#Importing k-Nearest Neighbors Classifier\n",
from sklearn.neighbors import KNeighborsClassifier
#Defining range of parameters for kNN Clssifier\n",
knn_params = {'n_neighbors': np.arange(1,11).tolist()}
#Using \"fit_algorithm\" function with kNN Classifier\n",
knn = fit_algorithm(KNeighborsClassifier(),X,y,knn_params)
knn


# In[167]:


#Defining range of parameters for Random Forest Clssifier\n",
forest_params = {'n_estimators': [10,20,30,40,50],
        'max_depth': [15,20,25,30],
        'max_features': [2,3,4],
        'random_state': [123],
        'n_jobs': [-1]
        }
# #Importing RandomForestClassifier from sklearn\n",
from sklearn.ensemble import RandomForestClassifier
forest = fit_algorithm(RandomForestClassifier(),X,y, forest_params)
forest


# In[169]:


#Importing GradientBoostingClassifier from sklearn\n",
from sklearn.ensemble import GradientBoostingClassifier
#Defining range of parameters for Gradient Boosting Clssifier\n",
gradient_params = {'n_estimators': [100],
        'max_depth': [3],
        'random_state': [123],
        'learning_rate': [0.1]
        }
gradient = fit_algorithm(GradientBoostingClassifier(),X,y,gradient_params)
gradient


# In[170]:


# Dataframe made of results 
summary = pd.concat([logreg,knn,forest,gradient],axis=1)
summary.columns = ['Logistic Regression', 'k-Nearest Neighbors','Random Forest','GBoosted Machines']
summary


# In[172]:


X_train, X_test, y_train, y_test = train_test_split(X, y, random_state = 123)
final_forest = RandomForestClassifier(n_estimators = 50, max_depth = 20, max_features = 4, random_state = 123, n_jobs = -1)
final_forest.fit(X_train, y_train)
importances = final_forest.feature_importances_
std = np.std([tree.feature_importances_ for tree in final_forest.estimators_],
        axis=0)
indices = np.argsort(importances)[::-1]
# Plot the feature importances of the forest\n",
plt.figure(figsize=(10, 8))
plt.title("Feature importances")
plt.bar(range(X_train.shape[1]), importances[indices],
        color="g", yerr=std[indices], align="center")
plt.xticks(range(X_train.shape[1]), X_train.columns, rotation = 90)
plt.xlim([-1, X_train.shape[1]])
plt.show()   


# In[47]:


#import csv
# write dataframe to a csv with header without index
df.to_csv('dfr.csv', header=True, index=False)


# In[49]:


df.head()


# In[ ]:


from sklearn import svm

