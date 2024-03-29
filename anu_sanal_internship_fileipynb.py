# -*- coding: utf-8 -*-
"""Anu Sanal- Internship fileipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1foSKZWwqYf7CcyI0tbMLW67OmLvF0J_C
"""

import pandas as pd

import numpy as np

df =pd.read_excel('Superstore.xlsx')

df.shape

df.info()

df.isna().sum()

"""No missing values"""

df.head()

"""# **Exploratory Data Analysis (EDA)**"""

df['Category'].value_counts()

df.describe()

df['Sub-Category'].value_counts()

df.columns

df1 = df.drop(['Row ID', 'Order ID', 'Order Date', 'Ship Date', 'Ship Mode',
       'Customer ID', 'Customer Name', 'Segment', 'Country', 'City', 'State',
       'Postal Code', 'Region', 'Product ID', 'Sub-Category',
       'Product Name'],axis =1 , inplace=True)

import seaborn as sns
import matplotlib.pyplot as plt

corrmatrix = df.corr().round(2)
fig, ax = plt.subplots(figsize=(5,8))
sns.heatmap(corrmatrix,annot=True)

df.columns

plt.figure()
plt.hist(df['Sales'],color='black')
plt.title('Distribution of Sales',fontsize=18)
plt.xlabel('Sales')
plt.ylabel('frequency')
plt.xticks()
plt.yticks()

plt.figure()
plt.hist(df['Profit'],color='black')
plt.title('Distribution of Profit',fontsize=18)
plt.xlabel('Profit')
plt.ylabel('frequency')
plt.xticks()
plt.yticks()

sns.pairplot(df)

plt.figure(figsize=(20,5))
plt.title("Boxplot")
sns.boxplot(data=df,orient="h")

drop_outlier = df[(df['Sales']>5000) | (df['Profit']>3000) | (df['Profit']<-2500)].index

df = df.drop(drop_outlier)

df.shape

plt.figure(figsize=(8,12))
sns.boxplot(data=df)

"""# **Forecasting Procedure**"""

data=pd.read_excel('Superstore.xlsx')

cols = ['Row ID', 'Order ID', 'Ship Date', 'Ship Mode',
       'Customer ID', 'Customer Name', 'Segment', 'Country', 'City', 'State',
       'Postal Code', 'Region', 'Product ID', 'Category', 'Sub-Category',
       'Product Name', 'Quantity', 'Discount', 'Profit']

data.drop(cols, axis =1 , inplace=True)

data = data.sort_values('Order Date')

data = data.set_index('Order Date')

data

data1 = data['Sales'].resample('MS').mean()

data1.plot(figsize=(15,6))
plt.ylabel("Sales",fontsize=18)
plt.xlabel("Order Date",fontsize=18)
plt.title("Order Date Vs Sales",fontsize=20)
plt.show()

from pylab import rcParams as rc

rc['figure.figsize']= 10, 12

"""# **Additive Model**"""

import statsmodels.api as sm
decomposition = sm.tsa.seasonal_decompose(data1,model='additive')

print("Trend \n ")
print(decomposition.trend)
print("Seasonal \n ")
print(decomposition.seasonal)
print("Residual \n ")
print(decomposition.resid)
print("Observed \n ")
print(decomposition.observed)

fig = decomposition.plot()
plt.show

"""# **ARIMA Model Forecasting**"""

import itertools

p=d=q=range(0,2)

pdq = list(itertools.product(p,d,q))

seasonal_pdq = [(x[0],x[1],x[2], 12) for x in pdq]

for param in pdq:
    for param_seasonal in seasonal_pdq:
        try:
            mod = sm.tsa.statespace.SARIMAX(data1,order = param, seasonal_order = param_seasonal ,
                                            enforce_stationarity= False , enforce_invertibility= False )
            results = mod.fit()
            
            print('ARIMA{} x {} 12 -- AIC : {}'.format(param, param_seasonal, results.aic))
            
        except:
             continue



"""# From the above ARIMA model's AIC value, we get that ARIMA model, ARIMA(1, 1, 1) x (1, 1, 0, 12) 12 with AIC : 243.34456553220386 is the best parameter(Best model).

The fitted model is,
"""

model = sm.tsa.statespace.SARIMAX(data1,
                               order=(1,1,1),
                               seasonal_order= (1,1,0,12),
                               enforce_stationarity = False,
                               enforce_invertibility=False)

results = model.fit()

print(results.summary().tables[1])

results.plot_diagnostics(figsize=(16,8))
plt.show()

pred = results.get_prediction(start = pd.to_datetime('2017-01-01'), dynamic = False)
pred_ci = pred.conf_int()

ax = data1['2014':].plot(label= 'observed')

pred.predicted_mean.plot(ax = ax, label = 'One step ahead Forecast',
                        alpha = 7, figsize= (14,7))

ax.fill_between(pred_ci.index,
               pred_ci.iloc[:,0],
               pred_ci.iloc[:,1],color = 'k', alpha= 0.2)

ax.set_xlabel('Date')
ax.set_ylabel('Sales')
plt.legend()

plt.show()

Sales_forecasted = pred.predicted_mean
Sales_original = data1['2017-01-01':]
mse = ((Sales_forecasted- Sales_original) ** 2).mean()

print('MSE of forecast :{}'.format(round(mse,2)))

pred_uc = results.get_forecast(steps = 15)
pred_ci = pred_uc.conf_int()

ax = data1.plot(label='observed', figsize=(10,8))
pred_uc.predicted_mean.plot(ax=ax, label='Forecast')
ax.fill_between(pred_ci.index,
               pred_ci.iloc[:,0],
               pred_ci.iloc[:,1],color='k',alpha=0.6)
ax.set_xlabel('Date')
ax.set_ylabel('Sales')

plt.legend()
plt.show()

pred_uc = results.get_forecast(steps = 100)
pred_ci = pred_uc.conf_int()

ax = data1.plot(label='observed', figsize=(10,8))
pred_uc.predicted_mean.plot(ax=ax, label='Forecast')
ax.fill_between(pred_ci.index,
               pred_ci.iloc[:,0],
               pred_ci.iloc[:,1],color='k',alpha=0.6)
ax.set_xlabel('Date')
ax.set_ylabel('Sales')

plt.legend()
plt.show()