# -*- coding: utf-8 -*-
"""
Created on Sat Mar 25 21:51:59 2017

@author: Varun
"""

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.metrics import classification_report,confusion_matrix
from sklearn.model_selection import GridSearchCV
#Decision tree
df=pd.read_csv('Rating  (2).csv')
X=df.drop(['Recommendation','Recommend Rating','State','City','Hospital Name','Provider ID'],axis=1)
y=df['Recommendation']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)

dt=DecisionTreeClassifier(criterion='entropy')
dt.fit(X_train,y_train)
pred=dt.predict(X_test)
print(confusion_matrix(y_test,pred))
print('#Decision Tree results')
print(classification_report(y_test,pred))


#Random Forest classifier
rfc=RandomForestClassifier(n_estimators=500)
rfc.fit(X_train,y_train)
pred=rfc.predict(X_test)
print(confusion_matrix(y_test,pred))
print('#Random Forest Results')
print(classification_report(y_test,pred))

#Support vector machines
svc=SVC()
svc.fit(X_train,y_train)
pred=svc.predict(X_test)
print(confusion_matrix(y_test,pred))
print('#SVC Results')
print(classification_report(y_test,pred))
param_grid={'C':[0.1,1,10,100],'gamma':[1,0.1,0.01,0.01],'kernel':['rbf','poly']}
grid=GridSearchCV(SVC(),param_grid,refit=True,verbose=0)
grid.fit(X_train,y_train)
grid_pred=grid.predict(X_test)
print(confusion_matrix(y_test,grid_pred))
print('#SVC Results')
print(classification_report(y_test,grid_pred))
print(grid.best_params_)
print(grid.best_estimator_)


#state wise Geographcal mapping
import plotly.plotly as py
import plotly.graph_objs as go 
from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot

init_notebook_mode(connected=True)


df = pd.read_csv('Rating1.csv')

data = dict(type='choropleth',
            colorscale = 'Viridis',
            locations = df['State'],
            z = df['Recommend Rating'],
            locationmode = 'USA-states',
            #text = df['text'],
            marker = dict(line = dict(color = 'rgb(255,255,255)',width = 2)),
            colorbar = {'title':"Rating"}
            )


layout = dict(title = 'Hospital Rating',
              geo = dict(scope='usa',
                         showlakes = False,
                         lakecolor = 'rgb(85,173,240)')
             )

choromap = go.Figure(data = [data],layout = layout)

iplot(choromap)






