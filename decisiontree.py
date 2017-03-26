# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import classification_report,confusion_matrix


df=pd.read_csv('loan_data.csv')
dummy_list=['purpose']
final=pd.get_dummies(df,columns=dummy_list,drop_first=True)
X=final.drop(['not.fully.paid'],axis=1)
y=final['not.fully.paid']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)
dt=DecisionTreeClassifier(criterion='entropy')
dt.fit(X_train,y_train)
pred=dt.predict(X_test)
print(confusion_matrix(y_test,pred))
print('\n')
print(classification_report(y_test,pred))