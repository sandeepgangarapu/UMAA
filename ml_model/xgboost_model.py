# -*- coding: utf-8 -*-
"""ROC with and without income

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/12Iosx1VwvlSouti3w_lhMhItWlUdcqpX
"""

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split, StratifiedShuffleSplit
from sklearn.decomposition import PCA
from imblearn.over_sampling import SMOTE
from sklearn.model_selection import GridSearchCV, cross_validate
from imblearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer, KNNImputer
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.metrics import roc_auc_score, confusion_matrix, classification_report, accuracy_score, roc_curve
from xgboost import XGBClassifier
import pickle
"""Load data"""


data = pd.read_csv('data_for_model.csv')
valid_data = pd.read_csv('all_data_for_final.csv')


data = data.set_index('ID_DEMO')
valid_data = valid_data.set_index('ID_DEMO')
print(data.columns)

y = data['target'].ravel()
X = data.drop(['target'], axis=1)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.10, random_state=42, stratify=y)

numeric_features = ['AGE', 'UMN_event',
       'UMN_member', 'UMN_donor', 'UMN_volun', 'UMN_inform', 'UMN_loyalty',
       'UMN_avg_Annual_score_5_years', 'annual_member', 'life_member',
       'non_member', 'Learning_emails', 'Legislature_emails', 'Social_emails',
       'Sports_emails', 'general_ctr_emails', 'Learning_events',
       'Legislature_events', 'Networking_events', 'Other_events',
       'Social_events', 'Sports_events', 'total_type_person_events']
numeric_transformer = Pipeline(steps=[
    ('scaler', StandardScaler())])

categorical_features = ['MARITAL_STATUS']
categorical_transformer = Pipeline(steps=[
                                          ('imputer', SimpleImputer(strategy='constant', fill_value='missing')),
                                          ('onehot', OneHotEncoder(drop='first'))])

preprocessor = ColumnTransformer(
    transformers=[
        ('num', numeric_transformer, numeric_features),
       ('cat', categorical_transformer, categorical_features)
       ], n_jobs=-1)

# Append classifier to preprocessing pipeline.
# Now we have a full prediction pipeline.


pipe = Pipeline(steps=[('preprocessor', preprocessor),
                      ('smt', SMOTE(random_state=42, sampling_strategy=1)),
                      ('pca', PCA(n_components='mle'))])


pipe.steps.append(['clf', XGBClassifier(learning_rate=0.01, n_estimators=5000, max_depth=2,
 min_child_weight=5, gamma=0.0, subsample=0.6, colsample_bytree=0.8, seed=27, reg_alpha=0.00002)])


pipe.fit(X_train, y_train)


y_pred_prob = pipe.predict_proba(X_test)
y_pred = pipe.predict(X_test)
print(roc_auc_score(y_test, y_pred_prob[:,1]))
print(confusion_matrix(y_test, y_pred))

filename = 'finalized_model.sav'
pickle.dump(pipe, open(filename, 'wb'))

