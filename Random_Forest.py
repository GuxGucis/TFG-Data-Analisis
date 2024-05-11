import pandas as pd
import numpy as np
from sklearn.preprocessing import OneHotEncoder
from sklearn.impute import SimpleImputer
from sksurv.ensemble import RandomSurvivalForest
from sksurv.util import Surv
from lifelines import KaplanMeierFitter
from lifelines.statistics import multivariate_logrank_test
import matplotlib.pyplot as plt
import seaborn as sns

# Set the base URL for your data
baseurl = "D:/gugui/Documentos/Universidad/TFG/"

# Load the data
df_rf = pd.read_csv(baseurl + "data/ANALITIC_2.csv", sep=",")

# Data preparation
print('------------------- PREPARACIÓN -------------------')
df_rf['Estado'] = np.select(
    [
        (df_rf['Hemodialisis'] == 1) & (df_rf['Transplante'] == 0),
        (df_rf['Hemodialisis'] == 0) & (df_rf['Transplante'] == 1),
        (df_rf['Hemodialisis'] == 1) & (df_rf['Transplante'] == 1),
        (df_rf['Hemodialisis'] == 0) & (df_rf['Transplante'] == 0)
    ],
    ['hemodialisis', 'transplante', 'ambas', 'nada']
)

df_rf.drop(columns=['Hemodialisis', 'Transplante'], inplace=True)
df_rf = df_rf.sort_values(by=['ID', 'fechatoma'])

# df_rf['dias_transcurridos'] = df_rf.groupby('ID')['fechatoma'].transform(lambda x: x.diff().dt.days.fillna(0).round())
# Convert 'fechatoma' to datetime format
df_rf['fechatoma'] = pd.to_datetime(df_rf['fechatoma'])
# Ensure the rest of your data manipulation steps are correctly handling datetime
df_rf = df_rf.sort_values(by=['ID', 'fechatoma'])
df_rf['dias_transcurridos'] = df_rf.groupby('ID')['fechatoma'].transform(
    lambda x: x.diff().dt.days.fillna(0).astype(int)
)

df_rf['FGE_microten'] = df_rf.groupby('ID')['FGE'].transform(lambda x: (x.shift(1) > x).astype(int))

df_rf['Fallecido'] = df_rf['Fallecido'].map({0: 'Vivo', 1: 'Fallecido'})
df_rf['Estado'] = df_rf['Estado'].astype('category')

# Random Forest Model
print('------------------- MODELO DE RANDOM FOREST -------------------')

# # Define the survival data
# Y = Surv.from_dataframe("FGE_microten", "dias_transcurridos", df_rf)
#
# # Define the covariates
# X = df_rf[df_rf.columns[8:]]
#
# # Fit the model
# rsf = RandomSurvivalForest(n_estimators=10, random_state=20, n_jobs=-1)
# rsf.fit(X, Y)

# One-hot encode the categorical variables
encoder = OneHotEncoder()
encoded_features = encoder.fit_transform(df_rf[['Estado']])

# Create a new DataFrame from the encoded features
encoded_df = pd.DataFrame(encoded_features.toarray(), columns=encoder.get_feature_names_out())

# Drop the original 'Estado' column and concatenate the new encoded DataFrame
df_rf = df_rf.drop('Estado', axis=1)
df_rf = pd.concat([df_rf.reset_index(drop=True), encoded_df.reset_index(drop=True)], axis=1)

# Impute numerical features
num_imputer = SimpleImputer(strategy='mean')
X_numerical = pd.DataFrame(num_imputer.fit_transform(df_rf.select_dtypes(include=[np.number])),
                           columns=df_rf.select_dtypes(include=[np.number]).columns)

# Concatenate the numerical and categorical data
X_final = pd.concat([X_numerical, df_rf.select_dtypes(exclude=[np.number])], axis=1)

# Define the survival data
Y = Surv.from_dataframe("FGE_microten", "dias_transcurridos", X_final)

# Fit the Random Survival Forest model
rsf = RandomSurvivalForest(n_estimators=10, random_state=20, n_jobs=-1)
rsf.fit(X_final, Y)

# Calculate variable importance
vimp_results = rsf.feature_importances_

# Print variable importance results
print('Variable Importance Results:')
print(vimp_results)

# Plotting variable importance
vimp_data = pd.DataFrame({
    'Variable': X_final.columns,
    'Importance': vimp_results
}).sort_values(by='Importance', ascending=False)

sns.barplot(data=vimp_data, x='Importance', y='Variable')
plt.title('Variable Importance')
plt.show()
