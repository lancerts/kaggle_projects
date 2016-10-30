"""
Logistic Regression

"""

import sqlite3
import pandas as pd
from pandas.io import sql
from sklearn.linear_model import LogisticRegression

conn = sqlite3.connect('C:/Users/tshao/data_Avito Context Ad Clicks/database.sqlite')

# Get train data
query = """
select Position, HistCTR, IsClick from trainSearchStream where ObjectType = 3 limit 100000000, 100000000;
"""
df = sql.read_sql(query, conn)
X = df[['Position', 'HistCTR']]
y = df.IsClick

# Get test data
query_test = """
select TestID, Position, HistCTR from testSearchStream where ObjectType = 3
"""
df_test = sql.read_sql(query_test, conn)
X_test = df_test[['Position', 'HistCTR']]

# Learn
model = LogisticRegression()
model.fit(X, y)
pred = model.predict_proba(X_test)

# Output to csv
filename = 'C:/Users/tshao/Dropbox/kaggle/Avito Context Ad Clicks/submission/glm.csv'
pd.DataFrame({'ID': df_test.TestId, 'IsClick': pred[:, 1]}).to_csv(filename, index=False)

# Zip
# with zipfile.ZipFile(filename + '.zip', 'w', zipfile.ZIP_DEFLATED) as z:
#     z.write(filename)

