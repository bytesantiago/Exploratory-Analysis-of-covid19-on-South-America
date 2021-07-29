# Load the Pandas libraries with alias 'pd' 
import pandas as pd 
# Read data from file 'filename.csv' 
# (in the same directory that your python process is based)
# Control delimiters, rows, column names with read_csv (see later) 
df = pd.read_csv("owid-covid-data.csv") 
# Preview the first 5 lines of the loaded data 
# data.head()

continent_name = ['Asia', 'South America', 'North America', 'Europe', 'Oceania'] 
index = 0

for continent in continent_name:
    resultdf = df.loc[df['continent'] == continent]
    resultdf.to_csv(str(continent_name[index])+'_dataset.csv', index=False) 
    index = index + 1