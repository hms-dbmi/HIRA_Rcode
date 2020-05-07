import pandas as pd
import numpy as np

# Input data  ===================
gnl = pd.read_csv("GNL_CD-all-codes-drug.csv", encoding='utf8')
gnl['Med_Name']= np.nan

ce = pd.read_csv("4CE_Phase1.1_File_Descriptions_Meds.tsv", sep="\t").sort_values(by=['Med_Name'])
ce['Med_Name']=ce['Med_Name'].str.lower()

# Partial matching  ===================
for drug in ce['Med_Name'].str.lower(): 
    gnl_index=gnl.index[gnl['GEN_SHORT'].str.lower().str.contains(drug)==True]
    gnl.at[gnl_index,'Med_Name']=drug

mixed= gnl.loc[~gnl['Med_Name'].isnull()]

# Merge DFs  ===================
all_df= pd.merge(mixed, ce, on ='Med_Name', how='inner').sort_values(by=['Med_Name']).drop(columns=['GEN_SHORT','GEN_LONG'])
gnl_all = pd.merge(gnl,all_df,  on ='GNL_CD', how='outer').sort_values(by=['GEN_SHORT'])

print(len(all['Med_Name'].unique()))
100*(len(all['Med_Name'].unique())/len(ce['Med_Name'].unique()))

# Drugs not found  ===================
not_found=list(set(ce['Med_Name'].unique().tolist())-set(all['Med_Name'].unique().tolist()))
nf=pd.DataFrame({'Med_Name':not_found}).sort_values(by=['Med_Name'])

# Save Data  ===================
gnl_all.to_csv("4CE_in_GNL_drug_overlap.tsv", sep='\t', header=True, index=False)
nf.to_csv("4CE_not_in_GNL.tsv", sep='\t', header=True, index=False)
