# TFM
The project consist on two folders:

# Data
It contains 3 folders, one for each NCBI/GEO serie. Each one of these folders contains gene counts file extracted from NCBI/GEO database as well as a file containing severity labels for each patient.

# Scripts
This folder contains 6 R scripts with which the exposed results can be reproduced.
## 1-Data Preprocessing
The data are load and preprocessed. The script ends with the building of the final gene expression matrix.
## 2-DEGs
DEGs are extracted from the gene expression matrix.
## 3-Canonic Proteins
DEGs canonic proteins are extracted via UniProt
## 4-INPROF
The protein list for each COVID-19 patient is queried to INPROF and a dataset with all patient proteins INPROF information is built.
## 5-tsne+mRMR
tSNE algorithm is performed using INPROF data and mRMR feature ranking is computed.
## 6-SVM
SVM models are trained using INPROF features incrementally following mRMR order.
