# TFM
The project consist on two folders:

# Data
It contains 3 folders, one for each NCBI/GEO serie. Each one of these folders contains gene counts file extracted from NCBI/GEO database as well as a file containing severity labels for each patient.

# Scripts
This folder contains 6 R scripts with which the exposed results can be reproduced.
## 1-Data Preprocessing.R
The data are load and preprocessed. The script ends with the building of the final gene expression matrix.
## 2-DEGs.R
DEGs are extracted from the gene expression matrix.
## 3-Canonic Proteins.R
DEGs canonic proteins are extracted via UniProt
## 4-INPROF.R
The protein list for each COVID-19 patient is queried to INPROF and a dataset with all patient proteins INPROF information is built.
## 5-tsne+mRMR.R
tSNE algorithm is performed using INPROF data and mRMR feature ranking is computed.
## 6-Models.R
SVM, Random-Forest and k-NN models are trained using INPROF features incrementally following mRMR order.
## 7-tsne+mRMR biclass.R
Biclass data set is built and tSNE algorithm is performed using it and its mRMR feature ranking is computed.
## 8-Models biclass.R
SVM, Random-Forest and k-NN models are trained using INPROF features incrementally following mRMR order.
