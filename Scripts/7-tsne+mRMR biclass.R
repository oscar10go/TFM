require(KnowSeq)
require(tidyverse)

#Change from 3 classes to 2 classes
data.biclass <- data.alignment.final[,-45]
data.biclass <- data.biclass %>% mutate(label = c(rep("Hosp",11),rep('OUT',130), rep('Hosp',18)))

#Get t-SNE
train <-data.biclass
Labels<-train$label
train$label<-as.factor(train$label)

tsne <- Rtsne(train[,-45], dims = 2, perplexity=20, verbose=TRUE,
              max_iter = 500,check_duplicates=FALSE,pca=FALSE,pca_center = FALSE,pca_scale=FALSE)

## Plotting
x.hosp<-tsne$Y[,1][c(1:11,142:159)]
y.hosp<-tsne$Y[,2][c(1:11,142:159)]

x.out<-tsne$Y[,1][c(12:141)]
y.out<-tsne$Y[,2][c(12:141)]
plot(x.out,y.out,cex = .8,pch=1,xlab="x axis",ylab="y axis",col="blue")
points(x.hosp,y.hosp,cex = .8,pch=2,col="red")

legend(x=7,y=-10,c("Hosp (29)","Out (130)"),cex=1.2,col=c("red","blue"),pch=c(2,1))

feature.ranking.alignment.biclass <- featureSelection(
  data = data.biclass[,-45],
  labels = data.alignment.final$label,
  vars_selected = variable.names(data.alignment.final)[-45],
  mode = "mrmr")

#Check if mRMR ranking differs from 3 classes mRMR ranking
feature.ranking.alignment <- featureSelection(
  data = data.alignment.final[,-45],
  labels = data.alignment.final$label,
  vars_selected = variable.names(data.alignment.final)[-45],
  mode = "mrmr")

