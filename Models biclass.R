library(KnowSeq)
library(caret)

#SVM
svm.2features.biclass <- svm_trn(data=data.biclass[,-45],
                                 labels = data.biclass$label,
                                 vars_selected = names(feature.ranking.alignment.biclass)[1:2],
                                 numFold = 159)

svm.accuracy.means.biclass= c()
svm.accuracy.sds.biclass = c()
confusion.matrixes.biclass <- vector(mode = "list", length = 44)


for (i in c(2:44)){
  print(i)
  model<-svm_trn(data=data.biclass[,-45],
                 labels = data.biclass$label,
                 vars_selected = names(feature.ranking.alignment.biclass)[1:i],
                 numFold = 159)
  confusion.matrix = svm.2features.biclass$cfMats[[1]]$table
  confusion.matrix[2,2] = 0
  svm.accuracy.means.biclass<- c(svm.accuracy.means.biclass, model$accuracyInfo$meanAccuracy)
  svm.accuracy.sds.biclass <- c(svm.accuracy.sds.biclass, model$accuracyInfo$standardDeviation)
  for (j in c(1:159)){
    confusion.matrix <- confusion.matrix+model$cfMats[[j]]$table
  }
  confusion.matrixes.biclass[[i]]<-confusion.matrix
}


#Get metrics
f1.biclass <- c()
sensitivity.biclass <- c()
specificity.biclass <- c()
precision.biclass <- c()

for (i in c(2:44)){
  stats <- confusionMatrix(confusion.matrixes.biclass[[i]])
  f1.biclass <- c(f1.biclass,stats$byClass[7])
  sensitivity.biclass <- c(sensitivity.biclass , stats$byClass[1])
  specificity.biclass  <- c(specificity.biclass , stats$byClass[2])
  precision.biclass  <- c(precision.biclass , stats$byClass[5])
  
}

names(precision.biclass)<-NULL
names(f1.biclass)<-NULL
names(sensitivity.biclass)<-NULL
names(specificity.biclass)<-NULL

#Plot them
x.biclass<-c(2:44)

plot.new()
plot(x.biclass,f1.biclass,cex = .8,xlab="Amount of Features",ylab="Value",col="blue",type = 'line',ylim = c(0.6,1))
lines(x.biclass,sensitivity.biclass,cex = .8,col="red")
lines(x.biclass,specificity.biclass,cex = .8,col="green")
lines(x.biclass,precision.biclass,cex = .8,col="black")
title('Metrics')
legend(x=32,y=0.7,c("F1","Sensitivity",'Specificity', 'Precision'),
       cex=1.2,col=c("blue","red","green",'black'),pch=c(1,1,1,1))


which(f1.biclass==max(f1.biclass))
which(sensitivity.biclass==max(sensitivity.biclass))
which(specificity.biclass==max(specificity.biclass))
which(precision.biclass==max(precision.biclass))



#Random Forest
rf.2features.biclass <- rf_trn(data=data.biclass[,-45],
                               labels = data.biclass$label,
                               vars_selected = names(feature.ranking.alignment)[1:2],
                               numFold = 159)

rf.accuracy.means.biclass= c()
rf.accuracy.sds.biclass = c()
rf.confusion.matrixes.biclass <- vector(mode = "list", length = 44)


for (i in c(2:44)){
  print(i)
  model<-rf_trn(data=data.biclass[,-45],
                labels = data.biclass$label,
                vars_selected = names(feature.ranking.alignment)[1:i],
                numFold = 159)
  confusion.matrix = rf.2features.biclass$cfMats[[1]]$table
  confusion.matrix[2,2] = 0
  rf.accuracy.means.biclass<- c(rf.accuracy.means.biclass, model$accuracyInfo$meanAccuracy)
  rf.accuracy.sds.biclass <- c(rf.accuracy.sds.biclass, model$accuracyInfo$standardDeviation)
  for (j in c(1:159)){
    confusion.matrix <- confusion.matrix+model$cfMats[[j]]$table
  }
  rf.confusion.matrixes.biclass[[i]]<-confusion.matrix
}

f1.biclass <- c()
sensitivity.biclass <- c()
specificity.biclass <- c()
precision.biclass <- c()

for (i in c(2:44)){
  stats <- confusionMatrix(rf.confusion.matrixes.biclass[[i]])
  f1.biclass <- c(f1.biclass,stats$byClass[7])
  sensitivity.biclass <- c(sensitivity.biclass , stats$byClass[1])
  specificity.biclass  <- c(specificity.biclass , stats$byClass[2])
  precision.biclass  <- c(precision.biclass , stats$byClass[5])
  
}

names(precision.biclass)<-NULL
names(f1.biclass)<-NULL
names(sensitivity.biclass)<-NULL
names(specificity.biclass)<-NULL

x.biclass<-c(2:44)

plot.new()
plot(x.biclass,f1.biclass,cex = .8,xlab="Amount of Features",ylab="Value",col="blue",type = 'line',ylim = c(0.6,1))
lines(x.biclass,sensitivity.biclass,cex = .8,col="red")
lines(x.biclass,specificity.biclass,cex = .8,col="green")
lines(x.biclass,precision.biclass,cex = .8,col="black")
title('Metrics')
legend(x=32,y=0.7,c("F1","Sensitivity",'Specificity', 'Precision'),
       cex=1.2,col=c("blue","red","green",'black'),pch=c(1,1,1,1))


which(f1.biclass==max(f1.biclass))
which(sensitivity.biclass==max(sensitivity.biclass))
which(specificity.biclass==max(specificity.biclass))
which(precision.biclass==max(precision.biclass))




#KNN
knn.2features.biclass <- knn_trn(data=data.biclass[,-45],
                                 labels = data.biclass$label,
                                 vars_selected = names(feature.ranking.alignment)[1:2],
                                 numFold = 159)

knn.accuracy.means.biclass= c()
knn.accuracy.sds.biclass = c()
knn.confusion.matrixes.biclass <- vector(mode = "list", length = 44)


for (i in c(2:44)){
  print(i)
  model<-knn_trn(data=data.biclass[,-45],
                 labels = data.biclass$label,
                 vars_selected = names(feature.ranking.alignment)[1:i],
                 numFold = 159)
  confusion.matrix = knn.2features.biclass$cfMats[[1]]$table
  confusion.matrix[2,2] = 1
  knn.accuracy.means.biclass<- c(knn.accuracy.means.biclass, model$accuracyInfo$meanAccuracy)
  kn.accuracy.sds.biclass <- c(knn.accuracy.sds.biclass, model$accuracyInfo$standardDeviation)
  for (j in c(1:159)){
    confusion.matrix <- confusion.matrix+model$cfMats[[j]]$table
  }
  knn.confusion.matrixes.biclass[[i]]<-confusion.matrix
}

f1.biclass <- c()
sensitivity.biclass <- c()
specificity.biclass <- c()
precision.biclass <- c()

for (i in c(2:44)){
  stats <- confusionMatrix(knn.confusion.matrixes.biclass[[i]])
  f1.biclass <- c(f1.biclass,stats$byClass[7])
  sensitivity.biclass <- c(sensitivity.biclass , stats$byClass[1])
  specificity.biclass  <- c(specificity.biclass , stats$byClass[2])
  precision.biclass  <- c(precision.biclass , stats$byClass[5])
  
}

names(precision.biclass)<-NULL
names(f1.biclass)<-NULL
names(sensitivity.biclass)<-NULL
names(specificity.biclass)<-NULL

x.biclass<-c(2:44)

plot.new()
plot(x.biclass,f1.biclass,cex = .8,xlab="Amount of Features",ylab="Value",col="blue",type = 'line',ylim = c(0.5,1))
lines(x.biclass,sensitivity.biclass,cex = .8,col="red")
lines(x.biclass,specificity.biclass,cex = .8,col="green")
lines(x.biclass,precision.biclass,cex = .8,col="black")
title('Metrics')
legend(x=32,y=0.62,c("F1","Sensitivity",'Specificity', 'Precision'),
       cex=1.2,col=c("blue","red","green",'black'),pch=c(1,1,1,1))


which(f1.biclass==max(f1.biclass))
which(sensitivity.biclass==max(sensitivity.biclass))
which(specificity.biclass==max(specificity.biclass))
which(precision.biclass==max(precision.biclass))


