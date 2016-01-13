NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk) 
NewsTrain$SectionName = as.factor(NewsTrain$SectionName) 
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName) 

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")

NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$Monthday = NewsTrain$PubDate$mday
NewsTrain$Month = NewsTrain$PubDate$mon
NewsTrain$Hour = NewsTrain$PubDate$hour
if (NewsTrain$Hour >= 0 && NewsTrain$Hour < 6 ) { 
NewsTrain$TimeOfDay = 0 
} else { 
if (NewsTrain$Hour >= 6 && NewsTrain$Hour < 12 ) { 
NewsTrain$TimeOfDay = 1 
} else { 
if (NewsTrain$Hour >= 12 && NewsTrain$Hour < 18 ) { 
NewsTrain$TimeOfDay = 2 
} else { 
NewsTrain$TimeOfDay = 3 
}
}
}

NewsTrain$Weekday = as.factor(NewsTrain$Weekday) 
NewsTrain$Monthday = as.factor(NewsTrain$Monthday) 
NewsTrain$Month = as.factor(NewsTrain$Month) 
NewsTrain$Hour = as.factor(NewsTrain$Hour) 
NewsTrain$TimeOfDay = as.factor(NewsTrain$TimeOfDay) 

NewsTrain$Headline <- NULL 
NewsTrain$Snippet <- NULL 
NewsTrain$Abstract <- NULL 
NewsTrain$PubDate <- NULL 
NewsTrain$UniqueID <- NULL 

NewsTrain[is.na(NewsTrain)] <- 0 
NewsTrain$Popular <- as.factor(NewsTrain$Popular)

require(caret)
require(randomForest)

set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain2$Popular, p = 0.75 , list=FALSE)
train = NewsTrain2[inTrain,] 
test = NewsTrain2[-inTrain,] 
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=300) 


library(ROCR) 

pred_labels = predict(model, newdata=test, type="prob") 
pred = prediction(pred_labels[,2], test$Popular) 
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print (paste("AUC: ", auc, sep=" ")) 

perf <- performance(pred,"tpr","fpr")
plot(perf,col="black",lty=3, lwd=3)

save(model, file="rf_model.Rdata")
