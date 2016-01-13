NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
library(tm)
CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
#CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtm = DocumentTermMatrix(CorpusAbstract)
sparse = removeSparseTerms(dtm, 0.99)
AbstractWords = as.data.frame(as.matrix(sparse))
colnames(AbstractWords) = make.names(colnames(AbstractWords))
TrainTxt = head(AbstractWords, nrow(NewsTrain))
TestTxt = tail(AbstractWords, nrow(NewsTest))
str(TrainTxt) 
NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk) 
NewsTrain$SectionName = as.factor(NewsTrain$SectionName) 
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName) 
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
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$Monthday = NewsTrain$PubDate$mday
NewsTrain$Month = NewsTrain$PubDate$mon
NewsTrain$Hour = NewsTrain$PubDate$hour
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
NewsTrain2$Headline <- NULL 
NewsTrain2$Snippet <- NULL 
NewsTrain2$Abstract <- NULL 
NewsTrain2$PubDate <- NULL 
NewsTrain$Headline <- NULL 
NewsTrain$Snippet <- NULL 
NewsTrain$Abstract <- NULL 
NewsTrain$PubDate <- NULL 
NewsTrain[is.na(NewsTrain)] <- 0 
NewsTrain$Popular <- as.factor(NewsTrain$Popular)
require(caret)
require(randomForest)
set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain$Popular, p = 0.75 , list=FALSE)
train = NewsTrain[inTrain,] 
test = NewsTrain[-inTrain,] 
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=500) 
library(ROCR) 
pred_labels = predict(model, newdata=test, type="prob") 
pred = prediction(pred_labels[,2], test$Popular) 
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print auc
auc
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=1000) 
pred_labels = predict(model, newdata=test, type="prob") 
pred = prediction(pred_labels[,2], test$Popular) 
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=300) 
pred_labels = predict(model, newdata=test, type="prob") 
pred = prediction(pred_labels[,2], test$Popular) 
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=200) 
pred_labels = predict(model, newdata=test, type="prob") 
pred = prediction(pred_labels[,2], test$Popular) 
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
perf <- performance(pred,"tpr","fpr")
plot(perf,col="black",lty=3, lwd=3)
save(model, file="rf_model.Rdata")
str(TrainTxt) 
cbind(NewsTrain, TrainTxt)
str(NewsTrain) 
NewsTrain2 = cbind(NewsTrain, TrainTxt)
set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain2$Popular, p = 0.75 , list=FALSE)
train = NewsTrain2[inTrain,] 
test = NewsTrain2[-inTrain,] 
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=300) 
pred_labels = predict(model, newdata=test, type="prob") 
pred = prediction(pred_labels[,2], test$Popular) 
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
incr = seq(25,500, by=25) 
for (i in incr) { 
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=i)
pred_labels = predict(model, newdata=test, type="prob") 
pred = prediction(pred_labels[,2], test$Popular) 
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print (paste("AUC: ", auc, sep=" ") 
} 
for (i in incr) {
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=i)
pred_labels = predict(model, newdata=test, type="prob") 
pred = prediction(pred_labels[,2], test$Popular) 
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print (paste("AUC: ", auc, sep=" ")) 
}
savehistory(file="script_final2.R") 
