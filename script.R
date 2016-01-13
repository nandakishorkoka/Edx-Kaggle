NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
str(NewsTrain) 
NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk) 
NewsTrain$SectionName = as.factor(NewsTrain$SectionName) 
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName) 
NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk) 
NewsTest$SectionName = as.factor(NewsTest$SectionName) 
NewsTest$SubsectionName = as.factor(NewsTest$SubsectionName) 
str(NewsTrain) 
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday
str(NewsTrain) 
NewsTrain$Weekday = as.factor(NewsTrain$Weekday) 
NewsTest$Weekday = as.factor(NewsTest$Weekday) 
NewsTrain[0] 
NewsTrain[0,] 
NewsTrain[1,] 
NewsTrain[1,'PubDate'] 
NewsTrain[1,'PubDate'] 
NewsTrain$Weekday = NewsTrain$PubDate$mday
NewsTest$Weekday = NewsTest$PubDate$mday
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday
NewsTrain$Monthday = NewsTrain$PubDate$mday
NewsTest$Monthday = NewsTest$PubDate$mday
NewsTest$Month = NewsTest$PubDate$mon
NewsTrain$Month = NewsTrain$PubDate$mon
NewsTrain$TimeOfDay = if (NewsTrain$Hour >= 0 && NewsTrain$Hour < 6 ) 0 else { if (NewsTrain$Hour >= 6 && NewsTrain$Hour < 12 )   
NewsTest$Hour = NewsTest$PubDate$hour
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
if (NewsTest$Hour >= 0 && NewsTest$Hour < 6 ) { 
NewsTest$TimeOfDay = 0 
} else { 
if (NewsTest$Hour >= 6 && NewsTest$Hour < 12 ) { 
NewsTest$TimeOfDay = 1 
} else { 
if (NewsTest$Hour >= 12 && NewsTest$Hour < 18 ) { 
NewsTest$TimeOfDay = 2 
} else { 
NewsTest$TimeOfDay = 3 
}
}
} 
str(NewsTrain) 
NewsTrain$Weekday = as.factor(NewsTrain$Weekday) 
NewsTrain$Monthday = as.factor(NewsTrain$Monthday) 
NewsTrain$Month = as.factor(NewsTrain$Month) 
NewsTrain$Hour = as.factor(NewTrain$Hour) 
NewsTrain$Hour = as.factor(NewsTrain$Hour) 
NewsTrain$TimeOfDay = as.factor(NewsTrain$TimeOfDay) 
NewsTest$Weekday = as.factor(NewsTest$Weekday) 
NewsTest$Monthday = as.factor(NewsTest$Monthday) 
NewsTest$Month = as.factor(NewsTest$Month) 
NewsTest$Hour = as.factor(NewsTest$Hour) 
NewsTest$TimeOfDay = as.factor(NewsTest$TimeOfDay) 
str(NewsTest) 
str(NewsTrain) ) 
str(NewsTrain)  
require (tm) 
train.corpus = Corpus(VectorSource(NewsTrain$Abstract))
train.corpus = tm_map(train.corpus, tolower)
train.corpus = tm_map(train.corpus, removePunctuation)
train.corpus = tm_map(train.corpus, removeWords, stopwords('english'))
train.dtm = TermDocumentMatrix (train.corpus) 
train.dtm
findFreqTerms(tran.dtm, lowfreq=3)
findFreqTerms(train.dtm, lowfreq=3)
findFreqTerms(train.dtm, lowfreq=30)
findFreqTerms(train.dtm, lowfreq=50)
findFreqTerms(train.dtm, lowfreq=40)
findFreqTerms(train.dtm, lowfreq=50)
train.dtm2 = removeSparseTerms (train.dtm, sparse=0.95)
train.dtm2 
findFreqTerms(train.dtm2, lowfreq=50) 
findFreqTerms(train.dtm2, lowfreq=20) 
findFreqTerms(train.dtm2, lowfreq=10) 
findFreqTerms(train.dtm2, lowfreq=0) 
findFreqTerms(train.dtm2, lowfreq=100) 
train.dtm2 = removeSparseTerms (train.dtm, sparse=0.70)
findFreqTerms(train.dtm2, lowfreq=10) 
train.dtm2 = removeSparseTerms (train.dtm, sparse=0.99)
findFreqTerms(train.dtm2, lowfreq=10) 
train.dtm2 = removeSparseTerms (train.dtm, sparse=0.98)
findFreqTerms(train.dtm2, lowfreq=20) 
findFreqTerms(train.dtm2, lowfreq=10) 
train.dtm2 = removeSparseTerms (train.dtm, sparse=0.99)
findFreqTerms(train.dtm2, lowfreq=10) 
train.txtdf <- as.data.frame(inspect(train.dtm2)) 
nrow(train.txtdf) 
ncol(train.txtdf)
nrow(NewsTran) 
nrow(NewsTrain) 
str( train.txtdf) 
train.txtdf
str(train.txtdf) 
colnames(train.txtdf) 
rownames(train.txtdf) 
findFreqTerms(train.dtm2, lowfreq=5) 
scale(train.txtdf) 
train.txtdf
train.txtdf[which (1 > 1),]
colnames(train.txtdf) 
train.txtdf[which ("1" > 1),]
train.txtdf[which ("1" = 1),]

train.txtdf2 = scale(train.txtdf) 
train.txtdf2
train.txtdf2 = as.data.frame(scale(train.txtdf)) 
train.txtdf2
t(train.txtdf2) 
 NewsTrain2 = cbind(NewsTrain, t(train.txtdf2))  
nrow(NewsTrain2) 
ncol(NewsTrain2) 
str(NewsTrain2) 
head(NewsTrain) 
NewsTrain[1,]
NewsTrain[2,]
NewsTrain[6300,]
NewsTrain[5200,]
NewsTrain[4200,]
NewsTrain[3200,]
NewsTrain[2200,]
NewsTrain[1200,]
NewsTrain[1400,]
ncol(NewsTrain2) 
library(caret) 
set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain2$Popular, p = 0.75 , list=FALSE) 
train = NewsTrain[inTrain,] 
test = NewsTrain[-inTrain,] 
nrow(train) 
NewsTrain2[is.na(NewsTrain2)] <- 0 
set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain2$Popular, p = 0.75 , list=FALSE)
train = NewsTrain2[inTrain,] 
test = NewsTrain2[-inTrain,] 
str(train) 
NewsTrain2$Headline <- NULL 
NewsTrain2$Snippet <- NULL 
NewsTrain2$Abstract <- NULL 
str(NewsTrain2) 
NewsTrain2$PubDate <- NULL 
str(NewsTrain2) 
library(randomForest) 
set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain2$Popular, p = 0.75 , list=FALSE)
train = NewsTrain2[inTrain,] 
test = NewsTrain2[-inTrain,] 
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=200) 
names(NewsTrain2)[names(NewsTrain2) == '2015'] <- 'x2015'
str(NewsTrain2) 
set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain2$Popular, p = 0.75 , list=FALSE)
train = NewsTrain2[inTrain,] 
test = NewsTrain2[-inTrain,] 
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=200) 
 NewsTrain2 = cbind(NewsTrain, t(train.txtdf2))  
train.txtdf2
train.txtdf3 = t(train.txtdf2)
colnames(train.txtdf3) 
colnames(train.txtdf3) = paste("text", colnames(train.txtdf3), sep="_")   
NewsTrain2 = cbind(NewsTrain,train.txtdf3) 
str(NewsTrain2) 
NewsTrain2$Headline <- NULL 
NewsTrain2$Snippet <- NULL 
NewsTrain2$Abstract <- NULL 
str(NewsTrain2)
NewsTrain2$PubDate <- NULL 
set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain2$Popular, p = 0.75 , list=FALSE)
train = NewsTrain2[inTrain,] 
test = NewsTrain2[-inTrain,] 
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=200) 
NewsTrain2[is.na(NewsTrain2)] <- 0 
set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain2$Popular, p = 0.75 , list=FALSE)
train = NewsTrain2[inTrain,] 
test = NewsTrain2[-inTrain,] 
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=200) 
NewsTrain2$Popular <- as.factor(NewsTrain2$Popular)
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=200) 
savehistory(file="script.R") 
set.seed(1980) 
inTrain <- createDataPartition ( y=NewsTrain2$Popular, p = 0.75 , list=FALSE)
train = NewsTrain2[inTrain,] 
test = NewsTrain2[-inTrain,] 
model <- randomForest(Popular ~ . , data = train , importance=TRUE, ntree=200) 
pred_labels = predict(model, type="prob", newdata=test) 
library(ROCR) 
pred = prediction(pred_labels, test$Popular) 
len(pred_labels) 
length(pred_labels) 
pred_labels
pred_labels = predict(model, newdata=test) 
pred_labels
pred = prediction(pred_labels, test$Popular) 
pred_labels = predict(model, newdata=test, type="prob") 
pred = prediction(pred_labels[,2], test$Popular) 
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
perf <- performance(pred,"tpr","fpr")
plot(perf,col="black",lty=3, lwd=3)
plot(perf,col="black",lty=3, lwd=3)
savehistory(file="script.R") 
