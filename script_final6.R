NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

library(tm)
CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract))) 
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtm = DocumentTermMatrix(CorpusAbstract)
sparse = removeSparseTerms(dtm, 0.99)
AbstractWords = as.data.frame(as.matrix(sparse))
colnames(AbstractWords) = make.names(colnames(AbstractWords))

TrainTxt = head(AbstractWords, nrow(NewsTrain))
TestTxt = tail(AbstractWords, nrow(NewsTest))

NewsTrain = cbind(NewsTrain, TrainTxt)
NewsTest = cbind(NewsTest, TestTxt)

NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk) 
NewsTrain$SectionName = as.factor(NewsTrain$SectionName) 
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName) 

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$Monthday = NewsTrain$PubDate$mday
NewsTrain$Weekday = as.factor(NewsTrain$Weekday) 
NewsTrain$Monthday = as.factor(NewsTrain$Monthday) 


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
NewsTrain$TimeOfDay = as.factor(NewsTrain$TimeOfDay) 

NewsTrain$Headline <- NULL 
NewsTrain$Snippet <- NULL 
NewsTrain$Abstract <- NULL 
NewsTrain$PubDate <- NULL 
NewsTrain$Headline <- NULL 
NewsTrain$Snippet <- NULL 
NewsTrain$Abstract <- NULL 
NewsTrain$PubDate <- NULL 
NewsTrain$Hour <- NULL 

NewsTrain[is.na(NewsTrain)] <- 0 
NewsTrain$Popular <- as.factor(NewsTrain$Popular)

require(randomForest)
model <- randomForest(Popular ~ . - UniqueID, data = NewsTrain , importance=TRUE, ntree=500)

NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk) 
NewsTest$SectionName = as.factor(NewsTest$SectionName) 
NewsTest$SubsectionName = as.factor(NewsTest$SubsectionName) 
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$Weekday = NewsTest$PubDate$wday
NewsTest$Monthday = NewsTest$PubDate$mday
NewsTest$Hour = NewsTest$PubDate$hour
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
NewsTest$Weekday = as.factor(NewsTest$Weekday) 
NewsTest$Monthday = as.factor(NewsTest$Monthday) 
NewsTest$TimeOfDay = as.factor(NewsTest$TimeOfDay) 
NewsTest$Headline <- NULL 
NewsTest$Snippet <- NULL 
NewsTest$Abstract <- NULL 
NewsTest$PubDate <- NULL 
NewsTest$Headline <- NULL 
NewsTest$Snippet <- NULL 
NewsTest$Abstract <- NULL 
NewsTest$PubDate <- NULL 
NewsTest$Hour <- NULL
NewsTest[is.na(NewsTest)] <- 0 

levels(NewsTest$NewsDesk) = levels(NewsTrain$NewsDesk)
levels(NewsTest$SectionName) = levels(NewsTrain$SectionName)
levels(NewsTest$SubsectionName) = levels(NewsTrain$SubsectionName)
levels(NewsTest$Weekday) = levels(NewsTrain$Weekday)
levels(NewsTest$Monthday) = levels(NewsTrain$Monthday)
levels(NewsTest$Hour) = levels(NewsTrain$Hour)
levels(NewsTest$TimeOfDay) = levels(NewsTrain$TimeOfDay)
PredTest = predict(model, newdata=NewsTest, type="response")
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "Submission-nkoka.csv", row.names=FALSE)
savehistory(file="script_final6.R") 
