NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
str(NewsTrain) 
NewsTrain$Text = paste (NewsTrain$Headline, NewsTrain$Abstract, sep=".") 
NewsTest$Text = paste (NewsTest$Headline, NewsTest$Abstract, sep=".") 
library(tm)
CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Text, NewsTest$Text))) 
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
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$Monthday = NewsTrain$PubDate$mday
NewsTrain$Month = NewsTrain$PubDate$mon
NewsTrain$Hour = NewsTrain$PubDate$hour
NewsTrain$Weekday = as.factor(NewsTrain$Weekday) 
NewsTrain$Monthday = as.factor(NewsTrain$Monthday) 
NewsTrain$Month = as.factor(NewsTrain$Month) 
NewsTrain$Hour = as.factor(NewsTrain$Hour) 
NewsTrain$TimeOfDay = as.factor(NewsTrain$TimeOfDay) 
NewsTrain$Headline <- NULL 
NewsTrain$Snippet <- NULL 
NewsTrain$Abstract <- NULL 
NewsTrain$PubDate <- NULL 
NewsTrain$Headline <- NULL 
NewsTrain$Snippet <- NULL 
NewsTrain$Abstract <- NULL 
NewsTrain$PubDate <- NULL 
NewsTrain[is.na(NewsTrain)] <- 0 
NewsTrain$Popular <- as.factor(NewsTrain$Popular)
require(caret)
require(randomForest)
set.seed(1980) 
model <- randomForest(Popular ~ . - UniqueID, data = NewsTrain , importance=TRUE, ntree=500)
str(NewsTrain) 
NewsTrain$Text <- NULL 
model <- randomForest(Popular ~ . - UniqueID, data = NewsTrain , importance=TRUE, ntree=500)
summary(model) 
save(model, file="rf_model.Rdata")
savehistory(file="script_final4.R") 
