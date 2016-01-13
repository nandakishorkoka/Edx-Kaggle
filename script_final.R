
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



Train$Popular = NewsTrain$Popular
Train$WordCount = NewsTrain$WordCount








Test$WordCount = NewsTest$WordCount



PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")


MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

