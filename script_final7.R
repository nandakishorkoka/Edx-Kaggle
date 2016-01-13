NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
str(NewsTrain) 
NewsTrain$Headline_numwords = sapply(gregexpr("\\W+", NewsTrain$Headline), length) + 1
head(NewsTrain) 
NewsTrain$Snippet_numwords = sapply(gregexpr("\\W+", NewsTrain$Snippet), length) + 1
head(NewsTrain) 
install.packages ("qdap") 
sentences <- data.table(sentSplit(NewsTrain, "Snippet"))
library(data.table)
sentences <- data.table(sentSplit(NewsTrain, "Snippet"))
library(qdap)
sentences <- data.table(sentSplit(NewsTrain, "Snippet"))
sentences
nrow(sentences)
nrow(NewsTrain) 
install.packages("openNLP")
install.packages("openNLPmodels.en")
library(openNLP)
library(openNLPmodels.en)
sentences = seq(nrow(data.table(sentSplit(NewsTrain, "Snippet"))))
sentences 
sentences = nrow(data.table(sentSplit(NewsTrain, "Snippet")))
sentences 
sentences = apply(NewsTrain, 1, function(x) nrow(sentSplit(x['Snippet']))
)
sentences = apply(NewsTrain, 1, function(x) length(gregexpr('[[:alnum:] ][.!?]', x['Snippet'])[[1]] )))
sentences = apply(NewsTrain, 1, function(x) length(gregexpr('[[:alnum:] ][.!?]', x['Snippet'])[[1]] ))
sentences
NewsTrain$Snippet_numsent = apply(NewsTrain, 1, function(x) length(gregexpr('[[:alnum:] ][.!?]', x['Snippet'])[[1]] ))
str(NewsTrain) 
polarity(NewsTrain$Headline)$all$polarity
NewsTrain$Headline_sentiment = polarity(NewsTrain$Headline)$all$polarity
NewsTrain$Snippet_sentiment = polarity(NewsTrain$Snippet)$all$polarity
str(NewsTrain) 
pos("this is great") 
pos("this is great")$POSfreq 
as.data.frame(pos("this is great")$POSfreq)
as.data.frame(pos("this is great")$POSrnp)
s = as.data.frame(pos("this is great")$POSrnp)
s
s = as.data.frame(pos("this is great")$POSrnp)$VBZ
s
s = as.data.frame(pos("this is great")$POSfreq)$VBZ
s
s = as.data.frame(pos("show me the money")$POSfreq)
s
as.data.frame(pos("show me the money to john")$POSfreq)
as.data.frame(pos("show john the money")$POSfreq)
as.data.frame(pos("Nanda will be off next week")$POSfreq)
as.data.frame(pos("Nanda")$POSfreq)
str(NewsTrain) 
pos("Nanda will be off next week")$POSfreq
pos("Nanda will be off next week")$POSfreq$NN
spos("Nanda will be off next week")$POSfreq$NN
s = pos("Nanda will be off next week")$POSfreq$NN
s
NewsTrain$Headline_numnouns = pos(NewsTrain$Headline)$POSfreq$NN
head(NewsTrain) 
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$Monthday = NewsTrain$PubDate$mday
NewsTrain$Weekday = as.factor(NewsTrain$Weekday) 
NewsTrain$Monthday = as.factor(NewsTrain$Monthday) 
NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk) 
NewsTrain$SectionName = as.factor(NewsTrain$SectionName) 
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName) 
str(NewsTrain) 
NewsTrain$Text = paste(NewsTrain$Headline, NewsTrain$Snippet, NewsTrain$Absract) 
NewsTrain$Text = paste(NewsTrain$Headline, NewsTrain$Snippet, NewsTrain$Absract, sep=".") 
savehistory(file="script_final7.R") 
