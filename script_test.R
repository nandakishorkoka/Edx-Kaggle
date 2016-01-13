NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk) 
NewsTest$SectionName = as.factor(NewsTest$SectionName) 
NewsTest$SubsectionName = as.factor(NewsTest$SubsectionName) 

NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

NewsTest$Weekday = NewsTest$PubDate$wday
NewsTest$Monthday = NewsTest$PubDate$mday
NewsTest$Month = NewsTest$PubDate$mon
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
NewsTest$Month = as.factor(NewsTest$Month) 
NewsTest$Hour = as.factor(NewsTest$Hour) 
NewsTest$TimeOfDay = as.factor(NewsTest$TimeOfDay) 



require (tm) 


NewsTest$Abstract = tolower(NewsTest$A)
NewsTest$Abstract = gsub("[[:punct:]]", "", NewsTest$Abstract)


strcount <- function(x, pattern, split){ 
unlist(lapply(
    strsplit(x, split),
       function(z) na.omit(length(grep(pattern, z)))
   ))
}

load("text_features.Rdata") 

for (t in text_features) { 
test.txtdf2[,eval(t)] = strcount(NewsTest$Abstract, t, split= " ") 
} 

colnames(test.txtdf2) = paste("text", colnames(test.txtdf2), sep="_")


NewsTest2 = cbind(NewsTest , test.txtdf2) 

NewsTest2$Headline <- NULL 
NewsTest2$Snippet <- NULL 
NewsTest2$Abstract <- NULL 
NewsTest2$PubDate <- NULL 


NewsTest2[is.na(NewsTest2)] <- 0 


load("rf_model.Rdata")

pred_labels = predict(model, newdata=test, type="prob") 
