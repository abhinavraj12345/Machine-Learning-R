
#------------------------------sentiment analysis------------------------
library(ggplot2)
library(tm)           #for text mining
library(wordcloud)
library(syuzhet)      #for sentiment
library(SnowballC)
plot.new()
#------------------------------------------------------------------------
text<-readLines("C:\\Users\\Abhinav Raj\\Downloads\\WhatsAppChat.txt")
docs<-Corpus(VectorSource(text))
trans<-content_transformer (function(x,pattern)gsub(pattern,"",x))
docs<-tm_map(docs,trans,"/")
docs<-tm_map(docs,trans,"@")
docs<-tm_map(docs,trans,"\\!")
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removeWords,stopwords("english"))
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,stemDocument)
dtm<-TermDocumentMatrix(docs)
mat<-as.matrix(dtm)
v<-sort(rowSums(mat),decreasing = TRUE)
head(v)
d<-data.frame(word=names(v),freq=v)
head(d,10)
set.seed(1056)
wordcloud(words=d$word,freq=d$freq,min.freq = 1,max.words = 200,random.order = FALSE,root.per=0.35,
          colors=brewer.pal(8,"Dark2"))
sentiment<-get_nrc_sentiment(text)
head(sentiment)
text<-cbind(text,sentiment)
