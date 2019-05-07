textar=function(linguagem){
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)



library(tidyr)

if(linguagem=="en")
  Lexicon=get_sentiments("bing")
else if(linguagem=="pt")
  Lexicon=read.csv("~/SentimentAnalysis/LexiconPortuguesPositivevsNegative.csv",header=TRUE)

jane_austen_sentiment <- tidy_books %>%
  inner_join(Lexicon) %>%
  count(book, index = linenumber %/% block, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
library(plotly)

p1=ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

ggplotly(p1)
}

texterCum=function(a,block=80,linguagem){
#library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)

tidy_books <-data.frame("Sentimento",a,1:length(a))
names(tidy_books)=c("book","word","linenumber")

library(tidyr)

if(linguagem=="en")
  Lexicon=get_sentiments("bing")
else if(linguagem=="pt")
  Lexicon=read.csv("~/SentimentAnalysis/LexiconPortuguesPositivevsNegative.csv",header=TRUE)

jane_austen_sentiment <- tidy_books %>%
  inner_join(Lexicon) %>%
  count(book, index = linenumber %/% block, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(plotly)
jane_austen_sentiment$sentiment=cumsum(jane_austen_sentiment$sentiment)
p1=ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

ggplotly(p1)
}

texterLocal=function(a,block=80,linguagem){
#library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)

tidy_books <-data.frame("Sentimento",a,1:length(a))
names(tidy_books)=c("book","word","linenumber")

library(tidyr)

if(linguagem=="en")
  Lexicon=get_sentiments("bing")
else if(linguagem=="pt")
  Lexicon=read.csv("~/SentimentAnalysis/LexiconPortuguesPositivevsNegative.csv",header=TRUE)

jane_austen_sentiment <- tidy_books %>%
  inner_join(Lexicon) %>%
  count(book, index = linenumber %/% block, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
library(plotly)
#jane_austen_sentiment$sentiment=cumsum(jane_austen_sentiment$sentiment)
p1=ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

ggplotly(p1)
}

textLocalComp=function(a,b,block=80,linguagem){
  #library(janeaustenr)
  library(dplyr)
  library(stringr)
  library(tidytext)
  
  tidy_books <-data.frame("Sentimento",a,1:length(a))
  names(tidy_books)=c("book","word","linenumber")
  
  library(tidyr)
  
  if(linguagem=="en")
    Lexicon=get_sentiments("bing")
  else if(linguagem=="pt")
    Lexicon=read.csv("~/SentimentAnalysis/LexiconPortuguesPositivevsNegative.csv",header=TRUE)
  
  jane_austen_sentiment <- tidy_books %>%
    inner_join(Lexicon) %>%
    count(book, index = linenumber %/% block, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  tidy_books <-data.frame("Sentimento2",b,1:length(b))
  names(tidy_books)=c("book","word","linenumber")
  
  library(tidyr)
  
  if(linguagem=="en")
    Lexicon=get_sentiments("bing")
  else if(linguagem=="pt")
    Lexicon=read.csv("~/SentimentAnalysis/LexiconPortuguesPositivevsNegative.csv",header=TRUE)
  
  jane_austen_sentiment <- tidy_books %>%
    inner_join(Lexicon) %>%
    count(book, index = linenumber %/% block, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  
  jane_austen_sentiment=rbind(jane_austen_sentiment,jane_austen_sentiment2)
  library(plotly)
  #jane_austen_sentiment$sentiment=cumsum(jane_austen_sentiment$sentiment)
  p1=ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
    geom_col(show.legend = FALSE) 
    
  
  ggplotly(p1)
}


textCumComp=function(a,b,block=80,linguagem){
  #library(janeaustenr)
  library(dplyr)
  library(stringr)
  library(tidytext)
  
  tidy_books <-data.frame("SentimentoFileOne",a,1:length(a))
  #tidy2<-data.frame("SentimentoFileTwo",a,1:length(a))
  names(tidy_books)=c("book","word","linenumber")
  #names(tidy2)=c("book","word","linenumber")
  #tidy_books=rbind(tidy_books,tidy2)
  #tidy_books$linenumber=1:nrow(tidy_books)
  
  library(tidyr)
  
  if(linguagem=="en")
    Lexicon=get_sentiments("bing")
  else if(linguagem=="pt")
    Lexicon=read.csv("~/SentimentAnalysis/LexiconPortuguesPositivevsNegative.csv",header=TRUE)
  
  jane_austen_sentiment <- tidy_books %>%
    inner_join(Lexicon) %>%
    count(book, index = linenumber %/% block, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  print("Passa pela primeira parte")
  #tidy_books <-data.frame("SentimentoFileOne",a,1:length(a))
  tidy_books<-data.frame("SentimentoFileTwo",b,1:length(b))
  names(tidy_books)=c("book","word","linenumber")
  #names(tidy2)=c("book","word","linenumber")
  #tidy_books=rbind(tidy_books,tidy2)
  #tidy_books$linenumber=1:nrow(tidy_books)
  
  library(tidyr)
  print("Vai calcular o segundo")
  if(linguagem=="en")
    Lexicon=get_sentiments("bing")
  else if(linguagem=="pt")
    Lexicon=read.csv("~/SentimentAnalysis/LexiconPortuguesPositivevsNegative.csv",header=TRUE)
  
  jane_austen_sentiment <- tidy_books %>%
    inner_join(Lexicon) %>%
    count(book, index = linenumber %/% block, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  print("calculou")
  print(jane_austen_sentiment)
  print(jane_austen_sentiment2)
  jane_austen_sentiment$sentiment=cumsum(jane_austen_sentiment$sentiment)
  jane_austen_sentiment2$sentiment=cumsum(jane_austen_sentiment$sentiment2)
  print(jane_austen_sentiment)
  jane_austen_sentiment=rbind(jane_austen_sentiment,jane_austen_sentiment2)
  print("bugo rbind")
  print(jane_austen_sentiment)
  p1=ggplot(jane_austen_sentiment, aes(index, sentiment, col = book)) +
    geom_line(show.legend = FALSE) 
    
  
  ggplotly(p1)
}

texterLocalEmotions=function(a,block=80,linguagem){
  #library(janeaustenr)
  library(dplyr)
  library(stringr)
  library(tidytext)
  
  tidy_books <-data.frame("Sentimento",a,1:length(a))
  names(tidy_books)=c("book","word","linenumber")
  
  library(tidyr)
  if(linguagem=="en")
    Lexicon=get_sentiments("nrc")
  else if(linguagem=="pt")
    Lexicon=read.csv("~/SentimentAnalysis/LexiconPortugues.csv",header=TRUE)
  jane_austen_sentiment <- tidy_books %>%
    inner_join(Lexicon) %>%
    count(book, index = linenumber %/% block, sentiment) %>%
    spread(sentiment, n, fill = 0) #%>%
    #mutate(sentiment = positive - negative)
  df1=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$anger,"anger")
  df2=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$anticipation,"anticipation")
  df3=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$disgust,"disgust")
  df4=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$fear,"fear")
  df5=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$joy,"joy")
  df6=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$sadness,"sadness")
  df7=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$surprise,"surprise")
  df8=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$trust,"trust")
  names(df1)=names(df2)=names(df3)=names(df4)=names(df5)=names(df6)=names(df7)=names(df8)=c('Index','Emotion','Name')
  df1=rbind(df1,df2,df3,df4,df5,df6,df7,df8)
  library(plotly)
  #jane_austen_sentiment$sentiment=cumsum(jane_austen_sentiment$sentiment)
  #p1=ggplot(jane_austen_sentiment, aes(x=index)) +
  #  geom_col(show.legend = FALSE,aes(y=anger,fill="anger")) + 
  # geom_col(show.legend = FALSE,aes(y=anticipation,fill="anticipation")) +
  #  geom_col(show.legend = FALSE,aes(y=disgust,fill="disgust")) +
  #  geom_col(show.legend = FALSE,aes(y=fear,fill="fear")) +
  #  geom_col(show.legend = FALSE,aes(y=joy,fill="joy")) +
  #  geom_col(show.legend = FALSE,aes(y=sadness,fill="sadness")) +
  #  geom_col(show.legend = FALSE,aes(y=surprise,fill="surprise")) +
   # geom_col(show.legend = FALSE,aes(y=trust,fill="trust")) 
    #geom_col(show.legend = FALSE,aes(y=positive,fill="positive")) +
    #geom_col(show.legend = FALSE,aes(y=negative,fill="negative")) 
    
  p1=ggplot(df1,aes(x=Index,y=Emotion,fill=Name)) + geom_col(show.legend = FALSE)
  ggplotly(p1)
}


texterCumulativeEmotions=function(a,block=80,linguagem){
  #library(janeaustenr)
  library(dplyr)
  library(stringr)
  library(tidytext)
  
  tidy_books <-data.frame("Sentimento",a,1:length(a))
  names(tidy_books)=c("book","word","linenumber")
  
  library(tidyr)
  if(linguagem=="en")
    Lexicon=get_sentiments("nrc")
  else if(linguagem=="pt")
    Lexicon=read.csv("~/SentimentAnalysis/LexiconPortugues.csv",header=TRUE)
  jane_austen_sentiment <- tidy_books %>%
    inner_join(Lexicon) %>%
    count(book, index = linenumber %/% block, sentiment) %>%
    spread(sentiment, n, fill = 0) #%>%
  #mutate(sentiment = positive - negative)
  jane_austen_sentiment$anger=cumsum(jane_austen_sentiment$anger)
  jane_austen_sentiment$anticipation=cumsum(jane_austen_sentiment$anticipation)
  jane_austen_sentiment$disgust=cumsum(jane_austen_sentiment$disgust)
  jane_austen_sentiment$fear=cumsum(jane_austen_sentiment$fear)
  jane_austen_sentiment$joy=cumsum(jane_austen_sentiment$joy)
  jane_austen_sentiment$sadness=cumsum(jane_austen_sentiment$sadness)
  jane_austen_sentiment$surprise=cumsum(jane_austen_sentiment$surprise)
  jane_austen_sentiment$trust=cumsum(jane_austen_sentiment$trust)
  
  df1=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$anger,"anger")
  df2=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$anticipation,"anticipation")
  df3=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$disgust,"disgust")
  df4=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$fear,"fear")
  df5=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$joy,"joy")
  df6=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$sadness,"sadness")
  df7=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$surprise,"surprise")
  df8=data.frame(jane_austen_sentiment$index,jane_austen_sentiment$trust,"trust")
  names(df1)=names(df2)=names(df3)=names(df4)=names(df5)=names(df6)=names(df7)=names(df8)=c('Index','Emotion','Name')
  df1=rbind(df1,df2,df3,df4,df5,df6,df7,df8)
  library(plotly)
  #jane_austen_sentiment$sentiment=cumsum(jane_austen_sentiment$sentiment)
  #p1=ggplot(jane_austen_sentiment, aes(x=index)) +
  #  geom_col(show.legend = FALSE,aes(y=anger,fill="anger")) + 
  # geom_col(show.legend = FALSE,aes(y=anticipation,fill="anticipation")) +
  #  geom_col(show.legend = FALSE,aes(y=disgust,fill="disgust")) +
  #  geom_col(show.legend = FALSE,aes(y=fear,fill="fear")) +
  #  geom_col(show.legend = FALSE,aes(y=joy,fill="joy")) +
  #  geom_col(show.legend = FALSE,aes(y=sadness,fill="sadness")) +
  #  geom_col(show.legend = FALSE,aes(y=surprise,fill="surprise")) +
  # geom_col(show.legend = FALSE,aes(y=trust,fill="trust")) 
  #geom_col(show.legend = FALSE,aes(y=positive,fill="positive")) +
  #geom_col(show.legend = FALSE,aes(y=negative,fill="negative")) 
  
  p1=ggplot(df1,aes(x=Index,y=Emotion,col=Name)) + geom_line(show.legend = FALSE)
  ggplotly(p1)
}
