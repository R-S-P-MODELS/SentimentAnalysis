require(rtweet)
require(tm)
source("Information.R")
source("Analise_texto.R")
source("sentimentscript.R")
CriarToken<-function(){
tweet<-create_token(
app = appname,
consumer_key = key,
consumer_secret = secret,
access_token = access_token,
access_secret = access_secret
)
return(tweet)
}


Query<-function(String,Linguagem,Quantidade){
tweets <- search_tweets(q = String,
                               n = Quantidade,include_rts = FALSE)
#tweets<-tweets[which(tweets$lang==Linguagem),]
return(unique(tweets$text[tweets$lang==Linguagem]) )

#return(unique(tweets$text))
}


GenerateGraph<-function(FilteredQuery,Linguagem){
b<-Palavras(FilteredQuery,Linguagem)
return(texterCum(b,20,Linguagem) )

}

GenerateEmotionGraph<-function(FilteredQuery,Linguagem){
b<-Palavras(FilteredQuery,Linguagem)
return(texterCumulativeEmotions(b,20,Linguagem) )

}

