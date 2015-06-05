#https://apps.twitter.com/app/new
#Manage apps: https://apps.twitter.com/
#Get Key and secret and tokens from her: https://apps.twitter.com
library(tm)
library(twitteR)
library(RCurl)
library(ROAuth)

#Enter your own key!
consumerKey<-
consumerSecret<-
accessToken<-
accessSecret<-

setup_twitter_oauth(consumerKey, consumerSecret,access_token=accessToken,
                    access_secret=accessSecret)

a<-getUser('statsinthewild')
a$statusesCount
a$followersCount
a$friendsCount
retweets(a$lastStatus$id)
retweeters(a$lastStatus$id)

a<-getUser('justinbieber')
a$statusesCount
a$followersCount
a$friendsCount



stuff<-searchTwitter("summer",n=50)
stuff

blm1<-searchTwitter("summer",until="2015-06-01",since="2015-05-31")
blm1

blm2<-searchTwitter("#blacklivesmatter",n=50,since="2014-06-01",until="2014-07-31")
blm2

stuffDF<-do.call(rbind,lapply(stuff,as.data.frame))
stuffDF$text<-gsub("[^A-z #]","",stuffDF$text)

myCorpus <- Corpus(VectorSource(stuffDF$text))
myCorpus<-tm_map(myCorpus,content_transformer(tolower))
myCorpus<-tm_map(myCorpus,PlainTextDocument)
myCorpus<-tm_map(myCorpus,removeWords,stopwords("english"))
myCorpus<-tm_map(myCorpus,removePunctuation)
myCorpus<-tm_map(myCorpus,removeNumbers)
myCorpus<-tm_map(myCorpus,stripWhitespace)

#Define my own function
removePunctuationTwitter<-function(x){
  out<-gsub('[!.,?]',"",x)
  out
}

myCorpus <- Corpus(VectorSource(stuffDF$text))
myCorpus<-tm_map(myCorpus, content_transformer(tolower))
myCorpus<-tm_map(myCorpus,PlainTextDocument)
myCorpus<-tm_map(myCorpus,removeWords,stopwords("english"))
#Everything the same, but now I am calling a new function that I defined
myCorpus<-tm_map(myCorpus,removePunctuationTwitter)
myCorpus<-tm_map(myCorpus,removeNumbers)
myCorpus<-tm_map(myCorpus,stripWhitespace)
#Important final step
myCorpus<-tm_map(myCorpus, PlainTextDocument)


#Get term frequency
sort(termFreq(PlainTextDocument(myCorpus)))
#Create a TermDocumentMatrix
twitterTDM<-TermDocumentMatrix(myCorpus)
#Find Associated words
findAssocs(twitterTDM,terms=c("summer"),corlimit=0.1)




#######################################################
#Scrap
#######################################################
for (i in 1:100){print(i)
                 greg[[i]]<-searchTwitter("summer",n=50)
}
doesit<-do.call(rbind,lapply(greg,function(x){do.call(rbind,lapply(x,as.data.frame))}))
dim(doesit[!duplicated(doesit$text),])
#remove all hash tags
nohash<-gsub("#.*","",greg)
nohash<-lapply(nohash,PlainTextDocument)
class(nohash)<-c("VCorpus","Corpus","list")
test<-TermDocumentMatrix(nohash)
test$dimnames$Docs<-names(presList)

greg2<-lapply(greg,function(x){strsplit(x,"[ .]")})
greg3<-lapply(greg2,function(x){x[[1]][grep("#.*",x[[1]])]})
greg3<-lapply(greg3,)

PlainTextDocument(greg3[[1]])
library(tm)
test<-TermDocumentMatrix(greg3)

