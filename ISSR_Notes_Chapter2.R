

#Used to be able to do this:
#titanic<-read.csv("http://bit.ly/1KSjVqg")
#Now you have to do this:
library(repmis)
titanic<-source_data("http://bit.ly/1KSjVqg")
class(titanic)
titanic[1:5,c(2,3,5)]


library(XML)
#Let's go look at this page
url<-"http://www.baseball-reference.com/teams/BOS/2014.shtml"
#Convert the web page tables into R data.frames
redSoxTable<-readHTMLTable(url)
class(redSoxTable)
names(redSoxTable)
class(redSoxTable$team_batting)


#Let's look in 
redSoxTable$team_batting[1:3,1:8]

dat<-redSoxTable$team_batting

dat$BA<-as.character(dat$BA)
dat$BA[dat$BA=="BA"]<-""
dat$BA<-as.numeric(dat$BA)
dat[order(-dat$BA),]
save.image("/Users/gregorymatthews/Dropbox/ISSR.RData")
load("/Users/gregorymatthews/Dropbox/ISSR.RData")
save(dat,file="/Users/gregorymatthews/Dropbox/dat.RData")
write.csv(dat,"/Users/gregorymatthews/Dropbox/ISSRtest.csv")

#Use gsub to remove unwanted character
redSoxTable$team_batting[,3]<-gsub("[*#]","",redSoxTable$team_batting[,3])
#Unwanted characters are removed
redSoxTable$team_batting[1:3,1:8]



#To get just infielders, we use the function %in%
#Only pull out infielders
infInd <- redSoxTable$team_batting$Pos%in%c("1B","2B","3B","SS")
infielders <- redSoxTable$team_batting[infInd,]
infielders[,1:8]



teamsData<-readHTMLTable("http://www.baseball-reference.com/leagues/MLB/2014.shtml")
class(teamsData)
names(teamsData)
teamsData[["teams_standard_batting"]][1:3,1:5]
#Construct a vector of all the team abbreviations
teams<-as.character(teamsData[["teams_standard_batting"]]$Tm[1:30])
teams


#resList will store the results
resList<-list()
#We will loop over all teams
for (t in teams){print(t)
                 url<-paste("http://www.baseball-reference.com/teams/",t,"/2014.shtml",sep="")
                 teamData<-readHTMLTable(url)
                 #Pull out the team batting component                  
                 output<-teamData$team_batting
                 #Convert player name to a character
                 output[,3]<-as.character(output[,3])
                 output[,3]<-gsub("[*#]","",output[,3])
                 #Store the output
                 resList[[t]]<-output[output[,3]!="",]
}


class(resList)
names(resList)
resList$BOS[1:3,1:8]
resList$NYY[1:3,1:8]
resList[["BOS"]][1:3,1:8]

#Now I want to stack all of these data frames on top of each other
allTeams<-do.call(rbind,resList)
dim(allTeams)



####################################################################
#Presidential Speeches Example
####################################################################
library(RCurl)
library(XML)
library(tm)
presList<-list()
url<-"http://www.presidentialrhetoric.com/historicspeeches/bush/first_inaugural.html"
a<-getURL(url)
b<-htmlParse(url)
x<-readHTMLTable(b)
text<-levels(x[[6]]$V1)
text<-gsub("\n","",text)
text<-gsub("[,.]","",text)
presList[["HWBush"]]<-PlainTextDocument(text)

url<-"http://www.presidentialrhetoric.com/historicspeeches/clinton/first_inaugural.html"
a<-getURL(url)
b<-htmlParse(url)
x<-readHTMLTable(b)
text<-levels(x[[6]]$V1)
text<-gsub("\n","",text)
text<-gsub("[,.]","",text)
presList[["Clinton"]]<-PlainTextDocument(text)




url<-"http://www.presidentialrhetoric.com/historicspeeches/bush_georgew/second_inaugural.html"
a<-getURL(url)
b<-htmlParse(url)
x<-readHTMLTable(b)
text<-levels(x[[6]]$V1)
text<-gsub("\n","",text)
text<-gsub("[,.]","",text)
presList[["Bush"]]<-PlainTextDocument(text)
namesVec<-names(presList)

presList<-Corpus(VectorSource(presList))
names(presList)<-namesVec

summary(presList)
inspect(presList[1])

presList<-tm_map(presList,stripWhitespace)
stopwords("english")
presList<-tm_map(presList, removeWords, stopwords("english"))
presList <- tm_map(presList, content_transformer(tolower))
presList<-tm_map(presList, stemDocument) #requires SnowballC package 

class(presList)
names(presList)

ctrl<-list(removePunctuation = list(preserve_intra_word_dashes = TRUE),
           stopwords=c("bush","Bush"),
           stemming=TRUE,
           wordLengths=c(4,Inf))
#Word frequency vectors. 
sort(termFreq(presList[["HWBush"]],control=ctrl))
sort(termFreq(presList[["Clinton"]],control=ctrl))
sort(termFreq(presList[["Bush"]],control=ctrl))



library(proxy)
#Slot 6 contains the text. 
presTDM<-TermDocumentMatrix(presList)
#dissimilarity(presTDM,method="eJaccard")
findAssocs(presTDM,c("will","vision"),corlimit=c(0.9,0.9))
findFreqTerms(presTDM,20,30)
removeSparseTerms(presTDM,0.5)
#1 means it has to be in all documents
#0 means we keep all words
#0.5 means we keep words that appear in at least half of the documents.  

#Measure distance between documents
library(proxy)
test<-proxy::dist(as.matrix(t(presTDM)), method = "eJaccard")

#Need the proxy packages
library(proxy)
data("crude")
tdm <- TermDocumentMatrix(crude)
test<-proxy::dist(as.matrix(t(tdm)), method = "eJaccard")




############################
#Start here for ALL speeches:
############################

#Now get them all
doc<-htmlParse(getURL("http://www.presidentialrhetoric.com/historicspeeches/index.html"))
vec<-xpathSApply(doc, "//a/@href")
vecList<-vec[grep("inaugural",vec)]

presList<-list()
for (vvv in vecList){print(vvv)
                     url<-paste("http://www.presidentialrhetoric.com/historicspeeches/",vvv,sep="")
                     a<-getURL(url)
                     b<-htmlParse(url)
                     x<-readHTMLTable(b)
                     text<-levels(x[[6]]$V1)
                     text<-gsub("\n","",text)
                     text<-gsub("[,.]","",text)
                     presList[[vvv]]<-PlainTextDocument(text)
}
namesVec<-names(presList)
presCorpus <- Corpus(VectorSource(presList))
names(presCorpus)<-namesVec
#presCorpus<-tm_map(presCorpus,tolower)
presCorpus <- tm_map(presCorpus, content_transformer(tolower))
presCorpus<-tm_map(presCorpus,removeWords,stopwords("english"))
presCorpus<-tm_map(presCorpus,removePunctuation)
presCorpus<-tm_map(presCorpus,removeNumbers)
presCorpus<-tm_map(presCorpus,stripWhitespace)
presTDM<-TermDocumentMatrix(presCorpus)
presTDM$dimnames$Docs<-substring(vecList,1,5)
#save(presTDM,file="/Users/gregorymatthews/Dropbox/presTDM.RData")



findFreqTerms(presTDM,300)
findAssocs(presTDM,"government",0.7)



#Testing between groups
m <- as.matrix(t(presTDM))
rownames(m)<-presTDM$dimnames$Docs
group<-factor(c(rep(1,31),rep(2,24)))
colnames(m)[1]

t.test(m[,1]~group)$p.value

goat<-function(x){
  out<-t.test(x~group)$p.value
  out
}
results<-apply(m,2,goat)
interesting<-data.frame(word=colnames(m),pval=results)
((interesting[order(interesting$pval),])[1:100,])[-c(1,4,10),]
t.test((m[,colnames(m)=="duties"])~group)
t.test((m[,colnames(m)=="world"])~group)
t.test((m[,colnames(m)=="public"])~group)
t.test((m[,colnames(m)=="god"])~group)
t.test((m[,colnames(m)=="poverty"])~group)

m[,colnames(m)=="work"]

temp<-strsplit(presList[[15]]," ")
cbind(temp[[1]][which(temp[[1]]=="duty")+1],
      temp[[1]][which(temp[[1]]=="duty")+2],
      temp[[1]][which(temp[[1]]=="duty")+3])
thing<-cbind(temp[[1]][which(temp[[1]]=="duty")-3],
             temp[[1]][which(temp[[1]]=="duty")-2],
             temp[[1]][which(temp[[1]]=="duty")-1])




