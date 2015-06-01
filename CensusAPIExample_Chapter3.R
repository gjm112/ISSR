#http://www.census.gov/developers/data/sf1.xml
#http://api.census.gov/data.html
#Census API
key<-"e818359af387a7b903f44810d34e9d7a3c7c07f7"
get<-"P0010001"
url<-paste("http://api.census.gov/data/2010/sf1?key=",key,"&get=",get,",NAME&for=state:*",sep="")
aaa<-readLines(url)
aaaList<-strsplit(aaa,',')
dat<-do.call(rbind,aaaList)
dat<-gsub('"','',dat)
dat<-gsub('[]]','',dat)
dat<-gsub('[[]','',dat)

get<-"P0010001"
url<-paste("http://api.census.gov/data/2010/sf1?key=",key,"&get=",get,",NAME&for=place:*&in=state:25",sep="")
aaa<-readLines(url)
do.call(rbind,strsplit(aaa,'\",\"'))

http://api.census.gov/data/2010/sf1?key=
  e818359af387a7b903f44810d34e9d7a3c7c07f7&get=P0010001,NAME&for=state:*