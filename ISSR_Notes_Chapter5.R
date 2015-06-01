#http://journal.r-project.org/archive/2013-1/collingwood-jurka-boydstun-etal.pdf
library(RTextTools)
data(USCongress)
help(USCongress)
summary(USCongress)
#Create a document matrix
doc_matrix <- create_matrix(USCongress$text, language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

#create a container
container <- create_container(doc_matrix, USCongress$major, trainSize=1:4000,
                              testSize=4001:4449, virgin=FALSE)

#Build the training data set. Previously Run
#SVM <- train_model(container,"SVM")
#save(SVM,file="/Users/gregorymatthews/Dropbox/ISSRshortCourse/SVM.RData")
load("/Users/gregorymatthews/Dropbox/ISSRshortCourse/SVM.RData")
str(SVM)
#Classify using the model we just built
SVM_CLASSIFY <- classify_model(container, SVM)
#Let's look at analytics
analytics <- create_analytics(container,cbind(SVM_CLASSIFY))
summary(analytics)
#Precision 
#Recall 
#Fscore


#GLMNET Previously run.
#GLMNET <- train_model(container,"GLMNET")
#save(GLMNET,file="/Users/gregorymatthews/Dropbox/ISSRshortCourse/GLMNET.RData")
load("/Users/gregorymatthews/Dropbox/ISSRshortCourse/GLMNET.RData")
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
analytics <- create_analytics(container,cbind(SVM_CLASSIFY,GLMNET_CLASSIFY))
summary(analytics)

create_ensembleSummary(analytics@document_summary)


#4 fold cross validation
cross_validate(container, 4, "SVM")
cross_validate(container, 4, "GLMNET")



#More interesting data
data(NYTimes)