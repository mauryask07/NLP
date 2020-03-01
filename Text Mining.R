library(ggplot2)## Graphical Visualization
library(caret)  ##  Partion of Data
library(tm)     ##  Text Mining
library(wordcloud) ## Generating Word Cloud
library(SnowballC) ## Word Stemming



## Import The Data (csv file)
df<-read.csv(file.choose(),header = T,stringsAsFactors = F)

## Select only important variable (here, "TRANS_CONV_TEXT"  and "Patient_Tag")
movie_review <- movie_review[,8:9]

## Change the Variable Name (for simplicity)
colnames(movie_review) <- c("Text", "Type")
dim(movie_review)
str(movie_review)
movie_review$Type<-factor(movie_review$Type)
summary(movie_review)
prop.table(table(movie_review$Type))
set.seed(100)
inTrain<-createDataPartition(y=movie_review$Type,p=0.5,list = FALSE)
train_m<-movie_review[inTrain,]
testdata<-movie_review[-inTrain,]


inTest<-createDataPartition(y=testdata$Type,p=0.5,list = FALSE)
test1_m<-testdata[inTest,]
test2_m<-testdata[-inTest,]



nrow(train_m)
nrow(test1_m)
nrow(test2_m)


prop.table(table(train_m$Type))
prop.table(table(test1_m$Type))
prop.table(table(test2_m$Type))

train_corpus_m<-VCorpus(VectorSource(train_m$Text))
length(train_corpus_m)

train_m$Text[1]

train_dtm_m<-DocumentTermMatrix(train_corpus_m,control = list(removeNumbers=T,removePunctuation=T,stripWhitespace=T,tolower=T,stopwords=T,stemming=T))
dim(train_dtm_m)




train_rmspa_m<-removeSparseTerms(train_dtm_m,0.80)
dim(train_rmspa_m)
mean_train=sort(colMeans(as.matrix(train_rmspa_m)),decreasing = T)
mean_train[1:20]
average_top20=mean(mean_train[1:20])
average_top20
barplot(mean_train[1:20],border = NA, las =3, xlab = "top 20 words", ylab="Frequency",ylim = c(0,3))

train_rmspa_m_nozero<-as.matrix(train_rmspa_m)
is.na(train_rmspa_m_nozero)<-train_rmspa_m_nozero==0
mean_train_m<-sort(colMeans(train_rmspa_m_nozero,na.rm = TRUE),decreasing = T)
mean_train_m[1:20]


train_Bowfreq<-as.matrix(train_rmspa_m)

train_data_m<-data.frame(y=train_m$Type,x=train_Bowfreq)
summary(train_data_m)
str(train_data_m)

train_BoW_m=findFreqTerms(train_rmspa_m)
length(train_BoW_m)


test1_corpus_m<-VCorpus(VectorSource(as.matrix(test1_m$Text)))

Bow_test1_m<-DocumentTermMatrix(test1_corpus_m,control = list(tolower = T, removeNumbers=T,removePunctuation=T,stopwords=T,stripWhitespace=T,stemming=T,dictionary=train_BoW_m))

str(Bow_test1_m)
dim(Bow_test1_m)

test1_Bowfreq_m<-as.matrix(Bow_test1_m)

test1_data_m<-data.frame(y=test1_m$Type,x=test1_Bowfreq_m)

str(test1_data_m)













library(party)

Bow_ctree_m<-ctree(y~.,data=train_data_m)
summary(Bow_ctree_m)
plot(Bow_ctree_m,type="simple")

test1Pred = predict(Bow_ctree_m,newdata=test1_data_m)
confusionMatrix(test1Pred,test1_data_m[,1],positive = "1",dnn=c("Prediction","True"))

library(rminer)
mmetric(test1Pred,test1_data_m[,1],c("ACC","TPR","PRECISION","F1"))


summary(test1Pred)




library(e1071)

Bow_nb_m<-naiveBayes(y~.,data=train_data_m)
summary(Bow_nb_m)


test1Pred = predict(Bow_ctree_m,newdata=test1_data_m)
confusionMatrix(test1Pred,test1_data_m[,1],positive = "1",dnn=c("Prediction","True"))

library(rminer)
mmetric(test1Pred,test1_data_m[,1],c("ACC","TPR","PRECISION","F1"))



library(kernlab)



Bow_ksvm_m<-ctree(y~.,data=train_data_m)


test1Pred = predict(Bow_ksvm_m,newdata=test1_data_m)
confusionMatrix(test1Pred,test1_data_m[,1],positive = "1",dnn=c("Prediction","True"))

library(rminer)
mmetric(test1Pred,test1_data_m[,1],c("ACC","TPR","PRECISION","F1"))









test2_corpus_m<-VCorpus(VectorSource(as.matrix(test2_m$Text)))

Bow_test2_m<-DocumentTermMatrix(test2_corpus_m,control = list(tolower = T, removeNumbers=T,removePunctuation=T,stopwords=T,stripWhitespace=T,stemming=T,dictionary=train_BoW_m))

str(Bow_test2_m)
dim(Bow_test2_m)

test2_Bowfreq_m<-as.matrix(Bow_test2_m)

test2_data_m<-data.frame(y=test2_m$Type,x=test2_Bowfreq_m)

str(test2_data_m)



library(party)

Bow_ctree_m<-ctree(y~.,data=train_data_m)
summary(Bow_ctree_m)
plot(Bow_ctree_m,type="simple")

test2Pred = predict(Bow_ctree_m,newdata=test2_data_m)
confusionMatrix(test2Pred,test2_data_m[,1],positive = "1",dnn=c("Prediction","True"))

library(rminer)
mmetric(test2Pred,test2_data_m[,1],c("ACC","TPR","PRECISION","F1"))

library(e1071)

Bow_nb_m<-naiveBayes(y~.,data=train_data_m)
summary(Bow_nb_m)


test1Pred = predict(Bow_ctree_m,newdata=test1_data_m)
confusionMatrix(test1Pred,test1_data_m[,1],positive = "1",dnn=c("Prediction","True"))

library(rminer)
mmetric(test1Pred,test1_data_m[,1],c("ACC","TPR","PRECISION","F1"))








