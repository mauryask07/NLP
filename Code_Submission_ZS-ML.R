library(caret)
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(rminer)
library(party)


#//////////// Import The DataSet (train.csv) ////////////########
patient_conversations<-read.csv(file.choose(),header = T,stringsAsFactors = F)

# Select only important variable (here, "TRANS_CONV_TEXT"  and "Patient_Tag") 
patient_conversations <- patient_conversations[,8:9]

# Change the Variable Name (for simplicity)
colnames(patient_conversations) <- c("Text", "Pat_Tag")

# Check the Dimension of the dataset
dim(patient_conversations)

# Change Patient_Tag type into factor
patient_conversations$Pat_Tag<-factor(patient_conversations$Pat_Tag)

## Structure of the DataSet
str(patient_conversations)

# Check the number of patients 
table(patient_conversations$Pat_Tag)


# Express Table Entries as Fraction of Marginal Table
prop.table(table(patient_conversations$Pat_Tag))



#//////// Prepare training data and testing data //////###


# split the dataset into a training and Validation datasets. We will use 80% of the data for training and the remaining 20% for validation.
inTrain<-createDataPartition(y=patient_conversations$Pat_Tag,p=0.8,list = FALSE)
train_m<-patient_conversations[inTrain,]
testdata<-patient_conversations[-inTrain,]

# Dimension of the training data and testing data 
dim(train_m)
dim(testdata)

# devide the validation dataset into test1_m and test2_m for split validation.
inTest<-createDataPartition(y=testdata$Pat_Tag,p=0.5,list = FALSE)
test1_m<-testdata[inTest,]
test2_m<-testdata[-inTest,]

# Dimension of the test1 data and test2 data 
dim(test1_m)
dim(test2_m)


# Check the partion results (if it is randomly partitioning)
# for Train_m Data set
prop.table(table(train_m$Pat_Tag))

# for test1_m data set
prop.table(table(test1_m$Pat_Tag))

# for test2_m data set
prop.table(table(test2_m$Pat_Tag))

#######//////////   Tokenization   ///////##########
## A corpus is a collection of text documents. We will use the Vcorpus() function to create a volatile corpus.
## The VectorSource() function will create one document for each sms text message. These will then be integrated into the corpus.

library(tm)
library(NLP)

train_corpus_clean<-VCorpus(VectorSource(train_m$Text))
length(train_corpus_clean)



#############////////// Word Cloud ////////############




toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(train_corpus_clean, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

                             
# Transform the Corpus to Document Term Matrix                             
dtm<- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)


#Word Cloud for whole dataset
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
                              

## Transform the Corpus to Document Term Matrix
DTM_train<-DocumentTermMatrix(train_corpus_clean,control = list(removeNumbers=T,removePunctuation=T,stripWhitespace=T,tolower=T,stopwords=T,stemming=T))
dim(DTM_train)
 

#Remove Sparse Terms from a Term-Document Matrix
train_rmspa_m<-removeSparseTerms(DTM_train,0.80)
dim(train_rmspa_m)

#show the average frequency of the top 20 most frequent words
mean_train=sort(colMeans(as.matrix(train_rmspa_m)),decreasing = T)
mean_train[1:20]

#Show the frequency in the Bar Plot
average_top20=mean(mean_train[1:20])
average_top20

#Show the Frequency in barplot
barplot(mean_train[1:20], las =3, xlab = "Top 20 words", ylab="Frequency",ylim = c(0,3))

#compare the average frequency if zeros are not counted in averaging
train_rmspa_m_nozero<-as.matrix(train_rmspa_m)
is.na(train_rmspa_m_nozero)<-train_rmspa_m_nozero==0

#calculate the mean without taking na
mean_train_m<-sort(colMeans(train_rmspa_m_nozero,na.rm = TRUE),decreasing = T)

# show the 20 words with highest frequency despites the na
mean_train_m[1:20]

#transform the formate from document term matrix to Matrix
train_Bow_freq<-as.matrix(train_rmspa_m)

#generate the training dataset 
train_data_m<-data.frame(y=train_m$Pat_Tag,x=train_Bow_freq)

#dimension of the training data set
dim(train_data_m)

#We will save the Bag of words generated from the training set for the later use
# Create vector of most frequent words
train_BoW_m=findFreqTerms(train_rmspa_m)

#Create a corpora of text documents in test1
test1_corpus_clean<-VCorpus(VectorSource(as.matrix(test1_m$Text)))

#Generate test1's Document Text Matrix based on the Bag of words decided by the training data
DTM_test1_m<-DocumentTermMatrix(test1_corpus_clean,control = list(tolower = T, removeNumbers=T,removePunctuation=T,stopwords=T,stripWhitespace=T,stemming=T,dictionary=train_BoW_m))
dim(DTM_test1_m)

#Transform the formate from document term matrix to Matrix of validation data
test1_Bow_freq_m<-as.matrix(DTM_test1_m)

#Generate the validation dataset 
test1_data_m<-data.frame(y=test1_m$Pat_Tag,x=test1_Bow_freq_m)



#####//////////// Decision Tree/////////############


#Use ctree() function from the party package to train our classifier for test1_data_m
library(party)

Pat_Conv_Classifier_tree1<-ctree(y~.,data=train_data_m)

#We will now test the performance of our patient conversation classifier in our validation dataset. In order to do so we will use the predict() function. 
#We can then compare the predictions from the actual class by creating a confusion matrix using the confusionMatrix() function from the caret package.

test1Pred = predict(Pat_Conv_Classifier_tree1,newdata=test1_data_m)
confusionMatrix(data = test1Pred, reference = test1_data_m[,1], positive = "1", dnn = c("Prediction", "Actual"))
library(rminer)
mmetric(test1Pred,test1_data_m[,1],c("ACC","TPR","PRECISION"))




#///////// Now Prepare the second validation dataset ///////######

# create a corpora of text documents in test1
test2_corpus_m<-VCorpus(VectorSource(as.matrix(test2_m$Text)))

# generate test1's Document Text Matrix based on the Bag of words decided by the training data
Bow_test2_m<-DocumentTermMatrix(test2_corpus_m,control = list(tolower = T, removeNumbers=T,removePunctuation=T,stopwords=T,stripWhitespace=T,stemming=T,dictionary=train_BoW_m))
test2_Bowfreq_m<-as.matrix(Bow_test2_m)
test2_data_m<-data.frame(y=test2_m$Pat_Tag,x=test2_Bowfreq_m)
dim(Bow_test2_m)


#####//////////// Decision Tree///////######

# use ctree() function from the party package to train our classifier for test2_data_m
library(party)
Pat_Conv_Classifier_tree2<-ctree(y~.,data=train_data_m)

#Test The Model for Validation dataset (test2_data_m) 
test2Pred = predict(Pat_Conv_Classifier_tree2,newdata=test2_data_m)
confusionMatrix(data = test2Pred, reference = test2_data_m[,1], positive = "1", dnn = c("Prediction", "Actual"))

library(rminer)
mmetric(test2Pred,test2_data_m[,1],c("ACC","TPR","PRECISION"))




######///// Prepare Test Data /////#######

test_data<-read.csv(file.choose())
test_data$Pat_Tag<-NA

test_data <- test_data[,c(9,11)]

## Change the Variable Name (for simplicity)
colnames(test_data) <- c("Text","Pat_Tag")

## Dimension of the DataSet
dim(test_data)

# Change Patient_Tag type into factor
test_data$Pat_Tag<-factor(test_data$Pat_Tag)

## Structure of the DataSet
str(test_data)

#To generate test1_data_m that include test1's term frequency of words selected from the train data
# create a corpora of review documents in test1
test_data_corpus_clean<-VCorpus(VectorSource(as.matrix(test_data$Text)))

#generate's test1's DOcument Text Matrix based on the Bag of words decided by the training data
DTM_test_data<-DocumentTermMatrix(test_data_corpus_clean,control = list(tolower = T, removeNumbers=T,removePunctuation=T,stopwords=T,stripWhitespace=T,stemming=T,dictionary=train_BoW_m))

dim(DTM_test_data)


test_data_Bow_freq<-as.matrix(DTM_test_data)
#generate the training dataset 
test_data_m<-data.frame(y=test_data$Pat_Tag,x=test_data_Bow_freq)

dim(test_data_m)



##########////////// Decision Tree//////////###########

# Predict the Test dataset by Decision Tree Algorithm
library(party)

Pat_Conv_Classifier_tree<-ctree(y~.,data=train_data_m)

Patient_Tag = predict(Pat_Conv_Classifier_tree,newdata=test_data_m)

pred.ctree<- data.frame(
  cbind(
    Index = 1:nrow(test_data_m), 
    as.data.frame(Patient_Tag)
  )
)



