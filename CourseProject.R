# R Script
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(nnet)

#Set Directories
ProjectDir <- "c:\\users\\kadriu\\documents\\GitHub\\Coursera-Practical-Machine-Learning"
SubDir <- "Data"
setwd(ProjectDir)

if (!file.exists(SubDir)) {
  dir.create(file.path(SubDir))
}
setwd(file.path(ProjectDir, SubDir))


#Download the Data
TrainLink <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
TestLink <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
TrainFile <- "pml-training.csv"
TestFile <- "pml-testing.csv"
if (!file.exists(TrainFile))
  download.file(TrainLink, destfile = TrainFile, method = "curl")
if (!file.exists(TestFile))
  download.file(TestLink, destfile = TestFile, method = "curl")

#Read Data and Load into Data Frames
RawTrainData <- read.csv(TrainFile)
RawTestData <- read.csv(TestFile)

#Raw Training Data Summary
#The 'classe' variable in the dataset is the variable to predict

#Number of Variables
dim(RawTrainData) #19622 160
str(RawTrainData)

#Raw Test Data Summary
#Number of Rows
dim(RawTestData) #20 160
str(RawTestData)

#check the rows of data which has complete cases
#Training Dataset
sum(complete.cases(RawTrainData)) #406 Very small part of the training data has complete data

#Test Dataset
sum(complete.cases(RawTestData)) #0 None of the test data has complete data

#Data Cleaning and Preperation
#We will remove the NAs and irrelevant variables

#In the training set check columns with total NA values greater then 10% of the rows (more than 2000 NAs)
RawTrainDataNZero <- RawTrainData[, colSums(is.na(RawTrainData)) < 2000]
ncol(RawTrainDataNZero) #93

#93 variables have total number of NAs greater then 10% of the total rows of data

#Check the numbers of columns which has one or more NAs
RawTrainDataZero <- RawTrainData[, colSums(is.na(RawTrainData)) == 0]
ncol(RawTrainDataZero) #93
#Again 93 columns have no NAs, no need for further detailed processing such as imputing
#Just remove the columns with NAs
RawTrainData <- RawTrainData[, colSums(is.na(RawTrainData)) == 0]

#In the testing set remove columns with one or more NA in the training dataset
RawTestData <- RawTestData[, colSums(is.na(RawTestData)) == 0]

#Further to this, we need to remove the unnecessary columns that do not contribute to the results
#These are 
# $ X                      : int  1 2 3 4 5 6 7 8 9 10 ...
# $user_name:Factor w / 6 levels "adelmo", "carlitos", ..:2 2 2 2 2 2 2 2 2 2 ...
# $raw_timestamp_part_1:int 1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
# $raw_timestamp_part_2:int 788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
# $cvtd_timestamp:Factor w / 20 levels "02/12/2011 13:32", ..:9 9 9 9 9 9 9 9 9 9 ...
# $new_window:Factor w / 2 levels "no", "yes":1 1 1 1 1 1 1 1 1 1 ...
#$ num_window:int 11 11 11 12 12 12 12 12 12 12 ...
TrainColsToRemove <- grepl("^X|user_name|timestamp|window", names(RawTrainData))
RawTrainData <- RawTrainData[, !TrainColsToRemove]
TestColsToRemove <- grepl("^X|user_name|timestamp|window", names(RawTestData))
RawTestData <- RawTestData[, !TestColsToRemove]

#check the rows of data which has complete cases again
#Training Dataset
sum(complete.cases(RawTrainData)) #19622 We do now have complete cases for all training data
#Test Dataset
sum(complete.cases(RawTestData)) #20 We do now have complete cases for all test data

set.seed(562389) #for reproducible results

#We further the partition the training dataset for training and validataion purposes.
#We would like to the validate the model with a subset of the data before applying the test data
#This is to avoid overfitting
#Generate a training and validation dataset
TrainIdx <- createDataPartition(RawTrainData$classe, p = 0.7, list = FALSE)
TrainData <- RawTrainData[TrainIdx,]
TestData <- RawTrainData[ - TrainIdx,]

#Data Modelling
#We will test several algorithms and compare their accuracy levels
#First one is the Random Forest
modelRf <- randomForest(classe~., data = TrainData)
predRf<-predict(modelRf,TestData)
confusionMatrix(TestData$classe, predRf)

#Second we test Artificial Neural Network
#A simple single layer network with no hidden perceptrons
modelNn <- nnet(classe~., data = TrainData, size=15)
predNn<-predict(modelNn,TestData, TYPE="class")
#Output of predictors for Artifical Neural Networks is different
#Rather then giving the highhest probability prediction
#It gives the probability for each prediction
#We select the one with the highest
prdNN<-c("A","B","C","D","E")[apply(predNn,1,which.max)]
table(prd,TestData$classe)
confusionMatrix(TestData$classe, prdNN)

#Third we test support vector algorithm
library(e1071)
modelSvm <- svm(classe~., data = TrainData)
predSvm<-predict(modelSvm,TestData)
confusionMatrix(TestData$classe, predSvm)

#Fourth we would like to test Linear Discriminant Analysis for its simplicity
library(MASS)
modelLda <- lda(classe~., data = TrainData)
predLda<-predict(modelLda,TestData)
confusionMatrix(TestData$classe, predLda$class)

#Fifth and last one if the regression trees
#Which are useful for their human interpretable results
#Usually used in healthcare related decisions
modelCart <- rpart(classe~., data = TrainData)
predCart<-predict(modelCart,TestData)
#Output of predictors for Classification and Regression Tree is different
#Rather then giving the highhest probability prediction
#It gives the probability for each prediction
#We select the one with the highest
prdCART<-c("A","B","C","D","E")[apply(predCart,1,which.max)]
confusionMatrix(TestData$classe, prdCART)

#Results of the comparision of accuracy of the different algorithms
OutputTable <- c("Random Forest",as.numeric(confusionMatrix(TestData$classe, predRf)$overall["Accuracy"]))
temp<- rbind(OutputTable, c("Artificial Neural Network",as.numeric(confusionMatrix(TestData$classe, prdNN)$overall["Accuracy"])))
temp<- rbind(temp, c("Support Vector",as.numeric(confusionMatrix(TestData$classe, predSvm)$overall["Accuracy"])))
temp<- rbind(temp, c("Linear Discriminant Analysis",as.numeric(confusionMatrix(TestData$classe, predLda$class)$overall["Accuracy"])))
temp<- rbind(temp, c("Classification and Regression Tree",as.numeric(confusionMatrix(TestData$classe, prdCART)$overall["Accuracy"])))
OutputTable <- temp
colnames(OutputTable) <- c("Algorithm", "Accuracy")
OutputTable

#By far random forest is the most acurate one
#We need to test the accuracy with the validation dataset
#Test for overfitting
predRf<-predict(modelRf,RawTestData)

#Correlation Plot between different attributes of the model
#The graph is not readable but gives an idea on the 
#importance of some of the model
corrPlot <- cor(TrainData[, -length(names(TrainData))])
corrplot(corrPlot, method="color")

#An additional tree visualization would reveal the
#Decision critieria
#Especialy important to be analyzed by a domain expert
#Sometime data driven models could bring some issues
treeModel <- rpart(classe ~ ., data=TrainData, method="class")
prp(treeModel)
