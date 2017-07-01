# This file includes the outcome of the Practical-Machine-Learning-Assignment
Practice Machine Learning Peer-Graded Assignment
Ella

June 29, 2017

Overview
This is an R Markdown document for Practice Machine Learning Peer-Graded Assignment. The goal of this project is to predict the manner in which participants did the exercise.

Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants to predict the manner in which they did the exercise.

Data set
All 6 participants were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

The training data for this project are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.

Preparation
rm(list=ls())    # free up memory for the download of the data sets
setwd("C:/Users/selin2/Documents/Data Science/Machine Learning/PMLAssignment")
library(caret)
#### Warning: package 'caret' was built under R version 3.3.3
#### Loading required package: lattice
#### Loading required package: ggplot2
#### Warning: package 'ggplot2' was built under R version 3.3.3
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training <- read.csv(url(trainUrl))
testing  <- read.csv(url(testUrl))
Exploring Data
dim(training)
#### [1] 19622   160
table(training$classe)
#### 
####    A    B    C    D    E 
#### 5580 3797 3422 3216 3607
dim(testing)
#### [1]  20 160
There are 19622 observations and 160 variables in the training data set with Classe A has the most observations. The testing data set inclused 20 observations and 160 variables.

Cleaning Up Data
Remove NA columns

training <- training[,!sapply(training,function(x) any(is.na(x)))]
dim(training)
#### [1] 19622    93
Remove the first 7 columns that are not related to the classe variable

training <- training[,-c(1:7)]
dim(training)
#### [1] 19622    86
Remove the Near Zero variance variables

training <- training[,-nearZeroVar(training)]
dim(training)
#### [1] 19622    53
Cross Validation – Building Prediction Models
#### set seed to 33833
set.seed(333)
#prepare training scheme
control <- trainControl(method="cv", number=2, allowParallel = TRUE)
Train with the Random Forest model

modrf = train(classe~.,data=training,method="rf",trControl=control)
#### Loading required package: randomForest
#### Warning: package 'randomForest' was built under R version 3.3.3
#### randomForest 4.6-12
#### Type rfNews() to see new features/changes/bug fixes.
#### 
#### Attaching package: 'randomForest'
#### The following object is masked from 'package:ggplot2':
#### 
####     margin
predrf <- predict(modrf,training)
confusionMatrix(predrf,training$classe)$overall[1]
#### Accuracy 
####        1
table(predrf,training$classe)
####       
#### predrf    A    B    C    D    E
####      A 5580    0    0    0    0
####      B    0 3797    0    0    0
####      C    0    0 3422    0    0
####      D    0    0    0 3216    0
####      E    0    0    0    0 3607
Train with the Generalized Boosted Regression Model

modgbm <- train(classe~.,data=training,method="gbm",trControl=control,verbose = FALSE)
#### Loading required package: gbm
#### Warning: package 'gbm' was built under R version 3.3.3
#### Loading required package: survival
#### Warning: package 'survival' was built under R version 3.3.3
#### 
#### Attaching package: 'survival'
#### The following object is masked from 'package:caret':
#### 
####     cluster
#### Loading required package: splines
#### Loading required package: parallel
#### Loaded gbm 2.1.3
#### Loading required package: plyr
#### Warning: package 'plyr' was built under R version 3.3.3
predgbm <- predict(modgbm,training)
confusionMatrix(predgbm,training$classe)$overall[1]
####  Accuracy 
#### 0.9722251
table(predgbm,training$classe)
####        
#### predgbm    A    B    C    D    E
####       A 5525   90    0    2    4
####       B   33 3628   73   11   23
####       C   13   75 3307   85   24
####       D    7    2   36 3100   39
####       E    2    2    6   18 3517
Train with the Linear Discriminant Analysis (lda) Model

modlda <- train(classe~.,data=training,method="lda",trControl=control)
#### Loading required package: MASS
predlda <- predict(modlda,training)
confusionMatrix(predlda,training$classe)$overall[1]
####  Accuracy 
#### 0.7048211
table(predlda,training$classe)
##        
## predlda    A    B    C    D    E
##       A 4568  586  341  191  133
##       B  121 2429  333  130  611
##       C  444  455 2254  379  323
##       D  429  148  411 2383  344
##       E   18  179   83  133 2196
Predict the Test Data
Since the Random Forest model has the highest accuracy 1 than the GBM model 0.9722 or the LDA model 0.7048211, the Random Forest model will be used to predict the test data

predT = predict(modrf,testing)
predT
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
