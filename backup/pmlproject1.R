library(caret)
library(rattle)
library(randomForest)
library(rpart)
library(rpart.plot)
library(RColorBrewer)


setwd("D:\\data\\rprojects\\pml")

#trainfile <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#testfile <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainfile <- "pml-training.csv"
testfile <- "pml-testing.csv"

train <- read.csv(trainfile, header = TRUE, sep = ",")
test <- read.csv(testfile, header = TRUE, sep = ",")

# Split the set up, 60% for training and 40% for testing -- by using the Training set download.  Save testing download for later.

t1 <- createDataPartition(y = train$classe, p = 0.60, list=FALSE)
TrainingSet <- train[t1, ]
TestingSet <- train[-t1, ]

# Remove first column of set
TrainingSet <- TrainingSet[c(-1)]

dim(TrainingSet)
dim(TestingSet)

# The following is a feature selection algorithm to remove variables that contain mostly NA's (75% or more, to be exact.)

trainingV3 <- TrainingSet #creating another subset to iterate in loop
for(i in 1:length(TrainingSet)) { #for every column in the training dataset
     if( sum( is.na( TrainingSet[, i] ) ) /nrow(TrainingSet) >= .75 ) { #if n?? NAs > 75% of total observations
          for(j in 1:length(trainingV3)) {
               if( length( grep(names(TrainingSet[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
                    trainingV3 <- trainingV3[ , -j] #Remove that column
               }   
          } 
     }
}

dim(trainingV3)

theSubset <- trainingV3[,8:length(trainingV3)]
NZV <- nearZeroVar(theSubset, saveMetrics = TRUE)
NZV
keep <- names(theSubset)








