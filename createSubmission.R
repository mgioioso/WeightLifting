# setwd("Documents/Personal/Data Science/Coursera Courses/Practical Machine Learning/")
library(caret)
library(doParallel)

training <- read.csv("pml-training.csv", na.strings=c("NA","","#DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings=c("NA","","#DIV/0!"))

#as.POSIXct(training$raw_timestamp_part_1, origin="1970-01-01")
# get rid of columns with just NAs
nas <- colSums(is.na(training))
naIds <- which(nas > 1900)

summaryRows <- which(training$new_window=="yes")

clean_train <- training[,-naIds]
clean_train <- clean_train[-summaryRows,]
clean_test <- testing[,-naIds]

# remove the metadata which will cause overfitting to nongeneralized features
# ie. username, time, etc
clean_train <- clean_train[,8:ncol(clean_train)]
clean_test <- clean_test[,8:ncol(clean_test)]

# look at correlation of the different columns to see if we can remove some more
corMat <- cor(clean_train[,-53])
keep <- matrix(TRUE, ncol(clean_train))
nums <- 1:ncol(corMat)

# If any later columns are more than .8 correlated with this column, then
# remove them from the final list
for (i in 1:ncol(corMat)) {
    keep[(nums>i) & (corMat[i,]>.8)] <- FALSE
}
trainSub <- clean_train[,which(keep)]
testSub <- clean_test[,which(keep)]

inTrain <- createDataPartition(trainSub$classe, p=.75, list=FALSE)
trainSubSmall <- trainSub[inTrain,]

rf <- randomForest(classe~., data=trainSubSmall, importance=TRUE, ntree=200)

answers <- predict(rf, testSub)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(answers)
