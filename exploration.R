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

# remove the metadata which will cause overfitting to nongeneralized features
# ie. username, time, etc
clean_train <- clean_train[,8:ncol(clean_train)]

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

inTrain <- createDataPartition(trainSub$classe, p=.05, list=FALSE)
trainSubSmall <- trainSub[inTrain,]

# set up parallelization
#cl <- makeCluster(detectCores())
#registerDoParallel(cl)


# Use random forest, and return similarity matrix (prox=T)
ptm1 <- proc.time()
modFit <- train(classe~., data=trainSubSmall, method="rf", prox=T)
ptm2 <- proc.time()
#stopCluster(cl)
ptm2-ptm1