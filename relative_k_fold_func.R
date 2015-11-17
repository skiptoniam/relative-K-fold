spp <- as.data.frame(read.csv('JSDM_4species_without_forest_plots.csv'))
nk <- 5
nkl <- 5
kfold_data <- list()
for(i in 1:nk){
  pres <-as.data.frame(apply(spp[,c(2,3,4,5)],2,function(x) rbinom(x,x,1/nkl)))
  print(colSums(pres))
  pres_id <- as.numeric(rownames(pres[rowSums(sapply(pres[,1:4],`!=`,e2=0))==1,]))
  abs_id <- sample(as.numeric(rownames(pres[rowSums(sapply(pres[,1:4],`!=`,e2=0))==0,])),nrow(pres)/nkl,replace = TRUE)
  print(nrow(spp[unique(c(pres_id,abs_id)),]))
  kfold_data[[i]]<- spp[unique(c(pres_id,abs_id)),]
#   spp <- spp[-unique(c(pres_id,abs_id)),]
#   nkl <- nkl - 1
}  

library(plyr)
library(randomForest)

data <- iris

# in this cross validation example, we use the iris data set to 
# predict the Sepal Length from the other variables in the dataset 
# with the random forest model 

k = 5 #Folds

# sample from 1 to k, nrow times (the number of observations in the data)
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

# prediction and testset data frames that we add to with each iteration over
# the folds

prediction <- data.frame()
testsetCopy <- data.frame()

#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  
  # run a random forest model
  mymodel <- randomForest(trainingset$Sepal.Length ~ ., data = trainingset, ntree = 100)
  
  # remove response column 1, Sepal.Length
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  # append this iteration's predictions to the end of the prediction data frame
  prediction <- rbind(prediction, temp)
  
  # append this iteration's test set to the test set copy data frame
  # keep only the Sepal Length Column
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
  progress.bar$step()
}

# add predictions and actual Sepal Length values
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)

# As an example use Mean Absolute Error as Evalution 
summary(result$Difference)
