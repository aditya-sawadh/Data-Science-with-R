install.packages('caret') # for createDataPartition, train, predict
install.packages('rpart.plot')

setwd(user_path)
library(readr)
library(rpart.plot)
library(randomForest)

library(RColorBrewer)
data<-read.csv("veterans.csv")

################Exploratory data analysis and Preprocessing the data###########################

data$TargetD <- NULL
sapply(data,class) # Need to treat $ gift amounts

#### removing dollar signs and commas from $value fields and making them numeric
data$GiftAvgLast<- as.numeric(substr(data[,'GiftAvgLast'],2,length(data[,'GiftAvgLast'])))
data$GiftAvg36<- as.numeric(substr(data[,'GiftAvg36'],2,length(data[,'GiftAvg36'])))
data$GiftAvgAll<- as.numeric(substr(data[,'GiftAvgAll'],2,length(data[,'GiftAvgAll'])))
data$GiftAvgCard36<- as.numeric(substr(data[,'GiftAvgCard36'],2,length(data[,'GiftAvgCard36'])))
data$DemMedHomeValue <- as.numeric(substr(gsub(",", "",data[,'DemMedHomeValue']),2,length(data[,'DemMedHomeValue'])))
data$DemMedIncome<- as.numeric(substr(gsub(",","",data[,'DemMedIncome']),2,length(data[,'DemMedIncome'])))

summary(data)  
table(data$TargetB)
# when median income and median home value values are not present they are mapped as zero 
# Majority of Age and GiftAvgCard36 variable is null

## function to visualize the data and make initial hypothesis 
plot_variables<- function (df){
  for (i in 1: ncol(df)){
    a=colnames(df[i])
    if(class(df[,i])=='integer'){
      hist(df[,i],main= a)
    }else plot(df[,i],main=a)
  }
}
#plot_variables(data)

##Replace missing values with the mean of the column

data[data$DemMedIncome==0,'DemMedIncome']<- NA
data[data$DemMedHomeValue==0,'DemMedHomeValue']<-NA
data$DemCluster<-as.factor(data$DemCluster)
data$TargetB<-as.factor(data$TargetB)


# split the data into training and test sets 70/30 split
# take a partition of the indexes then use the index vectors to subset
###
trainIdx <- createDataPartition(data$TargetB, p =0.7 , list = FALSE)
training<- data[trainIdx,][,-2]
testing<- data[-trainIdx,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # 10 fold cross-validation repeated 3 times

## Train a classification tree based on information gain and gini index
set.seed(100)
dtree_fit<- train(TargetB ~. , data = training, method = "rpart",
                   parms = list(split = "gini"),
                   trControl=trctrl,
                   na.action = na.omit,
                   tuneLength = 10) # Accuracy=55%

## Visualizing the decision tree
dtree_fit
prp(dtree_fit$finalModel, box.palette = "Blues", tweak = 1.2)

###Predictions and confusion matrix, model evaluation
dtree.pred <- predict(dtree_fit,testing)
table(observed = testing[,1], predicted = dtree.pred)


#####Creating Random forest model with Variable Importance
## Treat missing values and prepare data sets
data_rf=data
data_rf[is.na(data_rf$DemMedIncome),'DemMedIncome']<- median(data_rf$DemMedIncome,na.rm= TRUE)
data_rf[is.na(data_rf$DemMedHomeValue),'DemMedHomeValue']<-median(data_rf$DemMedHomeValue, na.rm= TRUE)
data_rf[is.na(data_rf$DemAge),'DemAge'] <- as.integer(median(data_rf$DemAge, na.rm= TRUE))
data_rf[is.na(data_rf$DemAge),'DemAge'] <-0
data_rf[is.na(data_rf$GiftAvgCard36),'GiftAvgCard36']<-0

trainIdx_rf<- createDataPartition(data_rf$TargetB, p =0.7 , list = FALSE)
training_rf<- subset(data_rf, select=-c(ID,DemCluster))[trainIdx_rf,]
#str(training_rf)
testing_rf<- data_rf[-trainIdx_rf,]

# Baseline Random forest model
rf=randomForest(TargetB ~ . , data = training_rf ,importance=TRUE)
rf

# Tune the model with cross validation
mtry <- sqrt(ncol(x))
rf.fit <- train(TargetB ~ ., 
                data = training_rf, 
                method = "rf",     # Use the "random forest" algorithm
                importance = TRUE, # importance=TRUE allows to inspect variable importance
                trControl = trainControl(method = "repeatedcv", # Use cross-validation
                                         number = 10, # Use 10 folds for cross-validation
                                         repeats = 3))
#Model Prediction using random forests
rf.pred <- predict(rf.fit,testing_rf)
table(observed = testing_rf[,1], predicted = rf.pred)

