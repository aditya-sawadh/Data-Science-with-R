library (data.table)
library (plyr)
library (stringr)

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
data$DemCluster<-as.factor(data$DemCluster)
data$TargetB<-as.factor(data$TargetB)

summary(data)  
table(data$TargetB)
## function to visualize the data and make initial hypothesis 
plot_variables<- function (df){
  for (i in 1: ncol(df)){
    a=colnames(df[i])
    if(class(df[,i])=='integer'){
      hist(df[,i],main= a)
    }else plot(df[,i],main=a)
  }
}
plot_variables(data)

## Treat missing values and prepare data set
data[data$DemMedIncome==0,'DemMedIncome']<- median(data$DemMedIncome,na.rm= TRUE)
data[data$DemMedHomeValue==0,'DemMedHomeValue']<-median(data$DemMedHomeValue, na.rm= TRUE)
data[is.na(data$DemAge),'DemAge'] <- as.integer(median(data$DemAge, na.rm= TRUE))
data[is.na(data$DemAge),'DemAge'] <-0
data[is.na(data$GiftAvgCard36),'GiftAvgCard36']<-0

## Splitting data into training and testing
trainIdx<- createDataPartition(data$TargetB, p =0.7 , list = FALSE)
training<- subset(data,select=-c(ID))[trainIdx,]
testing<- data[-trainIdx,]

#Logistic Regression initial model 
model <- glm(TargetB ~ ., family = binomial(link = 'logit'), data = training)
summary(model)
anova(model, test = 'Chisq')

## Model Tuning: Only significant features and evaluating the performance based on AIC value
model_1<- glm(TargetB ~ GiftCnt36+GiftCntAll+GiftCntCard36+ GiftTimeLast+ GiftTimeFirst+ PromCntCardAll +DemMedHomeValue  , family = binomial(link = 'logit'), data = training)
summary(model_1)
anova(model_1, test = 'Chisq')

#Prediction on Test data and creating confusion matrix
pred_model <- predict(model_1, subset(testing,select =c(GiftCnt36,GiftCntAll,GiftCntCard36,GiftTimeLast,GiftTimeFirst,PromCntCardAll,DemMedHomeValue)),type="response")
for(i in 1: length(pred_model)) {if (pred_model[i]<0.5){pred[i]<-0}else {pred[i]<-1}}
matrix<-as.matrix(table(observed = testing[,1], predicted = pred))

sensitivity_model<-matrix[1,2]/sum(matrix[,2])
specificity_model<-matrix[1,1]/sum(matrix[,1])

