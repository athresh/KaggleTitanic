#Visualize the data
library(ggplot2)
library(randomForest)

training<-read.csv("train.csv")
testing<-read.csv("test.csv")

set.seed(1)
## Preprocessing
training1<-training
training1$Survived<-as.factor(training1$Survived)

#Imputing age
meanAge<-mean(training1$Age[!is.na(training1$Age)])
training1$Age<-ifelse(is.na(training1$Age),meanAge,training1$Age)

#Imputing embarked

avgFare<-c("C"=mean(training1$Fare[training1$Embarked=="C"]),"Q"=mean(training1$Fare[training1$Embarked=="Q"]),"S"=mean(training1$Fare[training1$Embarked=="S"]))
training1$Embarked<-ifelse(training1$Embarked=="",which.min(abs(training1$Fare - avgFare)),training1$Embarked)


#Creating feature family size threshold
FSize <- training1$SibSp + training1$Parch
FSize<- ifelse(FSize>=4,2,1)
FSize<- ifelse(training1$SibSp + training1$Parch==0,0,FSize)
FSize <- as.factor(FSize)
training1<- data.frame(training1,FSize)



inTrain<-sample(1:891,625)
trainSub<-training1[inTrain,]
incv<-sample(setdiff(1:891,inTrain),266)
cvset<-training1[incv,]

## Test set preprocessing
testing1<-testing
meanFare<-mean(testing1$Fare[!is.na(testing1$Fare)])
testing1$Fare<-ifelse(is.na(testing1$Fare),meanFare,testing1$Fare)


testing1$Age<- ifelse(is.na(testing1$Age),mean(testing1$Age[!is.na(testing1$Age)]),testing1$Age)
testing1$Embarked<-ifelse(testing1$Embarked=="",which.min(abs(testing1$Fare - avgFare)),testing1$Embarked)


FSizeTest<- testing1$SibSp + testing1$Parch
FSizeTest<- ifelse(FSizeTest>=4,2,1)
FSizeTest<- ifelse(testing1$SibSp + testing1$Parch==0,0,FSizeTest)
FSizeTest<- as.factor(FSizeTest)
testing1<- data.frame(testing1,FSize=FSizeTest)

#train random forest
output.forest<-randomForest(Survived ~ Sex + Age + FSize +Pclass,data = trainSub)


prediction<-predict(output.forest,cvset)    #predicting on cross valdiation set
accuracyCV<- sum(prediction==cvset$Survived)/length(cvset$Survived)

predictTR<- predict(output.forest,training1)
accuracyTR<- sum(training1$Survived==predictTR)/length(training1$Survived)


levels(testing1$Embarked)<-levels(training1$Embarked)
levels(testing1$Cabin)<-levels(training1$Cabin)



predictTEST<-predict(output.forest,testing1)   #predicting on test set

results<-data.frame(PassengerId=testing1$PassengerId,Survived=predictTEST)
write.csv(results,"predictionsRFfinal.csv",row.names = FALSE)




SurvivalRate<- function(fr){
      sum(training1$Survived[training1$Fare>fr]==1)/sum(training1$Fare>fr)
}

numPass<- function(thr){
  (sum(training1$Fare<thr)-sum(training1$Fare<thr-10))
}