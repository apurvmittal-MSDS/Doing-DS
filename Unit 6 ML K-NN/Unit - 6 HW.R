############################################################
############## Question 1 ##################################
############################################################
############################################################

library(RCurl)
library(jsonlite)
library(httr)

baseURL <- "https://public.opendatasoft.com/api/records/1.0/search/?dataset=titanic-passengers&rows=2000&facet=survived&facet=pclass&facet=sex&facet=age&facet=embarked"

get_json <- getURL(baseURL)

get_data = fromJSON(get_json)

titanicDF <- get_data$records$fields

head(titanicDF)



# Read Training Data
titanic_train = read.csv("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 6/Dr Sadler/titanic_train.csv", header = TRUE)

#Read Test Data
titanic_test = read.csv("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 6/Dr Sadler/titanic_test.csv", header = TRUE)

titanic_train$Survived=as.factor(titanic_train$Survived)

#Change the factor value for response variable

titanic_train$Survived = factor(titanic_train$Survived,labels = c("Died","Survived"))

AgeClassTrain=titanic_train%>%filter(!is.na(Age))
classifications = knn.cv(AgeClassTrain[,c(3,6)],AgeClassTrain$Survived,k=15, prob = TRUE)
confusionMatrix(table(AgeClassTrain$Survived,classifications))
AgeClassTrain%>%ggplot(aes(x=Age, y=Pclass,color=Survived))+geom_point() + ylab("Class") +ggtitle("Survival Rate based on Age and Class - Train Data")


# Standardizing
dfZTrain = data.frame(zAge = scale(AgeClassTrain$Age), zClass = scale(AgeClassTrain$Pclass), Survived = AgeClassTrain$Survived)

classifications = knn.cv(dfZTrain[,1:2],dfZTrain$Survived, k = 15)
confusionMatrix(classifications,dfZTrain$Survived)

dfZTrain%>%ggplot(aes(x=zAge, y=zClass,color=Survived))+geom_point() + ylab("Class")

### Check KNN probability of survival bases on my Age and different classes
myAgeClass1 = data.frame(Pclass = 1, Age = 38)
myAgeClass2 = data.frame(Pclass = 2, Age = 38)
myAgeClass3 = data.frame(Pclass = 3, Age = 38)

# Age and Class 1

knn(AgeClassTrain[,c(3,6)], myAgeClass1, AgeClassTrain$Survived,k=15, prob = TRUE)

# Age and Class 2

knn(AgeClassTrain[,c(3,6)], myAgeClass2, AgeClassTrain$Survived,k=15, prob = TRUE)

# Age and Class 3

knn(AgeClassTrain[,c(3,6)], myAgeClass3, AgeClassTrain$Survived,k=15, prob = TRUE)
  

#### Try the model against the Test Data set

titanic_test = read.csv("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 6/Dr Sadler/titanic_test.csv", header = TRUE)



Train=titanic_train%>%filter(!is.na(Age))
Test=titanic_test%>%filter(!is.na(Age))

classifications = knn(Train[,c(3,6)],Test[,c(2,5)], AgeClassTrain$Survived,k=15, prob = TRUE)


Test$Survived = classifications


Test%>%ggplot(aes(x=Age, y=Pclass,color=Survived))+geom_point() + ylab("Class")+ggtitle("Survival Rate based on Age and Class - Test Data")

#############################################################################
##################Try with splitting Train Data 70/30 #######################
#############################################################################


splitPerc = .70
titanic_Data = read.csv("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 6/Dr Sadler/titanic_train.csv", header = TRUE)

trainIndices = sample(1:dim(titanic_Data)[1],round(splitPerc * dim(titanic_Data)[1])) # selecting the 70% of dataset for training.
train1 = titanic_Data[trainIndices,]
test1 = titanic_Data[-trainIndices,]
trainF=train1%>%filter(!is.na(Age))
testF=test1%>%filter(!is.na(Age))


#Change the factor value for response variable

trainF$Survived = factor(trainF$Survived,labels = c("Died","Survived"))
testF$Survived = factor(testF$Survived,labels = c("Died","Survived"))

classifications = knn(trainF[,c(3,6)],testF[,c(3,6)], trainF$Survived,k=15, prob = TRUE)

table(classifications,testF$Survived)
confusionMatrix(table(classifications,testF$Survived))


############################################################
############## Question 2 .1 ###############################
############################################################
############################################################

summary(iris)
set.seed(123) # set seed data
iterations = 500
numks = 90
splitPerc = .70

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  trainIndices = sample(1:dim(iris)[1],round(splitPerc * dim(iris)[1]))
  train = iris[trainIndices,]
  test = iris[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(1:2)],test[,c(1:2)],train$Species, prob = TRUE, k = i)
    table(classifications,test$Species)
    CM = confusionMatrix(table(classifications,test$Species))
    masterAcc[j,i] = CM$overall[1]
  }
  
}

MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1),MeanAcc, type = "l", main = "Plot of value of K vs. Accuracy", xlab = "Value of K", ylab="Accuracy %")

which.max(MeanAcc)
max(MeanAcc)

############################################################
############## Question 2 .2 ###############################
############################################################
############################################################


set.seed(123)
iterations = 500
numks = 90

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  
  for(i in 1:numks)
  {
    classifications = knn.cv(iris[,c(1:2)],iris[,5],k = i)
    CM = confusionMatrix(table(iris[,5],classifications))
    masterAcc[j,i] = CM$overall[1]
    
  }
  
}

MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1),MeanAcc, type = "l", main = "Plot of value of K vs. Accuracy (Internal CV)", xlab = "Value of K", ylab="Accuracy %")


which.max(MeanAcc)
max(MeanAcc)


