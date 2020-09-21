#install package and load library
install.packages("class")
install.packages("caret")
install.packages("e1071")
library(class)
library(caret)
library(e1071)

# Simple Example Credit Rating as a Function of income and debt

dfTrain = data.frame(income = c(34,67,70,110,89,90,102,104,110,120,170), 
                     CreditRating = c(750,680,670,675,710,690,715,720,650,710,720), 
                     Qualify = c("Yes","No","No","Yes","No","No","Yes","Yes","No","Yes","Yes"))

dfTrain %>% ggplot(aes(x = CreditRating, y = income, color = Qualify)) + geom_point()

dfTest = data.frame(income = 92, CreditRating = 694)

knn(dfTrain[,1:2], dfTest, dfTrain$Qualify, k = 7, prob = TRUE)

#Iris Example Classification

irisVersVirg = iris %>% filter(Species == "versicolor" | Species == "virginica")

df = data.frame(Sepal.Length = 6.1 , Sepal.Width = 2.5 )
df1 = data.frame(Petal.Length = 4.9 , Sepal.Width = 6.2 )

knn(irisVersVirg[,1:2],df,irisVersVirg$Species, k=5,prob = FALSE)
knn(irisVersVirg[,1:2],df,irisVersVirg$Species, k=15,prob = FALSE)

knn(irisVersVirg[,c(1,2)], df, irisVersVirg$Species, k = 5, prob = TRUE)
knn(irisVersVirg[,c(1,2)], df, irisVersVirg$Species, k = 15, prob = TRUE)


############################# Iris Example Cross Validation ####################################
############################# Train and Test Data           #################################### 

#Virginica v. Versicolor
set.seed(6)
splitPerc = .75
irisVersVirg = iris %>% filter(Species == "versicolor" | Species == "virginica")
summary(irisVersVirg)
irisVersVirg = droplevels(irisVersVirg,exclude = "setosa") # Dropping level as otherwise Setosa shows up as level albeit with 0 datapoints
summary(irisVersVirg)

trainIndices = sample(1:dim(irisVersVirg)[1],round(splitPerc * dim(irisVersVirg)[1])) # selecting the 75% of dataset for training.
train = irisVersVirg[trainIndices,]
test = irisVersVirg[-trainIndices,]

irisVersVirg %>% ggplot(aes(x = Sepal.Length,Sepal.Width,color = Species)) + geom_point()

# k = 3
classifications = knn(train[,c(1,2)],test[,c(1,2)],train$Species, prob = TRUE, k = 3)
table(classifications,test$Species)
confusionMatrix(table(classifications,test$Species))

# k = 5
classifications = knn(train[,c(1,2)],test[,c(1,2)],train$Species, prob = TRUE, k = 5)
table(test$Species,classifications)
confusionMatrix(table(test$Species,classifications))

# k = 10
classifications = knn(train[,c(1,2)],test[,c(1,2)],train$Species, prob = TRUE, k = 10)
table(test$Species,classifications)
confusionMatrix(table(test$Species,classifications))


# k = 20
classifications = knn(train[,c(1,2)],test[,c(1,2)],train$Species, prob = TRUE, k = 20)
table(test$Species,classifications)
CM = confusionMatrix(table(test$Species,classifications))
CM$overall[1]

############################# Iris Example Cross Validation ####################################
############################# Train and Test Data           #################################### 
############################# Loop for many k and one training / test partition ################

accs = data.frame(accuracy = numeric(30), k = numeric(30))

for(i in 1:30)
{
  classifications = knn(train[,c(1,2)],test[,c(1,2)],train$Species, prob = TRUE, k = i)
  table(test$Species,classifications)
  CM = confusionMatrix(table(test$Species,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}

plot(accs$k,accs$accuracy, type = "l", xlab = "k")


############################# Iris Example Cross Validation ####################################
############################# Train and Test Data           #################################### 
########### Loop for many k and the average of many training / test partition ##################

iterations = 500
numks = 30

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(irisVersVirg)[1],round(splitPerc * dim(irisVersVirg)[1]))
  train = irisVersVirg[trainIndices,]
  test = irisVersVirg[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(1,3)],test[,c(1,3)],train$Species, prob = TRUE, k = i)
    table(classifications,test$Species)
    CM = confusionMatrix(table(classifications,test$Species))
    masterAcc[j,i] = CM$overall[1]
  }
  
}

MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1),MeanAcc, type = "l")

#################################################
#################################################
#################################################
Emails = data.frame(Predicted = c("Spam","Ham","Ham", "Ham", "Ham", "Spam", "Ham", "Spam", "Ham", "Spam"), Actual = c("Spam", "Spam", "Ham", "Ham", "Spam", "Ham", "Spam","Ham","Spam","Spam" ))

table(Emails)

#Internal Cross Validation

# Simple Example Credit Rating as a Function of income and debt

df = data.frame(income = c(34,67,70,110,89,90,102,104,110,120,170), 
                CreditRating = c(750,680,670,675,710,690,715,720,650,710,720), 
                Qualify = c("Yes","No","No","Yes","No","No","Yes","Yes","No","Yes","Yes"))

knn.cv(df[,1:2], df$Qualify, k = 3)

## Iris data for Internal CV
## 

iris
classifications = knn.cv(iris[,c(1:2)], iris$Species,k=10)
confusionMatrix(table(iris$Species, classifications))

##################################################################
##################################################################
####################### Standardization ##########################

# Simple Example Credit Rating as a Function of income and debt...Not standardized

dfTrain = data.frame(income = c(34000,67000,70000,110000,89000,90000,102000,104000,110000,120000,170000), 
                     CreditRating = c(750,680,670,675,710,690,715,720,650,710,720), 
                     Qualify = c("Yes","No","No","Yes","No","No","Yes","Yes","No","Yes","Yes"))

dfTrain$Qualify = as.factor(dfTrain$Qualify)
classification = knn.cv(dfTrain[,1:2],dfTrain$Qualify, k = 3)
confusionMatrix(classification, dfTrain$Qualify)


# Simple Example Credit Rating as a Function of income and debt ... Standardized

dfTrain = data.frame(income = c(34,67,70,110,89,90,102,104,110,120,170), 
                     CreditRating = c(750,680,670,675,710,690,715,720,650,710,720), 
                     Qualify = c("Yes","No","No","Yes","No","No","Yes","Yes","No","Yes","Yes"))

dfZTrain = data.frame(Zincome = scale(dfTrain$income), ZCreditRating = scale(dfTrain$CreditRating), Qualify = dfTrain$Qualify)

classifications = knn.cv(dfZTrain[,1:2],dfZTrain$Qualify, k = 3)
confusionMatrix(classifications,dfTrain$Qualify)



# Simple Example Credit Rating as a Function of income and debt ... Similar Scale

dfTrain = data.frame(income = c(34,67,70,110,89,90,102,104,110,120,170), 
                     CreditRating = c(750,680,670,675,710,690,715,720,650,710,720), 
                     Qualify = c("Yes","No","No","Yes","No","No","Yes","Yes","No","Yes","Yes"))

dfTest = data.frame(income = 92, CreditRating = 694)

knn(dfTrain[,1:2], dfTest, dfTrain$Qualify, k = 5, prob = TRUE)

classifications = knn.cv(dfTrain[,1:2],dfTrain$Qualify, k = 3)
confusionMatrix(classifications,dfTrain$Qualify)


#Example Default

#read in data Credit Default.csv
credit = read.csv(file.choose(),header = TRUE)

#make resposnse a factor rather than 0,1
credit$default.payment.next.month = factor(credit$default.payment.next.month,labels = c("NoDefault","Default"))
summary(credit)
#plot the data
credit %>% ggplot(aes(x = AGE, y = LIMIT_BAL,color = default.payment.next.month)) + geom_point()

#Create standardized variables for later. 
#credit$Z_Lim = (credit$LIMIT_BAL-mean(credit$LIMIT_BAL))/sd(credit$LIMIT_BAL)
#credit$Z_AGE = (credit$AGE-mean(credit$AGE))/sd(credit$AGE)
credit$Z_Lim = scale(credit$LIMIT_BAL)
credit$Z_AGE = scale(credit$AGE)

#create training and test sets
trainInd = sample(seq(1,30000,1), .8*30000)
train = credit[trainInd,]
test = credit[-trainInd,]

#External CV
#Raw Limit and AGE
classifications = knn(train[,c(2,6)],test[,c(2,6)],train$default.payment.next.month,prob = TRUE, k = 5)
confusionMatrix(table(classifications,test$default.payment.next.month))

#Standardized
classifications = knn(train[,c(15,16)],test[,c(15,16)],train$default.payment.next.month,prob = TRUE, k = 5)
confusionMatrix(table(classifications,test$default.payment.next.month))


#Internal CV
#Raw Limit and AGE
classifications = knn.cv(credit[,c(2,6)],credit$default.payment.next.month,prob = TRUE, k = 5)
confusionMatrix(table(classifications,credit$default.payment.next.month))


#Standardized
classifications = knn.cv(credit[,c(15,16)],credit$default.payment.next.month,prob = TRUE, k = 5)
confusionMatrix(table(classifications,credit$default.payment.next.month))

###############3
###############
###############


# Archeology

pottery = read.csv(file.choose(),header = TRUE)
pottery
confusionMatrix(table(knn.cv(pottery[,1:5],pottery$Site, k = 3), pottery$Site))
QOI = data.frame(Al = 21, Fe = 6.7, Mg = 4.9, Ca = 0.10, Na = 0.11)
knn(pottery[,1:5],QOI,pottery$Site, prob = TRUE, k = 3)
knn(pottery[,1:5],QOI,pottery$Site, prob = TRUE, k = 5)
