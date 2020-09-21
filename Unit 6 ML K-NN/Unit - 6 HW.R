

############################################################
############## Question 2 .1################################
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


