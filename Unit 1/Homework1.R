# Simulator to Demonstrate CLT

## Control Parameters

##df = read.table("/Users/bivin/Desktop/OLD COMPUTER ARCHIVES/KadAfrica/MSDS/DDS/MSDS 6306/Unit 5/yob2016.txt",stringsAsFactors = FALSE,header = FALSE,sep = ";")
n1 = 10 # sample size per sample for 1st distribution
n2 = 100 # sample size per sample for 2nd distribution (we will compare these distribuions) 
simulations = 1000 #number of samples and thus number of xbars we will generate.  
mu = 0; # mean parameter for use with normal distribuions
sigma = 1; # standard deviation parameter for use with normal distribuions


## Data Holder

xbar_holder1 = numeric(simulations) # This will hold all the sample means for the first distribution.
xbar_holder2 = numeric(simulations) # This will hold all the sample means for the second distribution.


## Simulate and Store
##Generate 1000 samples each of size 10 and find the mean of each sample.  Then store each mean in the xbar_holder vector.


for (i in 1:simulations)
{ 
  sample1 = rnorm(n1,mean = mu, sd = sigma)
  sample2 = rnorm(n2,mean = mu, sd = sigma)
  xbar1 = mean(sample1)
  xbar2 = mean(sample2)
  xbar_holder1[i] = xbar1
  xbar_holder2[i] = xbar2
}


## display the distribution of sample means (plot a histogram of the sample means)

par(mfrow = c(2,1))
hist(xbar_holder1, col = "blue", main = paste("Distribution of the sample mean: n = ", n1), xlab = "Dist 1 Sample Means", xlim = c(-4,4))
hist(xbar_holder2, col = "red", main = paste("Distribution of the sample mean: n = ", n2), xlab = "Dist 2 Sample Means", xlim = c(-4,4))


## summary statistics of the distribution of the simulated sample means. 

summary(xbar_holder1) #5 number summary and the mean
summary(xbar_holder2) #5 number summary and the mean
sd(xbar_holder1) # standard deviation of dstribuion 1
sd(xbar_holder2) #standard deviation of distribuion 2


##############################################################################################

?rchisq

Population=rchisq(10000000,2) #rchisq function to select a population of 10,000,000 and 2 SD
summary(Population)
Population
###############################################################################################
#histogram of the population
hist(Population, xlab="Population", main="Histogram of Population")
#Mean of ppopulation
mean(Population)
#Standard Deviation of Population
sd(Population)

###############################################################################################
sampleSize=50
theSample = sample(Population,sampleSize)
hist(theSample, xlab= "Sample = 50", main="Histogram of Sample Distribution")

#####################################################################
#Function: xbarGenerator
#Samplesize: the size of the sample that each sample mean is based on
#number_of_samples:
#####################################################################
xBarVec = c() # Global vector to hold the sample mean
xbarGenerator = function(sampleSize, number_of_samples)
{
  for (i in 1:number_of_samples)
  {
    theSample = sample(Population,sampleSize)
    xbar = mean(theSample)
    xBarVec = c(xBarVec,xbar)
  }
  return(xBarVec)
}

xbars = xbarGenerator(50,1000)
#length(xbars)
hist(xbars, xlab= "Sample Mean of Sample Size 50", main="Histogram of Sample Mean")
mean(xbars)
sd(xbars)
##################################################################################




#Barchart of my profile in DS

Category = c("Data Viz","Statistics","Mathematics","Domain Expertise", "Computer Science", "Communication", "Machine Learning" )
Score = c(1,2,3,4,2,2,0)
df=data.frame(Score, Category)
barplot(df$Score,names.arg = df$Category, col = "blue", main = "My Data Science Profile", density = 60, ylab="Score", ylim = c(0,5) )
#par(mfrow=c(1,2))
dev.off()

################################################################################################

# Simulator to Demonstrate CLT

## Control Parameters

sampleSize = 50 # sample size per sample 

simulations = 10000 #number of samples and thus number of xbars we will generate.  
mu = 0; # mean parameter for use with normal distribuions
sigma = 2; # standard deviation parameter for use with normal distribuions


## Data Holder

xbar_holder = numeric(simulations) # This will hold all the sample mean.



## Simulate and Store
##Generate 10000 samples each of size 50 and find the mean of  sample.  Then store each mean in the xbar_holder vector.


for (i in 1:simulations)
{ 
 
  theSample = sample(Population,sampleSize)
  
  xbar = mean(theSample)
  
  xbar_holder[i] = xbar
  
}


## display the distribution of sample means (plot a histogram of the sample means)

hist(xbar_holder, col = "grey", main = paste("Distribution of the sample mean: n = ", sampleSize), xlab = "Distribution of Sample Means")

## summary statistics of the distribution of the simulated sample means. 

summary(xbar_holder) #5 number summary and the mean
mean(xbar_holder)
sd(xbar_holder) # standard deviation 

###################################################################################
age = c(25,19,37,29,40,28,31)
t.test(x = age, conf.int = .95,  alternative = "two.sided", mu=21)









