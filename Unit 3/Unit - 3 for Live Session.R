summary(fifa)
install.packages("GGally")
library(GGally)
## Part 1 - Filter the data set to create a dataframe that has just the Left Midfielders (LM) and Left Forwards (LF).  
fifa

fifa_lmlf = fifa %>%
  select(ID, Name, Age, Nationality, Position, Height, Weight, Acceleration, Agility) %>% 
  filter(Position %in% c("LM", "LF"))

summary(fifa_lmlf)

## Part 1 - Use GGally and ggpairs(), to plot the categorical variable Position (LM and LF), versus the continuous variables Acceleration and Agility.  

fifa_lmlf %>% 
  select(Position, Acceleration, Agility) %>%
  ggpairs(aes(color = Position))

## Part 1 - Test if the mean agility rating of left midfielders is different than that of the left forwards.  

lm=fifa_lmlf%>%filter(Position == "LM")
lf=fifa_lmlf%>%filter(Position == "LF")

t.test(x = lm$Agility, y = lf$Agility, conf.int = .95, var.equal = TRUE, alternative = "two.sided")

## Part -1 - Are the assumptions of this test reasonably met?  

lm%>%ggplot(aes(x=Agility))+geom_histogram()+ggtitle("Agility of Left Midfielders")

lf%>%ggplot(aes(x=Agility))+geom_histogram()+ggtitle("Agility of Left Forwards")

qqnorm(lm$Agility, main = "Left Midfielders QQ Plot")
qqline(lm$Agility, col='red')

qqnorm(lf$Agility,main = "Left Forwards QQ Plot")
qqline(lf$Agility, col='red')

ggplot(data=fifa_lmlf,aes(y=Agility, color=Position))+geom_boxplot()

sd(lm$Agility)
sd(lf$Agility)


######Part -2 - Create Categorical variable #########

Pot_Cat = cut(fifa$Potential, breaks = c(0,70,80,90, 100), labels = c("Low","Medium", "High", "Very High"))

##Continous variable

Penalties
# Converting Weight to numeric
fifa$Weight

w1 <- transform(fifa, Weight=as.numeric(gsub("\\D+", "", Weight)))

w1$Weight

fifa%>%ggplot(aes(x=w1$Weight,y=Penalties, color=Pot_Cat))+geom_smooth()+xlab("Weight in lbs") +ggtitle("Players Penalty Score vs Weight")+labs(color = "Player Potential") 


fifa%>%ggplot(aes(x=Penalties,y=SprintSpeed,fill=International.Reputation))+geom_smooth()

w1%>%ggplot(aes(x=Penalties,y=International.Reputation, color=Preferred.Foot))+geom_smooth()

w1%>%ggplot((aes(x=Weight)))+geom_histogram()
w1%>%ggplot((aes(x=Agility)))+geom_histogram()


