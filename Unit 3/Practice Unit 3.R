
#Dataframe for the Example
age = c(22,21,NA,24,19,20,23)
yrs_math_ed = c(4,5,NA,2,5,3,5)
names = c("Mary","Martha","Rosy","Kim","Kristen","Amy","Sam")
subject = c("English","Math",NA,"Sociology","Math","Music","Dance")
df = data.frame(Age = age, Years = yrs_math_ed,Name = names, Major = subject)
df

is.na(df$Years)
is.na(df$Name)
is.na(df$Major)

#### Filter out 'N/A' values by selecting value which are not 'NA' in 'Major' variable
df%>%filter(!is.na(Major)) 

install.packages("magrittr")
install.packages("GGally")
library(GGally)
library(magrittr)
mpg%>%select(class,cty,hwy)%>% ggpairs(aes(color=class))

### nycflights13
install.packages("nycflights13")
library(nycflights13)
summary(flights)
head(flights)


#######Summarize########

library(tidyverse)

df = data.frame(Name = c("Jack","Julie","Cali","Sunny","James"), Age = c(3,4,2,1,5), Height = c(23,25,30,29,24), Gender = c("Male","Female","Female","Female","Male"))

df
df %>% group_by(Gender) %>% summarize(MeanHeight = mean(Height))


## making a continous numeric to factor

mpg$displ
displ_Factor = cut(mpg$displ, breaks = c(1,4,6,8), labels = c("Low","Medium","High"))
displ_Factor
mpg %>% mutate(displ_Factor = displ_Factor) %>% ggplot(aes(x = hwy, y = cty, color = displ_Factor)) +geom_point() + ggtitle("City MPG v. Highway MPG by Displacement")



