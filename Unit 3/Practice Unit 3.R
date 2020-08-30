
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



########## Missing Values##################

#Fifa
gg_miss_var(fifa[,1:40]) + ylim(0,75)

gg_miss_var(fifa[,41:89]) + ylim(0,75)

#mpg

gg_miss_var(mpg) + ylim(0,10)

sapply(mpg, function(x) sum(is.na(x)))

#nycflights13
library(nycflights13)
gg_miss_var(flights)
sapply(flights, function(x) sum(is.na(x)))

##############################################

## Adding Label or Data on the plots  ##

total = count(mpg,class)
total

mpg%>%ggplot(aes(x=class, fill = class))+geom_bar()+ geom_text(data = total, aes(class,n+1, label=n))

###############################


#### MISSING VALUES ######

a = read_csv("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 3/Dr Sadler/MissingDataToy.csv")
a
str(a)
# count missing values in each column
s = sapply(a, function(x) sum(is.na(x)))
s
# Make all missing values NA
str(a)
a$chars[6] = NA
str(a)
a$chars = as.character(a$chars)
str(a)

### Provides a plot of number of missing values
library(naniar)
gg_miss_var(a)
a
# example
#Fifa
gg_miss_var(fifa[,1:40]) + ylim(0,75)

gg_miss_var(fifa[,41:89]) + ylim(0,75)






#### Diamonds Dataset###############

diamonds
str(diamonds)
gg_miss_var(diamonds)
d = sapply(diamonds, function(x) sum(is.na(x)))
d
diamonds%>%ggplot(aes(x=z))+geom_boxplot()

diamonds$ztest=diamonds$z
ztest
is.na(diamonds$z)
diamonds


### Replacing values with NA if z<3 or>20#######
diamonds_1 = diamonds %>% replace_with_na_at(.vars = c("z"), condition = ~.x <3)
diamonds_1
diamonds_2 = diamonds %>% replace_with_na_at(.vars = c("z"), condition = ~.x >20)
diamonds_2
gg_miss_var(diamonds_2[,1:11])
summary(diamonds)
dtest=sapply(diamonds_1,function(x) sum(is.na(x)))
dtest


diamondsz = cut(diamonds$z, breaks = c(0,4,32), labels = c("LOW","High"))
diamondsz

summary(diamondsz)
#############################
dev.off()
diamonds
diamonds %>%
  select(price, carat, cut) %>%
  ggpairs(mapping = aes(color = cut))

diamonds %>% 
  group_by(cut) %>% 
  summarize(median = median(price))

diamonds %>% 
  group_by(cut) %>% 
  summarize(median = median(carat))

diamonds %>%
  ggplot(aes(x = carat, y = price)) + 
  geom_point()

diamonds %>% 
  mutate(lcarat = log(carat), lprice = log(price)) %>%
  ggplot(aes(x = lcarat, y = lprice)) + 
  geom_point()

diamonds %>% 
  mutate(lcarat = log(carat), lprice = log(price)) %>%
  ggplot(aes(x = lcarat, y = lprice)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ylim(c(5.5, 10))

diamonds %>%
  mutate(resids = exp(fit$residuals)) %>%
  ggplot(aes(x = carat, y = resids)) + 
  geom_point()

diamonds %>%
  mutate(resids = exp(fit$residuals)) %>%
  ggplot(aes(x = carat, y = resids, color = cut)) + 
  geom_point()

diamonds %>% 
  mutate(resids = exp(fit$residuals)) %>%
  ggplot(aes(y = resids, color = cut)) +
  geom_boxplot()

diamonds %>% 
  mutate(resids = exp(fit$residuals)) %>%
  group_by(cut) %>% 
  summarise(median = median(resids))




