


########################################
######### Question 1 ###################
########################################
########################################
library(dplyr)
library(tidyr)
basketball = read.csv(file ="/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 2/PlayersBBall.csv", header = T, sep = ",", skipNul = T, blank.lines.skip = TRUE, strip.white = T, na.strings = c("","NA")) 
#baseball_Data = baseball_Data %>% na.omit()
#unique(baseball_Data$position == "")
# filter out empty rows
## https://dzone.com/articles/r-dplyr-removing-empty-rows
basketball = separate(basketball, height, into = c("feet","inches"),sep="-")
basketball$feet = as.numeric(basketball$feet)
basketball$inches = as.numeric(basketball$inches)
basketball$Tot.height =  12 * basketball$feet + basketball$inches
head(basketball)

basketball%>%ggplot(aes(x=Tot.height))+geom_bar(aes(color = position))+ ggtitle("Distribution of Height of Players at different Positions")+xlab("Height in Inches")+ylab("Count")



########################################
######### Question 2 ###################
########################################
########################################
########################################
 
fifa = read.csv(file ="/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 3/FIFA Players.csv", header = T, sep = ",", skipNul = T, blank.lines.skip = TRUE, strip.white = T, na.strings = c("","NA")) 

fifaWT<- transform(fifa, Weight=as.numeric(gsub("\\D+", "", Weight)))
str(fifaWT$Weight)

fifaWT<-separate(fifaWT, Height, into=c("feet","inches"), sep="'")
fifaWT$feet=as.numeric(fifaWT$feet)
fifaWT$inches=as.numeric(fifaWT$inches)
fifaWT$Tot.Height = 12*fifaWT$feet+fifaWT$inches

fifaWT%>%ggplot(aes(x=Tot.Height,y=Weight))+geom_smooth() + geom_point(shape = 18, size = 3)+ ggtitle("Realtionship between Height and Weight of Players")+xlab("Height in Inches")+ylab("Weight in lbs")


fifa.Pos = fifaWT %>%
  select(ID, Name, Age, Nationality, Position, Tot.Height, Weight) %>% 
  filter(Position %in% c("LB", "LM"))

fifa.Pos%>%ggplot(aes(x=Tot.Height,y=Weight, color=Position))+geom_smooth() + geom_point(shape = 18, size = 2)+ ggtitle("Relationship of Height and Weight for LB and LM Positions")+xlab("Height in Inches")+ylab("Weight in lbs") 

########################################
######### Question 3 ###################
########################################
########################################
########################################

# 3.1#

# Read the datafile to df
df<- read.table("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 5/yob2016.txt", 
                    sep =";", header = FALSE, dec =".",, col.names = c("Name", "Gender.2016", "Count.2016"))

summary(df)

# Detect and filter Baby name with three "y"

df%>%filter(str_detect(Name,"[y]{3}"))

# Save the dataset without the misspelled name with three y's

fy2016 = df%>%filter(!str_detect(Name,"[y]{3}"))

# 3.2#

# Read the 2015 datafile to df
y.2015 <- read.table("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 5/yob2015.txt", 
                      sep =",", header = FALSE, dec =".", col.names = c("Name", "Gender.2015", "Count.2015"))

tail(y.2015,n=10)

# Inner join two datasets by Name
Babies<-inner_join(y.2015,fy2016, by="Name")

#Filter out any NA values
Final = Babies%>%filter(!is.na(Count.2015)| !is.na(Count.2016))

# Converting the Gender to Factor
Final$Gender.2015=as.factor(Final$Gender.2015)
Final$Gender.2016=as.factor(Final$Gender.2016)

# 3.3#

# Creating a new Column with the total count of 2015 and 2016
Final$Total = Final$Count.2015+Final$Count.2016

# Sum of babies with popular names in 2015 and 2016
sum(Final$Total)

# Sort the Babies names
Final <- Final[order(Final$Total, decreasing = TRUE),]

head(Final[-c(2:5)],n=10)

# Omit boys names

Girls<-Final%>%filter(Gender.2015 =="F"& Gender.2016 =="F")


# write to csv
write.csv(Girls[-c(2:5)], file = "/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 5/Unit5_Girl_Babies_Apurv.csv")

########################################
######### Question 4 ###################
########################################
########################################
########################################


# sorted 2015 Data
y.2015 <- y.2015[order(y.2015$Count.2015, decreasing = TRUE),]

TopTen2015<-head(y.2015,n=10)

# sorted 2016 Data
fy2016 <- fy2016[order(fy2016$Count.2016, decreasing = TRUE),]
TopTen2016<-head(y.2016,n=10)

TopTen = data.frame(TopTen2015,TopTen2016)

Final%>%ggplot(aes(x=Gender.2015))+geom_bar()

Final%>%ggplot(aes(x=Gender.2016))+geom_bar()


########################################

sum(str_count(Final$Name,"(\\bA\\b)"))

str_count(Final$Name,"(\\. A| \"A)")

matchDF %>% ggplot(aes(x = Colors, fill = Colors)) + geom_bar()+ scale_fill_manual(values=colors[order(colors)])

# Calculating the percentage of name popularity
Final$Percent = (Final$Total/sum(Final$Total))


top08 <- Final %>% select(Name, Percent, Gender.2015) %>% arrange(desc(Percent)) %>% group_by(Gender.2015) %>% top_n(n =10, wt = Percent)


ggplot(top08, aes(top08, x = reorder(Name, Percent),y = Percent, fill = Gender.2015)) + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("") + ggtitle(label="Most popular baby names in 2015 and 2016") + scale_y_continuous(labels = scales::percent) + facet_wrap(c("Gender.2015"), scales = "free_y")  + theme_minimal() + theme(legend.position="none")  + scale_fill_hue(l=40, c=35) + scale_fill_manual(values=c("blue", "red"))