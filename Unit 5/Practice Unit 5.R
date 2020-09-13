df1 = data.frame(Student_ID = c("1234", "2345", "8910", "9101", "3456", "5678","8888"), Course = c("Time Series", "NLP", "Stats1", "DDS", "DDS", "ML2","Data Mining"))
df2 = data.frame(Student_ID = c("1234", "2345", "8910", "9101", "3456", "5678","99999", "11111"), Gender = c("M", "F", "M", "F", "F", "F", "M", "M"), State = c("TX", "TX", "CA", "ID", "NY", "FL","NM", "AZ") )

#inner join
merge(df1,df2, by = "Student_ID")
#or
inner_join(df1,df2,by = "Student_ID")
#or
df1 %>% inner_join(df2,by = "Student_ID")


#outer join
merge(df1,df2, by = "Student_ID",all = TRUE)
#or
full_join(df1,df2,by = "Student_ID")
#or
df1 %>% full_join(df2,by = "Student_ID")


#left join
merge(df1,df2, by = "Student_ID",all.x = TRUE)
left_join(df1,df2, by = "Student_ID")


#right join 
merge(df1,df2, by = "Student_ID",all.y = TRUE)
right_join(df1,df2, by = "Student_ID")


#Different Column Names: by.x and by.y
df3 = data.frame(Student_ID_Number = c("1234", "2345", "8910", "9101", "3456", "5678", "8888"), Course = c("Time Series", "NLP", "Stats1", "DDS", "DDS", "ML2", "Data Mining"))
df4 = data.frame(Student_ID = c("1234", "2345", "8910", "9101", "3456", "5678","99999", "11111"), Gender = c("M", "F", "M", "F", "F", "F", "M", "M"), State = c("TX", "TX", "CA", "ID", "NY", "FL","NM", "AZ") )

merge(df3,df4, by.x = "Student_ID_Number", by.y = "Student_ID")

###############################
###############################
###############################
###############################
#Headmap from AcuSpike##


#Data clean and heat map for Acuspikes 
# Data has been read into a Dataframe called "Acu"

library(ggplot2)
library(maps)
library(dplyr)
library(mapproj)

Acu = read.csv(file.choose(),header = TRUE) # read in company data
head(Acu)
dim(Acu)
lookup = data.frame(abb = state.abb, State = state.name) #makes a data frame with State name and abbreviation. 
colnames(Acu)[2] = "abb" # Change Column Name
Acu2 = merge(Acu,lookup,"abb") # make one dataset with state names and abb (Inner Join)

AcuMapData = count(Acu2,State) #count up the occurance of each state. 

#AcuMapData = AcuMapData[-c(5,9,43),] #Shows contrast between other states better
colnames(AcuMapData)[2] = "AcuSpikes" #change "n" to "Acuspikes"
AcuMapData$region <- tolower(AcuMapData$State)
AcuMapData2 = AcuMapData[-1]
states <- map_data("state")
map.df <- merge(states,AcuMapData2, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=AcuSpikes))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(100)),na.value="grey90")+ggtitle("Acuspike Systems by State")+
  coord_map()