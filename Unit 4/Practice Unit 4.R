olddata_wide <- read.table(header=TRUE, text='
subject genderage Control Treatment1 Treatment2
                           1   32_M     7.9  12.3  10.7
                           2   45_F     6.3  10.6  11.1
                           3   27_F     9.5  13.1  13.8
                           4   23_M    11.5  13.4  12.9
                           ')
olddata_wide$subject <- factor(olddata_wide$subject)

olddata_wide

#Change to Long
data_long <- gather(olddata_wide, condition, measurement, Control:Treatment2, factor_key=TRUE)
data_long = data_long %>% separate(genderage, into = c("age","gender"))
data_long
str(data_long)
data_long$age = as.numeric(data_long$age)
str(data_long)

#Change to Wide
data_wide <- spread(data_long, condition, measurement)
data_wide

### Merging two data sets

demographic <- read.table(header=TRUE, text='
                           subject age gender  condition 
                                  1     32   M    Control   
                                  2     45   F    Control     
                                  3     27   F    Control    
                                  4     23   M    Control       
                                  1     32   M Treatment1     
                                  2     45   F Treatment1       
                                  3     27   F Treatment1       
                                  4     23   M Treatment1       
                                  1     32   M Treatment2      
                                  2     45   F Treatment2      
                                  3     27   F Treatment2      
                                  4     23   M Treatment2
                           ')
demographic$subject <- factor(demographic$subject)

demographic


experimentData <- read.table(header=TRUE, text='
                         subject   condition measurement
                             1         Control         7.9
                             2         Control         6.3
                             3         Control         9.5
                             4         Control        11.5
                             1      Treatment1        12.3
                             2      Treatment1        10.6
                             3      Treatment1        13.1
                             4      Treatment1        13.4
                             1      Treatment2        10.7
                             2      Treatment2        11.1
                             3      Treatment2        13.8
                             4      Treatment2        12.9
                           ')
experimentData$subject <- factor(experimentData$subject)

experimentData

fullData = merge(demographic,experimentData,c("subject","condition"))
fullData

## Missing Data
```{r}
df = data.frame(year = c(2018,2018,2018,2018, 2019,2019,2019, 2019,2020,2020,2020), 
                quarter = c(1,2,3,4,1,2,3,4,2,3,4), 
                price = c(4.55,NA, 6.75, 8.2, 8.5, 9.6, 10.1, 11.0, 12.1, 13.1, 15.9),
                char = c("Sunny","Roger",NA,"NA","Sunny","Roger",NA,"NA","Sunny","Roger",NA))
df %>% complete(year, quarter) #all possible combinations of year and quarter for the levels observed.
df %>% spread(year,price) #change to wide


install.packages("jsonlite")
install.packages("RJSONIO")
library(jsonlite)
library(RJSONIO)



################################################
######### Twitter #############################







