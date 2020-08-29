install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot")
library(ggplot2)
library(ggthemes)
install.packages("plotly")
ibrary(plotly)

Player = read.csv("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 2/PlayersBBall.csv", header = TRUE)
## Modified Data set with conversion of height into Inches. Original was in characters and Date format
PlayerMod = read.csv("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 2/PlayersBBall-Apurv.csv", header = TRUE)

Player
summary(Player)
summary(PlayerMod)
Player$position=as.factor(Player$position)
PlayerMod$college=as.factor(PlayerMod$college)


## First question - the number of players in each position
ggplot(data=Player,aes(x = position)) + geom_bar(color="blue") + ggtitle("Number of Players in each Positions")+xlab("Position of Player")+ylab("Number of Players")


## Second question - distribution of weight for Center and Forward position
ggplot(data=Player,aes(x = position, y= weight)) + geom_boxplot(color="blue") + ggtitle("Distribution of Weight for Center and Forward Position")+xlab("Position of Player")+ylab("Weight")

CentForw = Player[Player$position == "F"|Player$position == "C",]
CentForw
summary(CentForw)

CentForw%>%ggplot(aes(x = position, y= weight)) + geom_boxplot(color="blue") + ggtitle("Distribution of Weight for Center and Forward Position")+xlab("Position of Player")+ylab("Weight")

## Third question - distribution of Height for Center and Forward position

CentForwMod = PlayerMod[PlayerMod$position == "F"|PlayerMod$position == "C",]
CentForwMod
summary(CentForwMod)

CentForwMod%>%ggplot(aes(x = position, y= height.TotInches)) + geom_boxplot(color="blue") + ggtitle("Distribution of Height for Center and Forward Position")+xlab("Position of Player")+ylab("Height in Inches")

## Fourth question - distribution of Height for any position

PlayerMod%>%ggplot(aes(x = position, y= height.TotInches)) + geom_boxplot(color="blue") + ggtitle("Distribution of Height of Players at different Positions")+xlab("Position of Player")+ylab("Height in Inches")+ylim(60,95)

## Fifth question - Change in Height with Change in Weight with positions

PlayerMod%>%ggplot()+geom_point(mapping = aes(x = height.TotInches, y= weight ))+ geom_smooth(mapping = aes(x = height.TotInches, y= weight)) + ggtitle("Relation of Height and Weight of Players")+xlab("Height in Inches")+ylab("Weight of Players")+xlim(60,95)


## Sixth question - Change in Height with Change in Weight with positions

PlayerMod%>%ggplot()+geom_point(mapping = aes(x = height.TotInches, y= weight))+ geom_smooth(mapping = aes(x = height.TotInches, y= weight, color=position)) + ggtitle("Relation of Height and Weight of Players at different Positions")+xlab("Height in Inches")+ylab("Weight of Players")+xlim(60,95)


## Seventh question - Change in Height with Year

PlayerMod%>%ggplot()+geom_point(mapping = aes(x = year_start, y= height.TotInches))+ geom_smooth(mapping = aes(x = year_start, y= height.TotInches)) + ggtitle("Relation of Height against the start of the playing Year")+xlab("Year")+ylab("Height in Inches")+ylim(60,95)

summary(PlayerMod)

#3D plot of height vs. weight vs. year and color code the points by position

p <- plot_ly(PlayerMod, x = ~weight, y = ~height.TotInches, z = ~year_start, color = ~position) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Weight'),
                      yaxis = list(title = 'Height (Inches)'),
                      zaxis = list(title = 'Start Year')))
p

######NOT WORKING#########
# Source: https://github.com/dgrtwo/gganimate
# install.packages("cowplot")  # a gganimate dependency
# devtools::install_github("dgrtwo/gganimate")
install.packages("cowplot")
install.packages("gganimate")
install.packages("gapminder")
library(ggplot2)
library(gganimate)
library(gapminder)
devtools::install_github('thomasp85/gganimate')
theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(PlayerMod, aes(height.TotInches, weight, size = pop, frame = year_start)) +
  geom_point() +
  geom_smooth(aes(group = year_start), 
              method = "lm", 
              show.legend = FALSE) +
  facet_wrap(~continent, scales = "free") +
  scale_x_log10()  # convert to log scale

gganimate(g, interval=0.2)

library(ggplot2)
theme_set(theme_bw())
###########################################################
### NOT WORKING Plot

ggplot(PlayerMod, aes(x=career, y=position, label=position)) + 
geom_point(stat='identity', aes(col=position, size=6)  +
scale_color_manual(name="Mileage",labels = c("Above Average", "Below Average"), values = c("above"="#00ba38", "below"="#f8766d")) + 
geom_text(color="white", size=2) +
labs(title="Diverging Dot Plot", subtitle="Normalized mileage from 'mtcars': Dotplot") + ylim(-2.5, 2.5) + coord_flip()
  
  
 
# Extra Plot for Career Span over the Position 
theme_set(theme_classic())
career = PlayerMod$year_end - PlayerMod$year_start
  g <- ggplot(PlayerMod, aes(career))
  g + geom_density(aes(fill=factor(position)), alpha=0.8) + 
    labs(title="Density plot", 
         subtitle="Career span by Position",
         caption="Basket Ball Dataset",
         x="Career Length in Years",
         fill="Position")
  
  dev.off()
##scatter plot
plot(PlayerMod$height.TotInches, career, xlim =95)
PlayerMod %>% ggplot(mapping = aes(x = height.TotInches, y= career)) + geom_point()+xlim(60,95)+geom_smooth(mapping = aes(x = height.TotInches, y= career))

## Import a new dataset of Education and Income
Education = read.csv("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 2/Education_Income.csv", header = TRUE)
summary(Education)

plot(Education$Educ, Education$Income2005)
Education %>% ggplot(mapping = aes(x = Educ, y= Income2005)) + geom_point()+geom_smooth(mapping = aes(x = Educ, y= Income2005))

Education%>%ggplot(aes(x = Educ, y= Income2005)) + geom_boxplot(color="blue") + ggtitle("Distribution of Income for the Education")+xlab("Education Level")+ylab("Income")




#########
p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p
  
p + transition_time(year) +
  labs(title = "Year: {frame_time}")

p + facet_wrap(~continent) +
  transition_time(year) +
  labs(title = "Year: {frame_time}")
