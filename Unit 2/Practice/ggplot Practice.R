# MPG dataset
summary(mpg)
#converting the drv to factor from characters
mpg$drv=as.factor(mpg$drv)
#converting the drv to factor from characters
mpg$class=as.factor(mpg$class)
#Scatterplot
mpg%>%ggplot(mapping = aes(x=hwy, y=cty))+geom_point()

#Scatterplot with smooth line
mpg%>%ggplot(mapping = aes(x=hwy, y=cty))+geom_smooth()

#Scatterplot with smooth line for different value of Drive
mpg%>%ggplot(mapping = aes(x=hwy, y=cty, linetype=drv, color = drv))+geom_smooth()

#Scatterplot with smooth line for different value of Drive for both points and line
mpg%>%ggplot(mapping = aes(x=hwy, y=cty, linetype=drv, color = drv))+geom_smooth()+geom_point()

#Scatterplot with smooth line for different value of Class for both points and line and line by drive as well
mpg%>%ggplot(mapping = aes(x=hwy, y=cty, linetype=drv, color = class))+geom_smooth()+geom_point()

#DIFFERENT WAY - Scatterplot with smooth line for different value of Class for both points and line and line by drive as well
mpg%>%ggplot()+
geom_point(mapping = aes(x=hwy, y=cty, color = class)) +
geom_smooth(mapping = aes(x=hwy, y=cty, linetype=drv, color=drv))

#DIFFERENT WAY - FACET_Wrap
mpg%>%ggplot()+
  geom_point(mapping = aes(x=hwy, y=cty, color = class)) +
  geom_smooth(mapping = aes(x=hwy, y=cty, linetype=drv, color=drv))+
  facet_wrap(~class)

#DIFFERENT WAY - FACET_grid
mpg%>%ggplot()+
  geom_point(mapping = aes(x=hwy, y=cty, color = class)) +
  geom_smooth(mapping = aes(x=hwy, y=cty, linetype=drv, color=drv))+
  facet_grid(drv~class)

## Transformations




## MAPS

install.packages("maps")
library(maps)
?map_data

##  Themes

install.packages("ggthemes")
library(ggthemes)

mpg%>%ggplot()+
  geom_point(mapping = aes(x=hwy, y=cty, color = class)) +
  geom_smooth(mapping = aes(x=hwy, y=cty, linetype=drv, color=drv))+theme_wsj()