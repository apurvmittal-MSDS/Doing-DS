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

