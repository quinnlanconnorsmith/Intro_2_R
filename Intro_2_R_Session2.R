# First clear your work environment
rm(list = ls())

# Set the working directory
#setwd("/Users/Quinn/Documents/R/Intro_2R/")

# Run your script
#source("Intro_2_R_Session1.R")

# List objects in workspace
ls()

#install GG plot (you only need to do this once! Like ever!)
install.packages(ggplot2)

#Activate it in R (you need to do this everytime you boot up R if you're going to be using any package)
library(ggplot2)

ggplot1 <- ggplot(data = streamDOC, aes( x = prcntWet, y=logDOC, fill=Type)) +
  geom_point(aes(color=factor(Type)))+
  stat_smooth(aes(color=factor(Type)),    
              method=lm,formula = y ~ x, level=0.95)+
  guides(color="none")+
  labs(x='Percent Wetland',y='log(Stream DOC)')


#Plot it 
ggplot1

#Let's take a look at a dataframe already present in R 
mpg
?mpg

#Let's plot some data! 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Look at those funny points that have high engine size but also higher MPG
#Hypothesize about why those may be have higher MPG. What can we do to test this hypothesis? 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Let's make the points relative to the size of the cars 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class)) +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Other ways to play around with this:

#Transparency 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class)) +
  labs(x='Engine size (liters)',y='Miles per gallon')
#oof, this one isn't super helpful 

#Different shapes
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))+
  labs(x='Engine size (liters)',y='Miles per gallon')
#Uh oh! R won't plot over 6 different shapes unless you manually input this 

#Let's look at another way to convey these data 
#Using facets, wrap by class of vehicle 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2) +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Let's double facet
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl) +
  labs(x='Engine size (liters)',y='Miles per gallon')
#There's a lot of numbers getting thrown around here, let's make things nicer 
drv_labs <- c("4-wheel", "front-wheel", "rear-wheel")
names(drv_labs) <- c("4", "f", "r")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl, labeller = labeller(drv = drv_labs)) +
  labs(x='Engine size (liters)',y='Miles per gallon') 

#Different geoms
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x='Engine size (liters)',y='Miles per gallon') 

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  labs(x='Engine size (liters)',y='Miles per gallon') 

#Setting the line type of a geom_smooth argument
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv)) +
  labs(x='Engine size (liters)',y='Miles per gallon') 

#Let's change this to drive train by color
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv)) +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Now let's overlay some points
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Now let's shift into MAXIMUM OVERDRIVE
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Overall trend with class 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth() +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Last thing with this dataset, lets look at box plots 
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  labs(x='Miles per gallon',y='Class of vehicle')


ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y='Miles per gallon',x='Class of vehicle')

#There's so much you can do with ggplot and R as a whole, this has served as a brief introduction to some of the basics
#I hope this has been helpful! -Q


