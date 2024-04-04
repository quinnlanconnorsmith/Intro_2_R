#Intro to R - UW AFS 4/4/2024

#Showing off! 
a <- 25
a*4
A*4
#Note that R is case sensitive! 

####Creating a working directory####

#Create a folder on your desktop called "Intro_2_R" and put streamDOC.csv in it

# Set the working directory
# Yours will look different than this! 
#setwd("/Users/Quinn/Desktop/R/Intro_2_R/")
#You can also manually set the working directory using the session tab above (this might be easier)

# Load data
streamDOC <- read.csv("streamDOC.csv")

####Explore the Data####

# Get the structure of the data frame
str(streamDOC)

# Look at the first 6 rows
head(streamDOC)

# Look at the last 6 rows
tail(streamDOC)

# Summary statistics
summary(streamDOC)

# Get all rows of column 1
streamDOC[ ,1]

# Get all rows of columns 2
streamDOC[ ,2]

# Get rows 1-7 of all columns
streamDOC[1:7, ]

# Get column names
colnames(streamDOC)

# Get observations from column "Type"
streamDOC$Type

# Get "Wet"
streamDOC$Wet

# Get "DOC"
streamDOC$DOC

####Transform the Data####

# Log transform the data and assign
logDOC <- log(streamDOC$DOC)

# Log transform the data and append to the data frame
streamDOC$logDOC <- logDOC

# Check that the operation was successful.
head(streamDOC)

# Convert fraction wetland to percent wetland, append to the streamDOC data frame with column name "prcntWet".
streamDOC$prcntWet <- streamDOC$Wet * 100

# Did it work?
head(streamDOC)

# Split the streamDOC data frame into observations of types Lake, and No Lake.
lake <- streamDOC[streamDOC$Type == "Lake", ]
no.lake <- streamDOC[streamDOC$Type == "No_Lake", ]

# Write streamDOC to data file "streamDOC_V2"
write.csv(streamDOC,"streamDOC_V2.csv")

####Plot the Data####

# Assign values to x and y
x <- streamDOC$Wet
y <- streamDOC$logDOC

# Plot y vs. x
plot(x, y)

# Add a title and axes labels
plot(x, y, main = "Stream DOC as Function of Wetland Extent",
     xlab = "Wetland Extent (fraction)", ylab = "log(DOC) (mg/L)")

# Reassign x as percent wetland extent
#Note that this overwrites the previous x variable! 
x <- streamDOC$prcntWet

# Plot and change the title and x-label
plot(x, y, main = "Stream DOC as Function of Wetland Extent",
     xlab = "Wetland Extent (%)", ylab = "log(DOC) (mg/L)")

# Run a linear regression
reg1 <- lm(y ~ x)

# Get the output summary of the linear regression
summary(reg1)

# Replot
plot(x, y, main = "Stream DOC as Function of Wetland Extent",
     xlab = "Wetland Extent (%)", ylab = "log(DOC) (mg/L)")

# Add the regression line to the plot
abline(reg1)

# Plot data with an upstream lake
x <- lake$prcntWet
y <- lake$logDOC
plot(x, y, pch = 16, col = "black", main = "Stream DOC as Function of Wetland Extent",
     xlab = "Wetland Extent (%)", ylab = "log(DOC) (mg/L)")

# Add points for data with no upstream lake
x <- no.lake$prcntWet
y <- no.lake$logDOC
points(x, y, pch = 17, col = "grey")

# Add a legend
legend("topleft", legend = c("Lakes", "No Lakes"), pch = c(16, 17), col = c("black", "grey"))

# Assign data.
dat <- c( mean(lake$DOC), mean(no.lake$DOC))

# Assign data names.
dat.names <- c( "Lake", "No Lake")

# Remove NA values and assign data
dat <- c( mean( na.omit(lake$DOC) ), mean(no.lake$DOC))

# Plot data and assign labels
barplot(height = dat, names.arg = dat.names, main = "Stream DOC, With and Without an Upstream Lake",
        ylab = "DOC (mg/L)")

# Plot data and assign labels
boxplot(streamDOC$DOC ~ streamDOC$Type, names = c ("Lake", "No Lake"),
        main = "Stream DOC, With and Without an Upstream Lake", ylab = "DOC (mg/L)")

####Run some stats!####

# Assign values to x and y
dist1 <- lake$DOC
dist2 <- no.lake$DOC

# Run the t-test
t.test(dist1, dist2)

####Let's load up ggplot####

#install GG plot (you only need to do this once! Like ever!)
install.packages("ggplot2")

#Activate it in R (you need to do this every time you boot up R if you're going to be using any package)
library(ggplot2)

#### ggplot2 and streamDOC ####

ggplot1 <- ggplot(data = streamDOC, aes( x = prcntWet, y=logDOC, fill=Type)) +
  geom_point(aes(color=factor(Type)))+
  geom_smooth(aes(color=factor(Type)),    
              method=lm,formula = y ~ x, level=0.95)+
  guides(color="none")+
  labs(x='Percent Wetland',y='log(Stream DOC)')


#Plot it 
ggplot1

#Now save it! This will save to your working directory!
tiff("ggplot1.tiff", units="in", width=12, height=8, res=600)
ggplot1
dev.off()

####Mapping tangent####

#There are lots of ways to map data, but if you want to do it in R, I love leaflet! 

# Install and load the required packages
install.packages("leaflet")
library(leaflet)

#Let's make a dataset! 

madison_points <- data.frame(name=c("Center for Limnology", "Capitol", "Genna's"),
                    latitude = c(43.0772986, 43.0750041, 43.0727399),
                     longitude = c(-89.4030087, -89.3845470, -89.3844072))

madison_map <- leaflet() %>% 
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~longitude, ~latitude, popup = ~as.character(name), radius=3, fillOpacity=0.9, color="blue",data = madison_points) %>%
  addScaleBar() %>%
  addLegend( # Add legend
    position = "bottomright",
    colors = "blue",
    labels = "Places we need to go"
  ) %>%
  addMiniMap(width = 150, height = 150)
madison_map

#You can also remove the provider tiles if you don't want satellite images! 

madison_map_1 <- leaflet() %>% 
  addTiles(group="Map") %>%
  #addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~longitude, ~latitude, popup = ~as.character(name), radius=3, fillOpacity=0.9, color="blue",data = madison_points) %>%
  addScaleBar() %>%
  addLegend( # Add legend
    position = "bottomright",
    colors = "blue",
    labels = "Places we need to go"
  ) %>%
  addMiniMap(width = 150, height = 150)
madison_map_1

####The mpg dataframe####

#Let's take a look at a dataframe already present in R 
mpg
?mpg

#Let's plot some data! 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Look at those funny points that have high engine size but also higher MPG
#Hypothesize about why those may be have higher MPG. What can we do to test this hypothesis? 
#
#
#
#
#
#
#
#
#
#
#
#
#
#Spoilers for the above question! 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  labs(x='Engine size (liters)',y='Miles per gallon')

#Use your own colors!
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  labs(x='Engine size (liters)',y='Miles per gallon') +
  scale_color_manual(breaks = c("2seater", "compact", "midsize", "minivan", "pickup", "subcompact", "suv"),
                     values=c("red", "blue", "green","orange", "violet", "cyan", "pink"))

#Get fancy with RcolorBrewer
install.packages("RColorBrewer")
library(RColorBrewer)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  labs(x='Engine size (liters)',y='Miles per gallon') +
  scale_color_brewer(palette="Dark2")

#End color tangent 

#Let's make the points relative to the size of the cars 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class)) +
  labs(x='Engine size (liters)',y='Miles per gallon')
#This doesn't look super great

#Other ways to play around with this:

#Transparency 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class)) +
  labs(x='Engine size (liters)',y='Miles per gallon')
#oof, this one isn't super helpful either

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

#There's a lot of numbers getting thrown around here, let's make things nicer and label our drive facet
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

#Instead of points, let's make this an average
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

#Flip that axis (if you'd like)
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y='Miles per gallon',x='Class of vehicle')

#There's so much you can do with ggplot and R as a whole, this has served as a brief introduction to some of the basics
#I hope this has been helpful! -Q

#Challenge mode - Answer the following questions using ggplot and the mpg dataset: 

#  Hint: We’ve done enough to plot these two together, but you may have to do some googling to subset data in ggplot2! 

#  Which manufacturer had the highest average highway mpg in 1999? What about 2008? 

#  Is there a way to plot each of these on their own and then together? 
#Spoilers below 
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#Challenge question - both 
ggplot(data = mpg, mapping = aes(x = manufacturer, y = hwy)) + 
  geom_boxplot() +
  facet_wrap(~year) + 
  coord_flip() +
  labs(y='Miles per gallon (highway)',x='Manufacturer')

#Separate 
ggplot(subset(mpg, year == "2008"), mapping = aes(x = manufacturer, y = hwy)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y='Miles per gallon (highway)',x='Manufacturer')

ggplot(subset(mpg, year == "1999"), mapping = aes(x = manufacturer, y = hwy)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y='Miles per gallon (highway)',x='Manufacturer')

#You can make sure the data you plotted matches up! 

summary(subset(mpg, year =="1999"))
summary(subset(mpg, year =="2008"))

