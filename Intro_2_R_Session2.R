# First clear your work environment
rm(list = ls())

# Set the working directory
setwd("/Users/Quinn/Documents/R/Intro_2R/")

# Run your script
source("Session1.R")

# List objects in workspace
ls()

# Assign values to x and y
x <- streamDOC$Wet
y <- streamDOC$logDOC

# Plot y vs. x
plot(x, y)

# Add a title and axes labels
plot(x, y, main = "Stream DOC as Function of Wetland Extent",
     xlab = "Wetland Extent (fraction)", ylab = "log(DOC) (mg/L)")

# Reassign x as percent wetland extent
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

# Assign values to x and y
dist1 <- lake$DOC
dist2 <- no.lake$DOC

# Run the t-test
t.test(dist1, dist2)

# Assign average values of CWH relative to high and low landscape position
low = c(82.5, 13.93) # Low landscape
high = c(36.07, 20.36) # High landscape

# Bind data along columns
dat = cbind(low, high)

# Name data
dat.names = c("Low", "High")

# Build barplot
barplot(height = dat, names.arg = dat.names, 
        ylab = "CWH (logs/km)", xlab = "Landscape Position", 
        legend.text = c("Low Development", "High Development"))

# Rebuild barplot
barplot(height = dat, names.arg = dat.names, beside = TRUE, ylim = c(0, 100), 
        ylab = "# logs/km", xlab = "Landscape Position", 
        legend.text = c("Low Development", "High Development"))

# Read in the data
fishCR <- read.csv("fishCR.csv")

# Look at the data structure
str(fishCR)

# Assign values to x and y for smelt data
x <- fishCR$Year
y <- fishCR$RS_per_year

# Plot smelt time series
plot(x, y, type = "l", col = "blue", main = "Crystal Lake Smelt and Perch Populations",
     ylab = "Catch (#)", xlab = "Year")

# Assign values to x and y for perch data
x <- fishCR$Year
y <- fishCR$YP_per_year

# Add the perch line series
lines(fishCR$Year, fishCR$YP_per_year, col = "red")

# Add a legend.
legend("topleft", legend = c("Rainbow Smelt", "Yellow Perch"), 
       col = c("blue","red"), lty = 1, bty = "n")

#install GG plot (you only need to do this once! Like ever! )
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

#There's so much you can do with ggplot and R as a whole, this has served as a brief introduction to some of the basics
#I hope this has been helpful! -Q


