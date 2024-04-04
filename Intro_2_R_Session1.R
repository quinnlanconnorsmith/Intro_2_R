####Session 1 Begin####

#Showing off! 
a <- 25
a*4
A*4
#Note that R is case sensitive! 

####Creating a working directory####

#Create a folder on your desktop called "Intro_2_R" and put streamDOC.csv in it

# Set the working directory
# Yours will look different than this! 
#setwd("/Users/Quinn/Documents/R/Intro_2_R/")
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
