# Set the working directory
# Yours will look different than this! 
#setwd("/Users/Quinn/Documents/R/Intro_2R/")

# Load data
streamDOC <- read.csv("streamDOC.csv")

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

# Log transform the data and assign
logDOC <- log(streamDOC$DOC)

# Log transform the data and append to the data frame
streamDOC$logDOC <- logDOC

# Check that the operation was sucessful.
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

