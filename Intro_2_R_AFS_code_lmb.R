#########################################################
#### Intro to R with Largemouth Bass Length-Weight Data
#########################################################

#### Showing off! ####

# Assign a value to an object
a <- 25
# Multiply it by 4
a * 4
# Uh oh...
A * 4
# Note that R is case sensitive!
# a and A are NOT the same object.

#### Creating a working directory ####

# Create a folder on your desktop called "Intro_2_R"
# Put your largemouth bass data file in it
# For example, this file is called "camp_lmb_lw.csv"

# Set the working directory
# Yours will look different than this!
# It may look something like: 
# setwd("/Users/Quinn/Desktop/Intro_2_R/")	
# setwd("~/Desktop/Intro_2_R")

# You can also manually set the working directory using:
# Session -> Set Working Directory -> Choose Directory

# Load the data
camp_lmb_lw <- read.csv("camp_lmb_lw.csv")

# Take a quick look
camp_lmb_lw

#### Explore the data ####

# Get the structure of the data frame
str(camp_lmb_lw)

# Look at the first 6 rows
head(camp_lmb_lw)

# Look at the last 6 rows
tail(camp_lmb_lw)

# Pull out all rows of column 1
camp_lmb_lw[, 1]

# Pull out all rows of column 2
camp_lmb_lw[, 2]

# Pull out rows 1 through 10 of all columns
camp_lmb_lw[1:10, ]

# Summary statistics for each column
summary(camp_lmb_lw)

# Pull out the fish lengths
camp_lmb_lw$length_mm

# Pull out the fish weights
camp_lmb_lw$weight_g

#### Subsetting data ####

# Split the data frame into reference and treatment basins
ref <- camp_lmb_lw[camp_lmb_lw$basin == "ref", ]
treat <- camp_lmb_lw[camp_lmb_lw$basin == "treat", ]

# Split by year too
yr2023 <- camp_lmb_lw[camp_lmb_lw$year == 2023, ]
yr2024 <- camp_lmb_lw[camp_lmb_lw$year == 2024, ]


#### First figures in base R ####

# Base R plotting is built right into R
# It is a nice place to start because the syntax is simple

# Assign x and y
x <- camp_lmb_lw$length_mm
y <- camp_lmb_lw$weight_g

plot(x,y)

# Basic scatterplot, but now with labels and a title! 
plot(x, y,
     main = "Largemouth bass length vs. weight",
     xlab = "Fish length (mm)",
     ylab = "Fish weight (g)")

# Bigger fish tend to weigh more
# This is a very common kind of relationship in fisheries data

# Let's compare fish lengths between basins with a boxplot
boxplot(camp_lmb_lw$length_mm ~ camp_lmb_lw$basin,
        names = c("Reference", "Treatment"),
        main = "Fish length by basin",
        ylab = "Fish length (mm)", 
        xlab = "Basin")

# Let's compare fish weights between basins with a boxplot
boxplot(camp_lmb_lw$weight_g ~ camp_lmb_lw$basin,
        names = c("Reference", "Treatment"),
        main = "Fish weight by basin",
        ylab = "Fish weight (g)", 
        xlab = "Basin")

#### Transform the data ####

# In fisheries, length-weight data are often analyzed on log scales
# because the relationship between length and weight is usually not linear

# Create a log-transformed weight column
camp_lmb_lw$log_weight <- log(camp_lmb_lw$weight_g)

# Create a log-transformed length column
camp_lmb_lw$log_length <- log(camp_lmb_lw$length_mm)

# Make year a factor so as to treat it like a category instead of an integer
camp_lmb_lw$year <- as.factor(camp_lmb_lw$year)

# Make a new basin column with nicer labels for plotting later
camp_lmb_lw$basin_f <- factor(camp_lmb_lw$basin,
                              levels = c("ref", "treat"),
                              labels = c("Reference", "Treatment"))

# Check that the transformation worked
head(camp_lmb_lw)

# Look at the structure again
str(camp_lmb_lw)

# Write new data to data file "camp_lmb_lw_V2"
write.csv(camp_lmb_lw,"camp_lmb_lw_V2.csv")
#Now you have this new data file in your working directory!

#### Now let's load ggplot ####

# ggplot2 is one of the most commonly used plotting packages in R
# You only need to install a package once
# install.packages("ggplot2")

# You need to load it each time you open R
library(ggplot2)


#### ggplot question 1: How are length and weight related in basins and years? ####

ggplot(data = camp_lmb_lw) +
  geom_point(mapping = aes(x = length_mm, y = weight_g)) +
  labs(x = "Fish length (mm)",
       y = "Fish weight (g)",
       title = "Largemouth bass length vs. weight") +
  theme_minimal()

#This is the same plot as above, but we can do so much more in ggplot!

ggplot(data = camp_lmb_lw) +
  geom_point(mapping = aes(x = length_mm, y = weight_g)) +
  labs(x = "Fish length (mm)",
       y = "Fish weight (g)",
       title = "Largemouth bass length vs. weight") +
  facet_wrap(~basin_f) +
  theme_minimal()

ggplot(data = camp_lmb_lw) +
  geom_point(mapping = aes(x = length_mm, y = weight_g)) +
  labs(x = "Fish length (mm)",
       y = "Fish weight (g)",
       title = "Largemouth bass length vs. weight") +
  facet_grid(year ~basin_f) +
  theme_minimal()


#### ggplot question 2: Are fish longer in one basin? ####

ggplot(data = camp_lmb_lw) +
  geom_boxplot(mapping = aes(x = basin_f, y = length_mm, fill = basin_f)) +
  labs(x = "Basin",
       y = "Fish length (mm)",
       fill = "Basin",
       title = "Are fish longer in one basin?") +
  theme_minimal()

# Let's also show the individual fish on top of the boxplot
ggplot(data = camp_lmb_lw) +
  geom_boxplot(mapping = aes(x = basin_f, y = length_mm),
               outlier.shape = NA) +
  geom_jitter(mapping = aes(x = basin_f, y = length_mm, color = basin_f),
              width = 0.15, alpha = 0.6) +
  labs(x = "Basin",
       y = "Fish length (mm)",
       fill = "Basin",
       color = "Basin",
       title = "Fish length by basin with individual fish shown") +
  theme_minimal()


#### ggplot question 3: Are fish longer in one year? ####

ggplot(data = camp_lmb_lw) +
  geom_boxplot(mapping = aes(x = year, y = length_mm, fill = year)) +
  labs(x = "Year",
       y = "Fish length (mm)",
       fill = "Year",
       title = "Are fish longer in one year?") +
  theme_minimal()

#Let's put some individual points on there! 
ggplot(data = camp_lmb_lw) +
  geom_boxplot(mapping = aes(x = year, y = length_mm)) +
  geom_jitter(mapping = aes(x = year, y = length_mm, color = year),
              width = 0.15, alpha = 0.6) +
  labs(x = "Year",
       y = "Fish length (mm)",
       fill = "Year",
       title = "Are fish longer in one year?") +
  theme_minimal()


#### ggplot question 4: Does the length-weight relationship differ by basin? ####

ggplot(data = camp_lmb_lw) +
  geom_point(mapping = aes(x = length_mm, y = weight_g, color = basin_f),
             alpha = 0.7) +
  geom_smooth(mapping = aes(x = length_mm, y = weight_g, color = basin_f),
              method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Fish length (mm)",
       y = "Fish weight (g)",
       color = "Basin",
       title = "Length-weight relationship by basin") +
  theme_minimal()

#Hmm, lets throw a non-linear curve on there instead of a straight line
ggplot(data = camp_lmb_lw) +
  geom_point(mapping = aes(x = length_mm, y = weight_g, color = basin_f),
             alpha = 0.7) +
  geom_smooth(mapping = aes(x = length_mm, y = weight_g, color = basin_f),
              method = "loess", formula = y ~ x, se = TRUE) +
  labs(x = "Fish length (mm)",
       y = "Fish weight (g)",
       color = "Basin",
       title = "Length-weight relationship by basin") +
  theme_minimal()

#### ggplot question 5: What does the log-log relationship look like? ####

# This is often more appropriate biologically for fish length-weight data
ggplot(data = camp_lmb_lw) +
  geom_point(mapping = aes(x = log_length, y = log_weight, color = basin_f),
             alpha = 0.7) +
  geom_smooth(mapping = aes(x = log_length, y = log_weight, color = basin_f),
              method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "log(Fish length)",
       y = "log(Fish weight)",
       color = "Basin",
       title = "Log-log length-weight relationship by basin") +
  theme_minimal()


#### Run some stats ####

# Compare fish lengths between basins
t.test(ref$length_mm, treat$length_mm)

# Compare fish weights between basins
t.test(ref$weight_g, treat$weight_g)


#### Saving a ggplot ####

# Save a biologically meaningful plot using the log-transformed data
ggplot1 <- ggplot(data = camp_lmb_lw) +
  geom_point(mapping = aes(x = log_length, y = log_weight, color = basin_f),
             alpha = 0.7) +
  geom_smooth(mapping = aes(x = log_length, y = log_weight, color = basin_f),
              method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "log(Fish length)",
       y = "log(Fish weight)",
       color = "Basin",
       title = "Log-log length-weight relationship by basin") +
  theme_minimal()

# Display it
ggplot1

# Save using ggsave()
ggsave("bass_log_length_weight_plot.png", plot = ggplot1,
       width = 10, height = 7, dpi = 300)


#### Mapping tangent ####

# There are lots of ways to map data in R
# If you want interactive maps, leaflet is a really fun option!

# install.packages("leaflet")
library(leaflet)

# Example points showing the two basins
lake_points <- data.frame(
  name = c("Reference basin", "Treatment basin"),
  latitude = c(46.001322, 45.998051),
  longitude = c(-89.729636, -89.731758)
)

# Make an interactive map
lake_map <- leaflet() %>%
  addTiles(group = "Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~longitude, ~latitude,
                   popup = ~as.character(name),
                   radius = 5,
                   fillOpacity = 0.9,
                   color = "blue",
                   data = lake_points) %>%
  addScaleBar() %>%
  addLegend(position = "bottomright",
            colors = "blue",
            labels = "Lake Basins") %>%
  addMiniMap(width = 150, height = 150)

lake_map


#### A simple model with two predictors ####

# Let's look at how weight changes with length and basin
model <- lm(log_weight ~ log_length + basin_f, data = camp_lmb_lw)

summary(model)

#Linear models at the simplest forms are just T-tests!

#In this model, we are predicting log(weight) from log(length) and basin. The strongest pattern is the effect of log_length: its estimate is 3.11, and its p-value is less than 2e-16, which tells us there is very strong evidence that heavier fish are associated with greater length on the log scale. In other words, as bass get longer, their weight increases predictably (no duh moment).

#The coefficient for basin_fTreatment is 0.019, which is small, and its p-value is 0.147. That means that after accounting for fish length, we do not have strong evidence that fish in the Treatment basin differ in weight from fish in the Reference basin.

#The R-squared value is 0.949, which means this model explains about 95% of the variation in log-transformed fish weight. That tells us fish length is doing a very good job predicting fish weight in this dataset. The overall model p-value is also extremely small, so the model as a whole explains log(weight) much better than a model with no predictors.


