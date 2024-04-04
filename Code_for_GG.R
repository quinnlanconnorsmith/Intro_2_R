# Load data
streamDOC <- read.csv("streamDOC.csv")

####Transform the Data####

# Log transform the data and assign
logDOC <- log(streamDOC$DOC)

# Log transform the data and append to the data frame
streamDOC$logDOC <- logDOC

# Convert fraction wetland to percent wetland, append to the streamDOC data frame with column name "prcntWet".
streamDOC$prcntWet <- streamDOC$Wet * 100

#install GG plot (you only need to do this once! Like ever!)
install.packages(ggplot2)

#Activate it in R (you need to do this every time you boot up R if you're going to be using any package)
library(ggplot2)

#### ggplot2 and streamDOC ####

ggplot1 <- ggplot(data = streamDOC, aes( x = prcntWet, y=logDOC, fill=Type)) +
  geom_point(aes(color=factor(Type)))+
  stat_smooth(aes(color=factor(Type)),    
              method=lm,formula = y ~ x, level=0.95)+
  guides(color="none")+
  labs(x='Percent Wetland',y='log(Stream DOC)')


#Plot it 
ggplot1

#Now save it! 
tiff("ggplot1.tiff", units="in", width=12, height=8, res=600)
ggplot1
dev.off()