### Linear regression Model Projec - Exploratory Data Analysis (EDA).
bike <- read.csv(file.choose(), header = T)
head(bike)
# We are trying to predict the total count of bikes rented during each hour covered by the test set.
## We are going to generate a scatter plot of count vs temperature with an alpha value of 0.5
require(ggplot2)
require(ggthemes)
require(dplyr)
ggplot(bike,aes(temp, count)) + geom_point(alpha=0.3,aes(color=temp)) + theme_few()

# Let's convert the datime column to POSIXct 
bike$datetime <- as.POSIXct(bike$datetime)
g <- ggplot(bike, aes(datetime, count)) + geom_point(alpha=0.5, aes(color=temp))
g + scale_color_continuous(low='#55D8CE',high='#FF6E2E') + theme_few()
# The correlation btwn temp and count can be determined as below,
cor(bike[,c('temp','count')])
# Creating a boxplot of count vs seasons (each season starting with a new box)
ggplot(bike, aes(factor(season), count)) + geom_boxplot(aes(color=factor(season))) + theme_few()
# Feature Engineering 
bike$hour <- sapply(bike$datetime, function(x) {format(x,"%H")})
print(head(bike))
# Scatterplot of count versus hour with plot only depicting data from 'workingday = 1'.
pl <- ggplot(filter(bike, workingday==1), aes(hour,count)) + geom_point(aes(color=temp), alpha=0.3, position = position_jitter(w=1,h=0))
pl <- pl + scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_few()
## Now that we are done exploring our data, let's start building the model.
# Based on the fact that our data has a growing trend and some seasonality to it, linear regression model would not be our best machine learnig algorithm. 
# Thus, we do not need to spilt our dataset into a training and testing set because if we predict the train set, we'd be predicting at points squeezed between two time points that we already have training data for.
temp.model <- lm(count ~ temp, bike)
print(summary(temp.model))
# let's predict the amount of bikes utilized at temperature equal 25 degrees.
temp.test <- data.frame(temp=c(25))
temp.test
predict(temp.model,temp.test)
# Manually, line 34 is 9.17*25 + 6.0462, where 9.17 is the slope of the line and 6.0462 is the y-intercept as highlighted by the model summary.
## Let's build another model which accounts for all independent variables.
bike.model <- lm(count ~. - casual - registered - datetime - atemp, bike)
print(summary(bike.model))
