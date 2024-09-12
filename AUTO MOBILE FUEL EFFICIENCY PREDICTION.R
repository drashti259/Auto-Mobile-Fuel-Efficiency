#Install Libraries
install.packages("tidyverse")
install.packages("ISLR2")
install.packages("magrittr")
install.packages("gridExtra")
install.packages("caret")
install.packages("leaps")
install.packages("vctrs")


#Call Libraries
library(tidyverse)
library(ISLR2)
library(ggplot2)
library(magrittr)
library(gridExtra)
library(caret)
library(leaps)


autompgdata <- autompg

colnames(autompgdata) <- c("MPG", "Cylinders","Displacement","Horsepower","Weight","Acceleration","Model Year","Origin","Model Names")
print(autompgdata)

#SUMMARY
summary(autompgdata)

#CLEANING THE DATA

#Replacing ? with NA
autompgdata[autompgdata == '?'] <- NA

#Finding missing values in column
is.na(autompgdata)

#Total missing Values
colSums(is.na(autompgdata))

#Identifying the Missing Values
which(is.na(autompgdata))

#Omitting rows Containing NA
autompgdata1<-na.omit(autompgdata)

#Total missing Values
colSums(is.na(autompgdata1))

#Displaying the datatype of the column
str(autompgdata1)

#Converting all to numeric
autompgdata1$Cylinders <- as.numeric(autompgdata1$Cylinders)
autompgdata1$Horsepower <- as.numeric(autompgdata1$Horsepower)
autompgdata1$`Model Year` <- as.numeric(autompgdata1$`Model Year`)

#SUMMARY STATISTICS
autompgdata1 %>% ggplot(aes(MPG)) + geom_histogram()
autompgdata1 %>% ggplot(aes(Cylinders)) + geom_histogram()
autompgdata1 %>% ggplot(aes(Displacement)) + geom_histogram()
autompgdata1 %>% ggplot(aes(Horsepower)) + geom_histogram()
autompgdata1 %>% ggplot(aes(Weight)) + geom_histogram()
autompgdata1 %>% ggplot(aes(Acceleration)) + geom_histogram()

#CORRELATION BETWEEN VARIABLES
cor(autompgdata1[,c("MPG", "Cylinders", "Displacement", "Horsepower", "Weight", "Acceleration", "Model Year")])

#SCATTERPLOT
autompgdata1 %>% ggplot(aes(x = Cylinders, y = MPG)) + geom_point() + geom_smooth(method="lm", se=FALSE)
autompgdata1 %>% ggplot(aes(x = Displacement, y = MPG)) + geom_point() + geom_smooth(method="lm", se=FALSE)
autompgdata1 %>% ggplot(aes(x = Horsepower, y = MPG)) + geom_point() + geom_smooth(method="lm", se=FALSE)
autompgdata1 %>% ggplot(aes(x = Weight, y = MPG)) + geom_point() + geom_smooth(method="lm", se=FALSE)
autompgdata1 %>% ggplot(aes(x = Acceleration, y = MPG)) + geom_point() + geom_smooth(method="lm", se=FALSE)
autompgdata1 %>% ggplot(aes(x = `Model Year`, y = MPG)) + geom_point() + geom_smooth(method="lm", se=FALSE)

#CO-EFFICIENTS
model <- lm(MPG ~ Cylinders + Displacement + Horsepower + Weight + Acceleration, data = autompgdata1)
coef(model)

#BOXPLOT
p1 <- autompgdata1 %>% ggplot(aes(x = Cylinders, y = MPG)) + geom_boxplot() + geom_jitter()
p2 <- autompgdata1 %>% ggplot(aes(x = Displacement, y = MPG)) + geom_boxplot() + geom_jitter()
p3 <- autompgdata1 %>% ggplot(aes(x = Horsepower, y = MPG)) + geom_boxplot() + geom_jitter()
p4 <- autompgdata1 %>% ggplot(aes(x = Weight, y = MPG)) + geom_boxplot() + geom_jitter()
p5 <- autompgdata1 %>% ggplot(aes(x = Acceleration, y = MPG)) + geom_boxplot() + geom_jitter()
p6 <- autompgdata1 %>% ggplot(aes(x = `Model Year`, y = MPG)) + geom_boxplot() + geom_jitter()
p7 <- autompgdata1 %>% ggplot(aes(x = Origin, y = MPG)) + geom_boxplot() + geom_jitter()

grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 2)

#RESIDUAL ANALYSIS
fullModel <- lm(MPG ~ Cylinders + Displacement + Horsepower + Weight + Acceleration, data = autompgdata1)
fullModelResids <- fullModel$residuals
fullModelFitted <- fullModel$fitted.values

hist(fullModelResids)
qqnorm(fullModelResids)
plot(fullModelFitted, fullModelResids)

#CROSS VALIDATION
largeautoModel <- lm(MPG ~ Cylinders + Displacement + Horsepower + Weight + Acceleration, data = autompgdata1)
summary(largeautoModel)

set.seed(10)

largeCVModel <- train(
  form = MPG ~ Cylinders + Displacement + Horsepower + Weight + Acceleration,
  data = autompgdata1,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
largeCVModel

#sUBSET
subsetautomodel <- regsubsets(MPG ~ Cylinders + Displacement + Horsepower + Weight + Acceleration, data = autompgdata1)
summary(subsetautomodel)
plot(subsetautomodel, scale = "adjr2")

subsetCVModel <- train(
  form = MPG ~ Cylinders + Displacement + Horsepower + Weight + Acceleration,
  data = autompgdata1,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
subsetCVModel

#BEST MODEL
bestModel <- lm(MPG ~ Cylinders + Horsepower + Weight , data = autompgdata1)
coef(bestModel)
bestModelResids <- bestModel$residuals
bestModelFitted <- bestModel$fitted.values
plot(bestModelFitted, bestModelResids)


