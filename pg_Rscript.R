#package
library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

#import the data
data <- read.csv("dataset26.csv")
data <- data[,2:8]  #deleting the first line

# glimpse data 
# Encoding(data[1,7])<-'ASCII' 
# data[1,7]
# guess_encoding(data[,7])
glimpse(data)
str(data)

# data processing 
# trans to binary
data$bnpoints[data$points > 90] <- 1 #binary 
data$bnpoints[data$points <= 90] <- 0

#extract year from title
data$year <- data$title
data$year <- str_replace_all(data$year, "[[:punct:]]", " ")
data$year <- str_extract(data$year,"[0-9]{4}") #extracting year with four digits 

#omiting abnormal year
data <- data %>%
  filter(year > 1900)

#omitting the observations with missing variables 
data <- na.omit(data)

#initial data analysis
#The boxplot of points against price
ggplot(data = data, aes(x = as.factor(bnpoints), y = price, group = bnpoints)) +
  geom_boxplot(fill = c("pink", "yellow")) +
  labs(x = "bnpoints", y = "price", title = "The boxplot of price grouped by points",
       axis = c("good", "Excellent"))+ 
  theme(legend.position = "none") 

exp(-2.61)

#The boxplot of points against year
ggplot(data = data, aes(x = as.factor(bnpoints), y = year, group = bnpoints)) +
  geom_boxplot(fill = c("blue","orange")) +
  labs(x = "bnpoints", y = "year", title = "The boxplot of year grouped by points") +
  theme(legend.position = "none") 

#The pie chart
ggplot(data = data, aes(x = factor(1), fill = country))+
  geom_bar(width = 2)+
  coord_polar("y")


#fitting the genera
myfit <- glm(bnpoints ~ country + price + year, data=data, family=binomial(link = "logit"))

summary(myfit)

predict(myfit, type = "response")

qchisq(p = 0.95, df = 1791)

