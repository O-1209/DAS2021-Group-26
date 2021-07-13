library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(dplyr)
library(stringr)
data <- read.csv("dataset26.csv")
# Encoding(data[1,7])<-'ASCII' 
# data[1,7]
# guess_encoding(data[,7])
glimpse(data)

# trans to binary
data$bnpoints[data$points > 90] <- 1 #binary 
data$bnpoints[data$points <= 90] <- 0

#extract year from title
data$year <- data$title
data$year <- str_replace_all(data$year, "[[:punct:]]", " ")
data$year <- parse_number(data$year)

#omit
data <- na.omit(data)

#boxplot 
data.price <- data %>%
  select(bnpoints, price)

ggplot(data = data.price, aes(x = bnpoints, y = price, group = bnpoints)) +
  geom_boxplot() +
  labs(x = "bnpoints", y = "price")+ 
  theme(legend.position = "none") 

data.year <- data %>%
  select(bnpoints, year)

ggplot(data = data.year, aes(x = bnpoints, y = year, group = bnpoints)) +
  geom_boxplot() +
  labs(x = "bnpoints", y = "year")+ 
  theme(legend.position = "none") 


#myfit
  myfit <- glm(newcol ~ country+price+province+year+variety+winery,
               data=data, family=binomial())



