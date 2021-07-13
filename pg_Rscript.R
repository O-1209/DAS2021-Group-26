library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(dplyr)
library(stringr)
data <- read.csv("dataset26.csv")
glimpse(data)
data[731,]
extract(data[-731,], "title","(\\d)", convert = TRUE) #select year 
guess_encoding(data[,6])

#boxplot by variety
data. <- evals %>%
  select(gender, age)
ggplot(data = evals.gender, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Age")+ 
  theme(legend.position = "none")



data$ynaffair[Affairs$affairs > 0] <- 1
data$ynaffair[Affairs$affairs== 0] <- 0
data$ynaffair <-factor(Affairs$ynaffair,levels=c(0,1),labels=c("No","Yes"))
variety <- length(unique(data$variety))
country <- length(unique(data$country))
year #receive from title
glm(points~country+price+province+year+variety+winery, 
    family=binomial(link='logit'),data=data) 
