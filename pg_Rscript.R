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
library(purrr)
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
#converting the type of bnpoints to factor

#extract year from title
data$year <- data$title
data$year <- str_replace_all(data$year, "[[:punct:]]", " ")
data$year <- str_extract(data$year,"[0-9]{4}") #extracting year with four digits 

#omiting abnormal year
data <- data %>%
  filter(year > 1900)
#converting the type of year from chr. to intger
data$year <- as.integer(data$year)

#omitting the observations with missing variables 
data <- na.omit(data)
str(data)

#initial data analysis
#The boxplot of points against price
ggplot(data = data, aes(x = as.factor(bnpoints), y = price, group = bnpoints)) +
  geom_boxplot(fill = c("pink", "yellow")) +
  labs(x = "bnpoints", y = "price", title = "The boxplot of price grouped by points",
       axis = c("good", "Excellent"))+ 
  theme(legend.position = "none") 

#The boxplot of points against year
ggplot(data = data, aes(x = as.factor(bnpoints), y = year, group = bnpoints)) +
  geom_boxplot(fill = c("blue","orange")) +
  labs(x = "bnpoints", y = "year", title = "The boxplot of year grouped by points") +
  theme(legend.position = "none") 

#The pie chart
ggplot(data = data, aes(x = factor(1), fill = country))+
  geom_bar(width = 2)+
  coord_polar("y")

#the model with only price and year
model <- glm(as.factor(bnpoints) ~ price+year,
             data=data, family=binomial(link = "logit"))
summary(model)
summ(model)
mod1coefs <- round(coef(model), 2)

#rmd
# confint(model) %>%
#   kable()


#Log-odds
mod.coef.logodds <- model %>%
  summary() %>%
  coef()
price.logodds.lower <- mod.coef.logodds["price", "Estimate"] - 
  1.96 * mod.coef.logodds["price", "Std. Error"]
price.logodds.upper <- mod.coef.logodds["price", "Estimate"] + 
  1.96 * mod.coef.logodds["price", "Std. Error"]

plot_model(model, show.values = TRUE, transform = NULL,
           title = "Log-Odds (instructor)", show.p = FALSE)

data.price <- data.price %>%
  mutate(logodds = predict(model))

#Odds
price.odds.lower <- exp(price.logodds.lower)
price.odds.upper <- exp(price.logodds.upper)

plot_model(model, show.values = TRUE, axis.lim = c(1,1.5),
           title = "Odds (instructor)", show.p = FALSE)

data.price <- data.price %>%
  mutate(odds = exp(logodds))

#Probabilities
p.num <- exp(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["price", "Estimate"] * 52)
p.denom <- 1 + p.num
p.num / p.denom
plogis(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["price", "Estimate"] * 52)

data.price <- data.price %>%
  mutate(probs = fitted(model))

ggplot(data = data.price, aes(x = price, y = probs)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "price", y = "Probability of instructor being '1'")










#myfit -- another model
myfit <- glm(bnpoints ~ country+price+year,
             data=data, family=binomial(link = "logit"))
#Log-odds
mod.coef.logodds2 <- myfit %>%
  summary() %>%
  coef()

#analysis of deviance table
anova(myfit, test = "Chi")

#confidence interval  ??
confint(myfit)
#ROC curve
library(pROC)
roc_(data, model$fitted.values, plot = TRUE)

#price.logodds.lower <- mod.coef.logodds["price", "Estimate"] - 
#  1.96 * mod.coef.logodds["price", "Std. Error"]
#price.logodds.upper <- mod.coef.logodds["price", "Estimate"] + 
#  1.96 * mod.coef.logodds["price", "Std. Error"]

#estimating the parameter
plot_model(myfit, show.values = TRUE, transform = NULL,
           title = "Log-Odds (instructor)", show.p = FALSE)

data.fitted <- data %>%
  select(bnpoints, country, price, year) %>%
  mutate(logodds = predict(myfit))

#Odds
#price.odds.lower <- exp(price.logodds.lower)
#price.odds.upper <- exp(price.logodds.upper)

plot_model(myfit, show.values = TRUE, axis.lim = c(1,1.5),
           title = "Odds (instructor)", show.p = FALSE)

data.fitted <- data.fitted %>%
  mutate(odds = exp(logodds))

#Probabilities
#p.num <- exp(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["price", "Estimate"] * 52)
#p.denom <- 1 + p.num
#p.num / p.denom
#plogis(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["price", "Estimate"] * 52)

#the probability plot 
#price against probability
data.fitted <- data.fitted %>%
  mutate(probs = fitted(myfit))

ggplot(data = data.fitted, aes(x = price, y = probs)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "price", y = "Probability of instructor being '1'")


#year against probability
ggplot(data = data.fitted, aes(x = year, y = probs)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "year", y = "Probability of instructor being '1'")

#
summary(myfit)

predict(myfit, type = "response")

qchisq(p = 0.95, df = 1791)
