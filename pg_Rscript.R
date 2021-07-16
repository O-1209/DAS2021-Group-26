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
data$bnpoints <- as.factor(data$bnpoints)
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
ggplot(data = data, aes(x = bnpoints, y = price, group = bnpoints)) +
  geom_boxplot(fill = c("pink", "yellow")) +
  labs(x = "bnpoints", y = "price", title = "The boxplot of price grouped by points",
       axis = c("good", "Excellent"))+ 
  theme(legend.position = "none") 

ggplot(data = data, aes(x = price, y = bnpoints)) +
  geom_point(position = position_jitter())

#The boxplot of points against year
ggplot(data = data, aes(x = bnpoints, y = year, group = bnpoints)) +
  geom_boxplot(fill = c("blue","orange")) +
  labs(x = "bnpoints", y = "year", title = "The boxplot of year grouped by points") +
  theme(legend.position = "none") 

ggplot(data = data, aes(x = year, y = bnpoints)) +
  geom_point(position = position_jitter())

#The pie chart
ggplot(data = data, aes(x = factor(1), fill = country))+
  geom_bar(width = 2)+
  coord_polar("y")

#myfit -- another model
myfit <- glm(bnpoints ~ country + price + year,
             data=data, family=binomial(link = "logit"))
#Log-odds
mod.coef.logodds2 <- myfit %>%
  summary() %>%
  coef()

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

plot_model(myfit, show.values = TRUE, axis.lim = c(1,3),
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

#ROC curve
#goodness of the model
#model of bnpoints ~ country + price + year 
library(ROCR)
pro.country <- predict(myfit, data, type = "response")
predict.country <- prediction(pro.country, data$bnpoints)
per.country <- performance(predict.country, measure = "tpr", x.measure = "fpr")
plot(per.country, col = "blue")

#outputting the AUC
auc.country <- performance(predict.country, measure = "auc")
auc.country@y.values

#Inference 
#analysis of deviance table
anova(myfit, test = "Chi")
#qchisq(p = 0.95, df = 1791)
#the p-value of the price is not significant, trying to fit a model omitting year variables

#the model2: bnpoints ~ price + country
model2 <- glm(bnpoints ~ country + price, data = data, family = binomial(link = "logit"))
summ(model2)

#showing the coefficient
mod2coefs <- round(coef(model2), 2)
mod2coefs

#Log-odds
mod.coef.logodds2 <- model2 %>%
  summary() %>%
  coef()

noyear.logodds.lower <- mod.coef.logodds2["price", "Estimate"] - 
  1.96 * mod.coef.logodds2["price", "Std. Error"]
noyear.logodds.upper <- mod.coef.logodds2["price", "Estimate"] + 
  1.96 * mod.coef.logodds2["price", "Std. Error"]
noyear.logodds.lower
noyear.logodds.upper

plot_model(model2, show.values = TRUE, transform = NULL,
           title = "Log-Odds (instructor)", show.p = FALSE)

data.noyear <- data %>%
  select(bnpoints, country, price) %>%
  mutate(logodds = predict(model))

#Odds
#confidence inteval for price
noyear.odds.lower <- exp(noyear.logodds.lower)
noyear.odds.upper <- exp(noyear.logodds.upper)
noyear.odds.lower
noyear.odds.upper
#visualization
plot_model(model2, show.values = TRUE, axis.lim = c(1,4),
           title = "Odds (instructor)", show.p = FALSE)

data.noyear <- data.noyear %>%
  mutate(odds = exp(logodds))

#probability 
#adding the predict value
data.noyear <- data.noyear %>%
  mutate(probs = fitted(model2))

ggplot(data = data.noyear, aes(x = price, y = probs)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "price", y = "Probability of instructor being '1'")

#ROC plot 
pro.noyear <- predict(model2, data, type = "response")
predict.noyear <- prediction(pro.noyear, data$bnpoints)
per.noyear <- performance(predict.noyear, measure = "tpr", x.measure = "fpr")
plot(per.noyear, col = "blue")

#outputting the AUC
auc.noyear <- performance(predict.noyear, measure = "auc")
auc.noyear@y.values
#Why auc has not changed a little？？？？？？


#the model3 with only price and year
model3 <- glm(bnpoints ~ price + year,
             data=data, family=binomial(link = "logit"))
summary(model3)
summ(model3) 
mod3coefs <- round(coef(model3), 2)

#rmd
# confint(model) %>%
#   kable()

#Log-odds
mod3.coef.logodds <- model %>%
  summary() %>%
  coef()
price3.logodds.lower <- mod3.coef.logodds["price", "Estimate"] - 
  1.96 * mod3.coef.logodds["price", "Std. Error"]
price3.logodds.upper <- mod3.coef.logodds["price", "Estimate"] + 
  1.96 * mod3.coef.logodds["price", "Std. Error"]
price3.logodds.lower
price3.logodds.upper

plot_model(model3, show.values = TRUE, transform = NULL,
           title = "Log-Odds (instructor)", show.p = FALSE)

data.nocountry <- data %>%
  select(bnpoints, price, year) %>%
  mutate(logodds = predict(model3))

#Odds
price3.odds.lower <- exp(price3.logodds.lower)
price3.odds.upper <- exp(price3.logodds.upper)
price3.odds.lower
price3.odds.upper

plot_model(model3, show.values = TRUE, axis.lim = c(1,1.5),
           title = "Odds (instructor)", show.p = FALSE)

data.nocountry <- data.nocountry %>%
  mutate(odds = exp(logodds))

#Probabilities
p.num <- exp(mod3.coef.logodds["(Intercept)", "Estimate"] + mod3.coef.logodds["price", "Estimate"] * 52)
p.denom <- 1 + p.num
p.num / p.denom
plogis(mod3.coef.logodds["(Intercept)", "Estimate"] + mod3.coef.logodds["price", "Estimate"] * 52)

data.nocountry <- data.nocountry %>%
  mutate(probs = fitted(model3))

ggplot(data = data.nocountry, aes(x = price, y = probs)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "price", y = "Probability of instructor being '1'")


#model of bnpoints ~ price + year 
pro.nocountry <- predict(model3, data, type = "response")
predict.nocountry <- prediction(pro.nocountry, data$bnpoints)
per.nocountry <- performance(predict.nocountry, measure = "tpr", x.measure = "fpr")
plot(per.nocountry, col = "blue")

#outputting the AUC
auc.nocountry <- performance(predict.nocountry, measure = "auc")
auc.nocountry@y.values








