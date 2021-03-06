#Package
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

#glimpse data 
str(data)

# data processing 
# trans to binary
data$bnpoints[data$points > 90] <- 1 
data$bnpoints[data$points <= 90] <- 0
#converting the type of bnpoints to factor
data$bnpoints <- as.factor(data$bnpoints)

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
  labs(x = "Points", y = "Price", title = "The boxplot of price grouped by points") + 
  scale_x_discrete("Points", labels = c("0" = "Good", "1" = "Excellent")) +
  theme(legend.position = "none") 

#scatter plot
ggplot(data = data, aes(x = price, y = bnpoints)) +
  geom_point(position = position_jitter()) +
  scale_y_discrete("Points", labels = c("0" = "Good", "1" = "Excellent")) +
  labs(x = "Price", y = "Points", title = "The scatter plot of Price against Points") 
  

#The boxplot of points against year
ggplot(data = data, aes(x = bnpoints, y = year, group = bnpoints)) +
  geom_boxplot(fill = c("blue","orange")) +
  labs(x = "Points", y = "Year", title = "The boxplot of year grouped by points") + 
  scale_x_discrete("Points", labels = c("0" = "Good", "1" = "Excellent")) +
  theme(legend.position = "none") 

#scatter plot
ggplot(data = data, aes(x = year, y = bnpoints)) +
  geom_point(position = position_jitter()) +
  scale_y_discrete("Points", labels = c("0" = "Good", "1" = "Excellent")) +
  labs(x = "Year", y = "Points", title = "The scatter plot of Price against Points") 

#The pie chart
ggplot(data = data, aes(x = factor(1), fill = country))+
  geom_bar(width = 2)+
  coord_polar("y")

#model1:bnpoints ~ country + price + year
model1 <- glm(bnpoints ~ country + price + year,
             data=data, family=binomial(link = "logit"))
model1%>%
  summary() 
#Inference 
#analysis of deviance table
anova(model1, test = "Chi") 
# %>%kabel()
#the p-value of the price is not significant, trying to fit a model omitting year variables

#the model2: bnpoints ~ price + country
model2 <- glm(bnpoints ~ country + price, data = data, family = binomial(link = "logit"))

model2%>%
  summary()

#model3 dividing the country into three parts, fitting a new model
data.3 <- data %>%
  select(bnpoints, country, price) %>%
  mutate(country.new = ifelse(country == "Austria", "Austria",
                                     ifelse(country == "Germany", "Germany",
                                            "AAOthercountry")))
model3 <-  glm(bnpoints ~ country.new + price, data = data.3, 
               family = binomial(link = "logit")) 
model3 %>%
  summary()

# Confidence Interval of odd ratio
plot_model(model3, show.values = TRUE,
           title = "Odds ratio", show.p = TRUE)

#probability 
#adding the predict value
data.3 <- data.3 %>%
  mutate(probs = fitted(model3))

#predicting result 
ggplot(data = data.3, aes(x = price, y = probs)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "price", y = "Probability of being Excellent wine")

#plot the predicted probabilities being excellent wine against price by the country
plot_model(model3, type = "pred", terms = c("price", "country.new"))

#ROC plot 
#goodness of the model
library(ROCR)
pro.3 <- predict(model3, data.3, type = "response")
predict.3 <- prediction(pro.3, data$bnpoints)
per.3 <- performance(predict.3, measure = "tpr", x.measure = "fpr")
plot(per.3, col = "red", title = "ROC plot")

#outputting the AUC
auc.3 <- performance(predict.3, measure = "auc")
auc.3@y.values

#DELEX model to explain the importance of the variables
#library the package
library(lattice)
library(caret)
library(DALEX)

#classifying the training dataset and test dataset
wine <- as_tibble(data)
str(wine)
id <- sample(2, nrow(wine), replace = TRUE, prob = c(0.7, 0.3))
winetrain <- wine[id == 1,]
winetest <- wine[id == 2,]

#building the full model using GLM
bnpoints_glm <- train(bnpoints ~country + price + year, data = winetrain,
                      method = "glm", family = "binomial")
#explaining the model
p_fun <- function(object, newdata){
  predict(object, newdata = newdata, type = "prob")[,2]
}
yTest <- as.numeric(as.character(winetest$bnpoints))

explainer_glm <- DALEX::explain(bnpoints_glm, label = "glm", 
                                data = winetest, y = yTest,
                                predict_function = p_fun)

#the importance analysis of variables
importance_glm <- variable_importance(explainer_glm, loss_function = loss_root_mean_square)
plot(importance_glm)











