# Helper packages
library(tidyverse)
library(dplyr) # for data wrangling
library(ggplot2) # for awesome plotting
library(rsample) # for data splitting
# Modeling packages
library(caret) # for logistic regression modeling
# Model interpretability packages
library(vip) # variable importance

df <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)
# Create training (70%) and test (30%) sets for the
# rsample::attrition data.
set.seed(45623) # for reproducibility
churn_split <- initial_split(df, prop = .7, strata = "Attrition")
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

#simple logistic regression
model1 <- glm(as.factor(Attrition) ~ MonthlyIncome, family = "binomial",
              data = churn_train)
model2 <- glm(as.factor(Attrition) ~ OverTime, family = "binomial",
              data = churn_train)

summary(model1)

summary(model2)

exp(coef(model1))

exp(coef(model2))

model3 <- glm(
  as.factor(Attrition) ~ MonthlyIncome + OverTime,
  family = "binomial",
  data = churn_train
)

summary(model3)

exp(coef(model3))
exp(confint(model3))


set.seed(123)
cv_model1 <- train(
  as.factor(Attrition) ~ MonthlyIncome,
  data = churn_train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model2 <- train(
  as.factor(Attrition) ~ MonthlyIncome+OverTime+Gender+StandardHours+EmployeeCount,
  data = churn_train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model3 <- train(
  as.factor(churn_train$Attrition) ~ .,
  data = churn_train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)





