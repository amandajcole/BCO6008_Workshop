# install.packages("MASS")
# install.packages("ISLR")
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)
library(MASS)
library(ISLR)
# install.packages("ranger", "randomforest")
library(ranger, randomforest)


set.seed(123)

#step 1 - specify model: type, mode and engine

lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

view(Boston)
data(Boston)

#Step 2 - take the model specification from step 1 and apply it to 
#the data = use fit() and put formula y~x

lm_fit <- lm_spec %>%
  fit(medv~lstat, data = Boston)

# look at the data
lm_fit %>% pluck("fit") %>% summary()

tidy(lm_fit)

# Step 3 - when we use the fitted model from step 2 to predict new y 
# in new data - eg testing data or completely new data
predict(lm_fit, new_data = Boston)

# examining new predicted values
final_model <- augment(lm_fit, new_data = Boston) # %>% select(medv, .pred)





rm(list = ls())

data(Boston)

# step 1

model_spec<-linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")
# step 2


model_fit <- model_spec %>% fit(data = Boston, medv~age+crim+rm)

# step 3
second_model <- augment(model_fit, new_data = Boston)

tidy(model_fit)





rm(list = ls())

data(Boston)

# step 1

model_spec_3<-linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")



# step 2


model_fit_3 <- model_spec_3 %>% fit(data = Boston, medv~.)

# step 3
third_model <- augment(model_fit_3, new_data = Boston)

# https://parsnip.tidymodels.org/reference/index.html


model_spec_rf <- rand_forest() %>%
  set_mode("regression") %>%
  set_engine("ranger")

