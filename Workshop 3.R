library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)

#muffin_cupcake_data_original <- read_csv("https://raw.github.com/adashofdata/muffin-cupcake/blob/master/recipes_muffins_cupcakes.csv")


data_orig <- read_csv("https://raw.githubusercontent.com/adashofdata/muffin-cupcake/master/recipes_muffins_cupcakes.csv")

data_orig %>% skim()

#clean variable names
muffin_cupcake_clean <- clean_names(data_orig)

#splitting the clean dataset into training and testing
muffin_cupcake_split <- initial_split(muffin_cupcake_clean)

#save training and testing datset separately
muffin_cupcake_train <- training(muffin_cupcake_split)
muffin_cupcake_test <- testing(muffin_cupcake_split)

muffin_cupcake_clean %>% count(type)

# define the recipe
muffin_recipe <- recipe(type~flour + milk + sugar+egg, data = muffin_cupcake_train)

summary(muffin_recipe)
muffin_recipe_steps <- muffin_recipe %>%
  step_impute_mean(all_numeric()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_range(all_numeric(),min = 0, max = 1)

muffin_recipe_steps

#Prepare the recipe

prepped_recipe <- prep(muffin_recipe_steps, training = muffin_cupcake_train)

prepped_recipe

prepped_recipe_train_preprocessed <- bake(prepped_recipe, muffin_cupcake_train)

prepped_recipe_test_preprocessed <- bake(prepped_recipe, muffin_cupcake_test)
