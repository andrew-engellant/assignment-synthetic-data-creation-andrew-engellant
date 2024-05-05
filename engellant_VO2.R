# The below code synthesizes artificial data that resembles the real-world
# The goal is to create a data set that can be used to predict an individuals
# VO2 Max from their age, height, weight, max_hr, training status and sex

library(tidyverse)
library(Hmisc)

set.seed(20240216)

# Build an empty dataframe. 
num_rows <- 1000

fake_data <- tibble(row=1:num_rows)

# Add variables and fill table with values
# Categorical variables
fake_data <- fake_data %>%
  mutate(training = sample(x = c("intense", "high", "moderate", "low", "none"),
                          size = num_rows,
                          replace = TRUE,
                          prob = c(0.1, 0.2, 0.3, 0.25, 0.15)),
         assigned_sex = sample(x = c("male", "female"),
                               size = num_rows,
                               replace = TRUE,
                               prob = c(0.6, 0.4)))

# Contiuous Variables
fake_data <- fake_data %>%
  mutate(height = rnorm(n = num_rows,
                        mean = 68,
                        sd = 5),
         weight = rnorm(n = num_rows,
                        mean = 165,
                        sd = 35),
         max_hr = rnorm(n = num_rows,
                        mean = 192,
                        sd = 5),
         age = rnorm(n = num_rows,
                     mean = 33,
                     sd = 7))

# Create some coefficients for the categorical variables
training_coef <- tibble(training = c("intense", "high", "moderate", "low", "none"),
                        tr_coef = c(1.6, 1.4, 1.1, 1, 0.9))

sex_coef <- tibble(assigned_sex = c("male", "female"),
                   sex_coef = c(1.2, 1))

# Join on the coefs
fake_data <- fake_data %>% 
  left_join(training_coef,by="training") %>% 
  left_join(sex_coef,by="assigned_sex")

# Create a model
fake_data <- fake_data %>%
  mutate(VO2_Max = 30 * (tr_coef * sex_coef) 
         - age * 0.15
         + max_hr * 0.08
         - weight * 0.05
         + rnorm(num_rows,
                 mean=0,
                 sd=5))

# Drop the coefficients
fake_data <- fake_data %>% 
  select(-contains("coef"))

# Write txt file
write_tsv(fake_data %>% select(row:VO2_Max),"fake_VO2_max_data.txt")

# Check data with a model
summary(lm(VO2_Max ~ age + weight + max_hr + assigned_sex + training,
        data = fake_data))
       
         
