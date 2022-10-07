# Written by: Mwangi George
# Written on: 2022-10-06
# Description: HR Data Analytics Project script

# loading the package tidyverse. 
library(tidyverse) 

# loading the package fastDummies for efficient dummy variable creation.
library(fastDummies)

# Loading the package mfx for calculating marginal effects of a model
library(mfx)

# Read data from working directory and stores it as hr_data in R
hr_data <- read_csv("Dataset/HR_comma_sep.csv", 
                    show_col_types = F) 

# Print the structure of the dataframe
glimpse(hr_data)

# convert the variables work_accident, left, promotion_last_5years, Department, and salary into factors.
hr_data <- hr_data %>% 
  mutate(Work_accident = as.factor(Work_accident)) %>% 
  mutate(left = as.factor(left)) %>% 
  mutate(promotion_last_5years = as.factor(promotion_last_5years)) %>% 
  mutate(Department = as.factor(Department)) %>% 
  mutate(salary = as.factor(salary))

# print new variables and their class
sapply(hr_data, class)

# print variable names 
names(hr_data)

# rename all variables to lower case
hr_data <- hr_data %>% 
  rename_all(tolower)

# check for rows with NAs 
hr_data[!complete.cases(hr_data),]  # No missing data

hr_data <- hr_data %>% 
  mutate(left = if_else(left == 0, "no", "yes")) %>% 
  mutate(left = as.factor(left))

# read the first 3 rows of the dataframe
hr_data %>% 
  head(3)

# unique observations in the left variable
unique(hr_data$left)

# summarize the data using the variable left based on mean (EDA)
hr_data %>% 
  group_by(left) %>% 
  summarise(mean_satisfaction_level = mean(satisfaction_level),
            mean_last_evaluation = mean(last_evaluation),
            mean_number_project = mean(number_project),
            mean_average_monthly_hours = mean(average_montly_hours),
            mean_time_spend_company = mean(time_spend_company))

# count the number of employees in each salary category and group by the left variable
hr_data %>% 
  select(left, salary) %>% 
  group_by(left) %>% 
  table()

# bar chart showing impact of employees salaries on retention
ggplot(data = hr_data)+
  geom_bar(aes(x = salary, fill = left), position = "dodge")+
  labs(title = "Employee Retention by Salary Category", y = "Count")+
  theme_classic()

# count the number of employees in each department category and group by the left variable
hr_data %>% 
  select(department, left) %>% 
  group_by(left) %>% 
  table()

# Achieving the same using base R xtabs function
xtabs(~department+ left, hr_data)

## bar chart showing correlation between Employee Department and Retention
hr_data %>% 
  ggplot(aes(department, fill = left))+
  geom_bar(position = "dodge", alpha = 0.8)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Employee Retention by Department")

#Modify the promotion_last_5years variable into "promoted" and "not promoted" for easy understanding of the visualization
hr_data <- hr_data %>% 
  mutate(promotion_last_5years = if_else(promotion_last_5years == 1, "promoted", "not promoted"))

# print the first 3 rows 
head(hr_data, 3)

# count the number of employee in each promotion category and group by whether they left or not
xtabs(~ promotion_last_5years + left, data = hr_data)

# visualizing the above table
hr_data %>% 
  ggplot(aes(promotion_last_5years, fill = left))+
  geom_bar(position = "dodge", alpha = 0.8)+
  theme_classic()+
  labs(title = "Employe Retention by Promotion last 5 years")

# selecting variables for use in modeling 

model_data <- hr_data %>% 
  dplyr::select(left, satisfaction_level, average_montly_hours, promotion_last_5years, salary)

head(model_data)
# view the structure of new dataset 

str(model_data)
# manipulating the variables promotion_last_5years and left into 1's and 0's
model_data <- model_data %>% 
  mutate(promotion_last_5years = if_else(promotion_last_5years == "promoted", 1,0)) %>% 
  mutate(left = if_else(left == "yes", 1,0))

# print first six rows
head(model_data)

# call the sapply function to check variable types
sapply(model_data, class)

# convert the variables promotion_last_5years and left into factors
model_data <- model_data %>% 
  mutate(left = as.factor(left)) %>% 
  mutate(promotion_last_5years = as.factor(promotion_last_5years))

# print first six rows
head(model_data)

# call the sapply function again to check variable types
sapply(model_data, class)

# check if we have the correct data types 
str(model_data)



# loading package for efficient dummy variable creation
library(fastDummies)

# Create dummies using the fastDummies package
model_data <- dummy_cols(model_data, select_columns = "salary")
head(model_data)

# modify the created dummies into factors 
model_data <- model_data %>% 
  mutate(salary_high = as.factor(salary_high)) %>% 
  mutate(salary_low = as.factor(salary_low)) %>% 
  mutate(salary_medium = as.factor(salary_medium))
head(model_data)

# deselect the column salary
model_data <- model_data %>% 
  dplyr::select(-salary)

# Check for correct data types 
str(model_data)

# To make my results reproducible
set.seed(1)

# Utilize 70 percent of the dataset as training set, and the remaining 30 percent as testing set.
sample <- sample(c(T, F), nrow(model_data), replace = T, prob = c(0.7, 0.3))

# assign train set to train
train <- model_data[sample, ]

#assign test set to test
test <- model_data[!sample, ]

# fit the logistic regression model. I omit one dummy variable (salary_medium) to avoid the dummy variable trap
logistic_model <- glm(left ~ satisfaction_level + average_montly_hours + promotion_last_5years + salary_high + salary_low, data = train, family = "binomial")

# Disable the scientific notation for model summary
options(scipen = 999)

# call model summary
summary(logistic_model)

# loading the mfx package to calculate marginal effects
library(mfx)

# Calculate coefficient marginal effects 
marginals <- logitmfx(logistic_model, data = train)

# print marginals 
marginals

# combining results from the two outputs 
results <- data.frame(variable = c("constant", "satisfaction_level", "average_montly_hours", "promotion_last_5years", "salary_high", "salary_low"),
                      coefficient_estimate = c( 0.230089265, -3.757770499, 0.002696088, -1.241433934, -1.314926658,  0.480776151),
                      marginal_effect = c(NA, -0.585600940, 0.000420151, -0.131127097,  -0.143944516,  0.075207864),
                      P_value = c(0.0582, 0.0000000000000002, 0.0000000334, 0.0000097573, 0.0000000000000002, 0.0000000000000002))

# print results
results

# Calculate the pR2
pscl::pR2(logistic_model)["McFadden"]

# Alternatively, using base R functions
with(summary(logistic_model), 1-deviance/null.deviance)

# Calculate variable importance
caret::varImp(logistic_model)

# utilizing the vif function from the car package to check for multicollinearity
car::vif(logistic_model)

# Defining two new employees
new <- data.frame(satisfaction_level = 0.64, average_montly_hours = 250, promotion_last_5years = c(1, 0), salary_high = c(0, 1), salary_low = c(1,0), salary_medium = 0)

# changing categorical variables to factors 
new <- new %>% 
  mutate(promotion_last_5years = as.factor(promotion_last_5years),
         salary_high = as.factor(salary_high),
         salary_low = as.factor(salary_low),
         salary_medium = as.factor(salary_medium))
# view new
head(new)

# predict probability of leaving
predict(logistic_model, new, type = "response")

# calculate probabilities using the test data
predicted <- predict(logistic_model, test, type = "response")

# view the first 10 probabilities
head(predicted, 10)

# Loading the Information value package
library(InformationValue)

# find the optimal cutoff probability to use
optimal <- optimalCutoff(test$left, predicted)[1]

# print optimal 
optimal

# calculate misclassification error
misClassError(test$left, predicted, threshold = optimal)

# plot the ROC curve
plotROC(test$left, predicted)

