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




