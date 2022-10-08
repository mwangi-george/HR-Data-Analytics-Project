HR Data Analytics Markdown
================
Mwangi George
2022-10-05

-   <a href="#introduction" id="toc-introduction">Introduction</a>
-   <a href="#loading-important-packages"
    id="toc-loading-important-packages">Loading important packages.</a>
-   <a href="#loading-dataset-from-working-directory"
    id="toc-loading-dataset-from-working-directory">Loading dataset from
    working directory</a>
-   <a href="#understanding-the-dataframe"
    id="toc-understanding-the-dataframe">Understanding the dataframe.</a>
    -   <a href="#data-manipulation" id="toc-data-manipulation">Data
        manipulation</a>
-   <a href="#exploratory-data-analysis"
    id="toc-exploratory-data-analysis">Exploratory data analysis</a>
    -   <a href="#findings" id="toc-findings">Findings</a>
-   <a href="#logistic-regression" id="toc-logistic-regression">Logistic
    Regression</a>
    -   <a href="#introduction-1" id="toc-introduction-1">Introduction</a>
    -   <a href="#creating-training-and-test-samples"
        id="toc-creating-training-and-test-samples">Creating training and test
        samples</a>
    -   <a href="#training-the-model" id="toc-training-the-model">Training the
        model</a>
    -   <a href="#calculating-marginal-effects"
        id="toc-calculating-marginal-effects">Calculating marginal effects</a>
    -   <a href="#simplified-table-of-the-results"
        id="toc-simplified-table-of-the-results">Simplified table of the
        results</a>
    -   <a href="#results-interpretation"
        id="toc-results-interpretation">Results Interpretation</a>
    -   <a href="#assessing-model-fitgoodness-of-fit"
        id="toc-assessing-model-fitgoodness-of-fit">Assessing Model Fit/Goodness
        of Fit</a>
    -   <a href="#assessing-variable-importance"
        id="toc-assessing-variable-importance">Assessing Variable importance</a>
    -   <a href="#checking-for-multicollinearity"
        id="toc-checking-for-multicollinearity">Checking for
        Multicollinearity</a>
    -   <a href="#use-the-fitted-model-to-make-predictions"
        id="toc-use-the-fitted-model-to-make-predictions">Use the fitted model
        to make predictions</a>
    -   <a href="#model-diagnostics" id="toc-model-diagnostics">Model
        Diagnostics</a>
    -   <a href="#receiver-operating-characteristic-curve"
        id="toc-receiver-operating-characteristic-curve">Receiver Operating
        Characteristic Curve</a>
-   <a href="#recommendations" id="toc-recommendations">Recommendations</a>
-   <a href="#references" id="toc-references">References</a>
-   <a href="#contact-me" id="toc-contact-me">Contact Me</a>

## Introduction

-   As noted in the README file, my main focus in this markdown is to
    conduct Exploratory Data Analysis (EDA) and build a model that
    predicts employee retention.

## Loading important packages.

-   For consistency of functions and productivity, I prefer working with
    the whole `tidyverse package`.

``` r
# loading important packages. 
library(tidyverse) 
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(mfx)
```

    ## Loading required package: sandwich
    ## Loading required package: lmtest
    ## Loading required package: zoo
    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric
    ## 
    ## Loading required package: MASS
    ## 
    ## Attaching package: 'MASS'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select
    ## 
    ## Loading required package: betareg

``` r
library(InformationValue)
library(car)
```

    ## Loading required package: carData
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following objects are masked from 'package:InformationValue':
    ## 
    ##     confusionMatrix, precision, sensitivity, specificity
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

## Loading dataset from working directory

-   The dataset is in the comma separated values format.

``` r
# Read data from working directory and stores it as hr_data in R
hr_data <- read_csv("Dataset/HR_comma_sep.csv", 
                    show_col_types = F) 
```

## Understanding the dataframe.

-   Before jumping into the actual tasks, it is important that I
    understand the data I am working with

``` r
# Print the structure of the dataframe
glimpse(hr_data)
```

    ## Rows: 14,999
    ## Columns: 10
    ## $ satisfaction_level    <dbl> 0.38, 0.80, 0.11, 0.72, 0.37, 0.41, 0.10, 0.92, …
    ## $ last_evaluation       <dbl> 0.53, 0.86, 0.88, 0.87, 0.52, 0.50, 0.77, 0.85, …
    ## $ number_project        <dbl> 2, 5, 7, 5, 2, 2, 6, 5, 5, 2, 2, 6, 4, 2, 2, 2, …
    ## $ average_montly_hours  <dbl> 157, 262, 272, 223, 159, 153, 247, 259, 224, 142…
    ## $ time_spend_company    <dbl> 3, 6, 4, 5, 3, 3, 4, 5, 5, 3, 3, 4, 5, 3, 3, 3, …
    ## $ Work_accident         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ left                  <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ promotion_last_5years <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ Department            <chr> "sales", "sales", "sales", "sales", "sales", "sa…
    ## $ salary                <chr> "low", "medium", "medium", "low", "low", "low", …

### Data manipulation

1.  From the above output, some variables that should be categorical
    variables are stored as doubles (numerics). This calls for some
    cleaning to avoid misleading results.

``` r
# convert the variables work_accident, left, promotion_last_5years, Department, and salary into factors.
hr_data <- hr_data %>% 
  mutate(Work_accident = as.factor(Work_accident),
         left = as.factor(left),
         promotion_last_5years = as.factor(promotion_last_5years),
         Department = as.factor(Department),
         salary = as.factor(salary)) 
# print new variables and their class
sapply(hr_data, class)
```

    ##    satisfaction_level       last_evaluation        number_project 
    ##             "numeric"             "numeric"             "numeric" 
    ##  average_montly_hours    time_spend_company         Work_accident 
    ##             "numeric"             "numeric"              "factor" 
    ##                  left promotion_last_5years            Department 
    ##              "factor"              "factor"              "factor" 
    ##                salary 
    ##              "factor"

2.  It is also clear from the `glimpse(hr_data)` output that there is an
    inconsistent naming of variables; Work_accident and Department start
    with upper case while others do not. To enhance productivity in
    later stages, I will convert them to lower case.

``` r
# print variable names 
names(hr_data)
```

    ##  [1] "satisfaction_level"    "last_evaluation"       "number_project"       
    ##  [4] "average_montly_hours"  "time_spend_company"    "Work_accident"        
    ##  [7] "left"                  "promotion_last_5years" "Department"           
    ## [10] "salary"

``` r
# rename all variables to lower case
hr_data <- hr_data %>% 
  rename_all(tolower)
```

3.  It is important to check for any missing data in the dataframe to
    avoid inconsistent results.

``` r
# check for rows with NAs 
hr_data[!complete.cases(hr_data),]
```

    ## # A tibble: 0 × 10
    ## # … with 10 variables: satisfaction_level <dbl>, last_evaluation <dbl>,
    ## #   number_project <dbl>, average_montly_hours <dbl>, time_spend_company <dbl>,
    ## #   work_accident <fct>, left <fct>, promotion_last_5years <fct>,
    ## #   department <fct>, salary <fct>

``` r
# No missing data
```

4.  Before moving to analysis, the `left` variable is a factor
    containing two levels, 0 if the employee continue to stay at the
    company and 1 if they left. For easier workflow, I will change these
    levels into `no` and `yes`.

``` r
hr_data <- hr_data %>% 
  mutate(left = if_else(left == 0, "no", "yes")) %>% 
  mutate(left = as.factor(left))

# read the first 3 rows of the dataframe
hr_data %>% 
  head(3)
```

    ## # A tibble: 3 × 10
    ##   satisfa…¹ last_…² numbe…³ avera…⁴ time_…⁵ work_…⁶ left  promo…⁷ depar…⁸ salary
    ##       <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <fct>   <fct> <fct>   <fct>   <fct> 
    ## 1      0.38    0.53       2     157       3 0       yes   0       sales   low   
    ## 2      0.8     0.86       5     262       6 0       yes   0       sales   medium
    ## 3      0.11    0.88       7     272       4 0       yes   0       sales   medium
    ## # … with abbreviated variable names ¹​satisfaction_level, ²​last_evaluation,
    ## #   ³​number_project, ⁴​average_montly_hours, ⁵​time_spend_company,
    ## #   ⁶​work_accident, ⁷​promotion_last_5years, ⁸​department

``` r
# unique observations in the left variable
unique(hr_data$left)
```

    ## [1] yes no 
    ## Levels: no yes

## Exploratory data analysis

-   In this section, the objective is to find out which variables have a
    direct and clear impact on employee retention. To accomplish this, I
    will group the employees into two; those who left `yes` and those
    who stayed `no`. For the numeric variables, I will calculate the
    mean for each of the groups. These means can be useful metrics to
    explain employee retention.

``` r
# summarize the data using the variable left based on mean (EDA)
hr_data %>% 
  group_by(left) %>% 
  summarise(mean_satisfaction_level = mean(satisfaction_level),
            mean_last_evaluation = mean(last_evaluation),
            mean_number_project = mean(number_project),
            mean_average_monthly_hours = mean(average_montly_hours),
            mean_time_spend_company = mean(time_spend_company))
```

    ## # A tibble: 2 × 6
    ##   left  mean_satisfaction_level mean_last_evaluation mean_numb…¹ mean_…² mean_…³
    ##   <fct>                   <dbl>                <dbl>       <dbl>   <dbl>   <dbl>
    ## 1 no                      0.667                0.715        3.79    199.    3.38
    ## 2 yes                     0.440                0.718        3.86    207.    3.88
    ## # … with abbreviated variable names ¹​mean_number_project,
    ## #   ²​mean_average_monthly_hours, ³​mean_time_spend_company

### Findings

-   **Part 1 - Numeric Variables**

1.  There exists a big difference in the `mean_satisfaction_level`
    between employees who left and those who stayed. Employees who left
    had a lower satisfaction level compared to those who stayed by
    0.2267116 `(0.6668096-0.4400980)`. This could explain why they left
    but further statistical analysis is necessary to test whether the
    difference observed is statistically significant.

2.  There is no clear impact of the variable `last_evaluation` on
    employee retention since the means for both groups are nearly equal.

3.  The same case applies to `mean_number_project`. The difference is
    very small but statistical analysis is vital to test if it is indeed
    statistically significant.

4.  The difference in the `mean_average_monthly_hours` between employees
    who left and those who stayed is quite big. On average, those who
    left appear to have been working more more than those who stayed by
    8.359 hours.

5.  For the categorical variables i.e (`work_accident`, `department`,
    and `salary`), I will have to take another approach to understand
    their impact on employee retention.

-   **Part 2 - Categorical Variables**

1.  **Impact of employee salary on retention**

-   Since I am dealing with two categorical variables, the best way to
    understand their relationship is through a visualization. First, I
    will create a table that summarizes the salary categories by whether
    one left or stayed.

``` r
# count the number of employees in each salary category and group by the left variable
hr_data %>% 
  dplyr::select(left, salary) %>% 
  group_by(left) %>% 
  table()
```

    ##      salary
    ## left  high  low medium
    ##   no  1155 5144   5129
    ##   yes   82 2172   1317

``` r
# bar chart showing impact of employees salaries on retention
ggplot(data = hr_data)+
  geom_bar(aes(x = salary, fill = left), position = "dodge", alpha = 0.8)+
  labs(title = "Employee Retention by Salary Category", y = "Count")+
  theme_classic()
```

![](Project_Report_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

-   From the above graph, most of the employees who left the company
    were in the low salary category, followed by medium then high. I
    notice that these results are consistent with theory.

2.  **Correlation between Employee Department and Retention**

``` r
# count the number of employees in each department category and group by the left variable
hr_data %>% 
  dplyr::select(department, left) %>% 
  group_by(left) %>% 
  table()
```

    ##              left
    ## department      no  yes
    ##   accounting   563  204
    ##   hr           524  215
    ##   IT           954  273
    ##   management   539   91
    ##   marketing    655  203
    ##   product_mng  704  198
    ##   RandD        666  121
    ##   sales       3126 1014
    ##   support     1674  555
    ##   technical   2023  697

``` r
# Achieving the same using base R xtabs function
xtabs(~department+ left, hr_data)
```

    ##              left
    ## department      no  yes
    ##   accounting   563  204
    ##   hr           524  215
    ##   IT           954  273
    ##   management   539   91
    ##   marketing    655  203
    ##   product_mng  704  198
    ##   RandD        666  121
    ##   sales       3126 1014
    ##   support     1674  555
    ##   technical   2023  697

``` r
## bar chart showing correlation between Employee Department and Retention
hr_data %>% 
  ggplot(aes(department, fill = left))+
  geom_bar(position = "dodge", alpha = 0.8)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Employee Retention by Department")
```

![](Project_Report_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

-   From the above chart, most of the employees who left the firm were
    in the sales department, followed by the technical department and
    then support department as the top 3.

3.  **Correlation between `promotion_last_5years` and employee
    retention**

``` r
#Modify the variable promotion_last_5years into "promoted" and "not promoted" for easy understanding of the visualization
hr_data <- hr_data %>% 
  mutate(promotion_last_5years = if_else(promotion_last_5years == 1, "promoted", "not promoted"))
# print the first 3 rows 
head(hr_data, 3)
```

    ## # A tibble: 3 × 10
    ##   satisfa…¹ last_…² numbe…³ avera…⁴ time_…⁵ work_…⁶ left  promo…⁷ depar…⁸ salary
    ##       <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <fct>   <fct> <chr>   <fct>   <fct> 
    ## 1      0.38    0.53       2     157       3 0       yes   not pr… sales   low   
    ## 2      0.8     0.86       5     262       6 0       yes   not pr… sales   medium
    ## 3      0.11    0.88       7     272       4 0       yes   not pr… sales   medium
    ## # … with abbreviated variable names ¹​satisfaction_level, ²​last_evaluation,
    ## #   ³​number_project, ⁴​average_montly_hours, ⁵​time_spend_company,
    ## #   ⁶​work_accident, ⁷​promotion_last_5years, ⁸​department

``` r
# count the number of employee in each promotion category and group by whether they left or not
xtabs(~ promotion_last_5years + left, data = hr_data)
```

    ##                      left
    ## promotion_last_5years    no   yes
    ##          not promoted 11128  3552
    ##          promoted       300    19

``` r
# visualizing the above table
hr_data %>% 
  ggplot(aes(promotion_last_5years, fill = left))+
  geom_bar(position = "dodge", alpha = 0.8)+
  theme_classic()+
  labs(title = "Employe Retention by Promotion last 5 years")
```

![](Project_Report_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

-   From the above table and bar chart, not receiving a promotion has a
    high impact on retention. Out of a total of 3571 employees who left
    the company, 3552(about 99 percent) did not receive a promotion in
    the last five years. This produces the idea that the variable
    `promotion_last_5years` has a direct on employee retention.

-   Based on this exploratory analysis, it is safe to say that the
    variables `satisfaction_level`, `average_monthly_hours`,
    `promotion_last_5years`, and `salary` have a direct impact on
    employee retention. Using these variables, I will proceed to
    building a logistic regression model to predict employee retention.

## Logistic Regression

### Introduction

-   Logistic regression utilizes the method of maximum likelihood
    estimation to identify an equation of the form *log\[p(X)/(1-p(x))\]
    = pr(D = 1) = B0 +B1X1 +B2X2 + … + BnXn + u*.

-   The right hand side of the equation predicts the logit (log odds) of
    the dependent variable taking the value 1. In my case, I am
    predicting the probability of leaving (1) using the variables
    `satisfaction_level`, `average_monthly_hours`,
    `promotion_last_5years`, and `salary`.

-   Forming an equation to represent this

-   *\`left = B0 + B1 x satisfaction_level + B2 x
    average_monthly_hours + B3 x promotion_last_5years + B4 x salary*

-   **Selecting necessary variables and assigning the data to
    model_data**

``` r
# select variables for use in modeling 
model_data <- hr_data %>% 
  dplyr::select(left, satisfaction_level, average_montly_hours, promotion_last_5years, salary)

# print first six rows
head(model_data)
```

    ## # A tibble: 6 × 5
    ##   left  satisfaction_level average_montly_hours promotion_last_5years salary
    ##   <fct>              <dbl>                <dbl> <chr>                 <fct> 
    ## 1 yes                 0.38                  157 not promoted          low   
    ## 2 yes                 0.8                   262 not promoted          medium
    ## 3 yes                 0.11                  272 not promoted          medium
    ## 4 yes                 0.72                  223 not promoted          low   
    ## 5 yes                 0.37                  159 not promoted          low   
    ## 6 yes                 0.41                  153 not promoted          low

``` r
# view the structure of new dataset 
str(model_data)
```

    ## tibble [14,999 × 5] (S3: tbl_df/tbl/data.frame)
    ##  $ left                 : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ satisfaction_level   : num [1:14999] 0.38 0.8 0.11 0.72 0.37 0.41 0.1 0.92 0.89 0.42 ...
    ##  $ average_montly_hours : num [1:14999] 157 262 272 223 159 153 247 259 224 142 ...
    ##  $ promotion_last_5years: chr [1:14999] "not promoted" "not promoted" "not promoted" "not promoted" ...
    ##  $ salary               : Factor w/ 3 levels "high","low","medium": 2 3 3 2 2 2 2 2 2 2 ...

``` r
# manipulating the variables promotion_last_5years and left into 1's and 0's
model_data <- model_data %>% 
  mutate(promotion_last_5years = if_else(promotion_last_5years == "promoted", 1,0)) %>% 
  mutate(left = if_else(left == "yes", 1,0))

# print first six rows
head(model_data)
```

    ## # A tibble: 6 × 5
    ##    left satisfaction_level average_montly_hours promotion_last_5years salary
    ##   <dbl>              <dbl>                <dbl>                 <dbl> <fct> 
    ## 1     1               0.38                  157                     0 low   
    ## 2     1               0.8                   262                     0 medium
    ## 3     1               0.11                  272                     0 medium
    ## 4     1               0.72                  223                     0 low   
    ## 5     1               0.37                  159                     0 low   
    ## 6     1               0.41                  153                     0 low

``` r
# check variable types
sapply(model_data, class)
```

    ##                  left    satisfaction_level  average_montly_hours 
    ##             "numeric"             "numeric"             "numeric" 
    ## promotion_last_5years                salary 
    ##             "numeric"              "factor"

``` r
# convert the variables promotion_last_5years and left into factors
model_data <- model_data %>% 
  mutate(left = as.factor(left)) %>% 
  mutate(promotion_last_5years = as.factor(promotion_last_5years))

# print first six rows
head(model_data)
```

    ## # A tibble: 6 × 5
    ##   left  satisfaction_level average_montly_hours promotion_last_5years salary
    ##   <fct>              <dbl>                <dbl> <fct>                 <fct> 
    ## 1 1                   0.38                  157 0                     low   
    ## 2 1                   0.8                   262 0                     medium
    ## 3 1                   0.11                  272 0                     medium
    ## 4 1                   0.72                  223 0                     low   
    ## 5 1                   0.37                  159 0                     low   
    ## 6 1                   0.41                  153 0                     low

``` r
# check variable types
sapply(model_data, class)
```

    ##                  left    satisfaction_level  average_montly_hours 
    ##              "factor"             "numeric"             "numeric" 
    ## promotion_last_5years                salary 
    ##              "factor"              "factor"

``` r
# check correct data types 
str(model_data)
```

    ## tibble [14,999 × 5] (S3: tbl_df/tbl/data.frame)
    ##  $ left                 : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ satisfaction_level   : num [1:14999] 0.38 0.8 0.11 0.72 0.37 0.41 0.1 0.92 0.89 0.42 ...
    ##  $ average_montly_hours : num [1:14999] 157 262 272 223 159 153 247 259 224 142 ...
    ##  $ promotion_last_5years: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ salary               : Factor w/ 3 levels "high","low","medium": 2 3 3 2 2 2 2 2 2 2 ...

-   **Creating dummies for the variable salary**

``` r
# loading fastDummies package for efficient dummy variable creation
library(fastDummies)

# Create dummies using the fastDummies package
model_data <- dummy_cols(model_data, select_columns = "salary")
head(model_data)
```

    ## # A tibble: 6 × 8
    ##   left  satisfaction_level average_mont…¹ promo…² salary salar…³ salar…⁴ salar…⁵
    ##   <fct>              <dbl>          <dbl> <fct>   <fct>    <int>   <int>   <int>
    ## 1 1                   0.38            157 0       low          0       1       0
    ## 2 1                   0.8             262 0       medium       0       0       1
    ## 3 1                   0.11            272 0       medium       0       0       1
    ## 4 1                   0.72            223 0       low          0       1       0
    ## 5 1                   0.37            159 0       low          0       1       0
    ## 6 1                   0.41            153 0       low          0       1       0
    ## # … with abbreviated variable names ¹​average_montly_hours,
    ## #   ²​promotion_last_5years, ³​salary_high, ⁴​salary_low, ⁵​salary_medium

``` r
# modify the created dummies into factors 
model_data <- model_data %>% 
  mutate(salary_high = as.factor(salary_high),
         salary_low = as.factor(salary_low),
         salary_medium = as.factor(salary_medium)) 

# print first six rows
head(model_data)
```

    ## # A tibble: 6 × 8
    ##   left  satisfaction_level average_mont…¹ promo…² salary salar…³ salar…⁴ salar…⁵
    ##   <fct>              <dbl>          <dbl> <fct>   <fct>  <fct>   <fct>   <fct>  
    ## 1 1                   0.38            157 0       low    0       1       0      
    ## 2 1                   0.8             262 0       medium 0       0       1      
    ## 3 1                   0.11            272 0       medium 0       0       1      
    ## 4 1                   0.72            223 0       low    0       1       0      
    ## 5 1                   0.37            159 0       low    0       1       0      
    ## 6 1                   0.41            153 0       low    0       1       0      
    ## # … with abbreviated variable names ¹​average_montly_hours,
    ## #   ²​promotion_last_5years, ³​salary_high, ⁴​salary_low, ⁵​salary_medium

-   **Drop the column salary**

``` r
# deselect the column salary
model_data <- model_data %>% 
  dplyr::select(-salary)

# Check for correct data types 
str(model_data)
```

    ## tibble [14,999 × 7] (S3: tbl_df/tbl/data.frame)
    ##  $ left                 : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ satisfaction_level   : num [1:14999] 0.38 0.8 0.11 0.72 0.37 0.41 0.1 0.92 0.89 0.42 ...
    ##  $ average_montly_hours : num [1:14999] 157 262 272 223 159 153 247 259 224 142 ...
    ##  $ promotion_last_5years: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ salary_high          : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ salary_low           : Factor w/ 2 levels "0","1": 2 1 1 2 2 2 2 2 2 2 ...
    ##  $ salary_medium        : Factor w/ 2 levels "0","1": 1 2 2 1 1 1 1 1 1 1 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

### Creating training and test samples

-   This step involves splitting the dataset into a training set to
    train the model on and a testing set to test the model on.

``` r
# To make my results reproducible
set.seed(1)

# Utilize 70 percent of the dataset as training set, and the remaining 30 percent as testing set.
sample <- sample(c(T, F), nrow(model_data), replace = T, prob = c(0.7, 0.3))

# assign train set to train
train <- model_data[sample, ]

#assign test set to test
test <- model_data[!sample, ]
```

### Training the model

``` r
# fitting the logistic regression model. I omit one dummy variable (salary_medium) to avoid the dummy variable trap
logistic_model <- glm(left ~ satisfaction_level + average_montly_hours + promotion_last_5years + salary_high + salary_low, data = train, family = "binomial")

# Disable the scientific notation for model summary
options(scipen = 999)

# call model summary
summary(logistic_model)
```

    ## 
    ## Call:
    ## glm(formula = left ~ satisfaction_level + average_montly_hours + 
    ##     promotion_last_5years + salary_high + salary_low, family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6359  -0.7018  -0.4813  -0.1888   2.7585  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value             Pr(>|z|)    
    ## (Intercept)             0.2300893  0.1214909   1.894               0.0582 .  
    ## satisfaction_level     -3.7577705  0.1058720 -35.494 < 0.0000000000000002 ***
    ## average_montly_hours    0.0026961  0.0004882   5.523         0.0000000334 ***
    ## promotion_last_5years1 -1.2414339  0.2807097  -4.422         0.0000097573 ***
    ## salary_high1           -1.3149267  0.1456135  -9.030 < 0.0000000000000002 ***
    ## salary_low1             0.4807762  0.0526416   9.133 < 0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 11508.1  on 10474  degrees of freedom
    ## Residual deviance:  9617.1  on 10469  degrees of freedom
    ## AIC: 9629.1
    ## 
    ## Number of Fisher Scoring iterations: 5

### Calculating marginal effects

``` r
# loading the mfx package to calculate marginal effects
library(mfx)

# Calculate coefficient marginal effects 
marginals <- logitmfx(logistic_model, data = train)

# print marginals 
marginals
```

    ## Call:
    ## logitmfx(formula = logistic_model, data = train)
    ## 
    ## Marginal Effects:
    ##                               dF/dx    Std. Err.        z                 P>|z|
    ## satisfaction_level     -0.585600940  0.016121827 -36.3235 < 0.00000000000000022
    ## average_montly_hours    0.000420151  0.000076546   5.4888     0.000000040456854
    ## promotion_last_5years1 -0.131127097  0.017862274  -7.3410     0.000000000000212
    ## salary_high1           -0.143944516  0.010079452 -14.2810 < 0.00000000000000022
    ## salary_low1             0.075207864  0.008243888   9.1229 < 0.00000000000000022
    ##                           
    ## satisfaction_level     ***
    ## average_montly_hours   ***
    ## promotion_last_5years1 ***
    ## salary_high1           ***
    ## salary_low1            ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## dF/dx is for discrete change for the following variables:
    ## 
    ## [1] "promotion_last_5years1" "salary_high1"           "salary_low1"

### Simplified table of the results

``` r
# combining results from the two outputs 
results <- data.frame(variable = c("constant", "satisfaction_level", "average_montly_hours", "promotion_last_5years", "salary_high", "salary_low"),
                      coefficient_estimate = c( 0.230089265, -3.757770499, 0.002696088, -1.241433934, -1.314926658,  0.480776151),
                      marginal_effect = c(NA, -0.585600940, 0.000420151, -0.131127097,  -0.143944516,  0.075207864),
                      P_value = c(0.0582, 0.0000000000000002, 0.0000000334, 0.0000097573, 0.0000000000000002, 0.0000000000000002))

# print results
results
```

    ##                variable coefficient_estimate marginal_effect            P_value
    ## 1              constant          0.230089265              NA 0.0582000000000000
    ## 2    satisfaction_level         -3.757770499    -0.585600940 0.0000000000000002
    ## 3  average_montly_hours          0.002696088     0.000420151 0.0000000334000000
    ## 4 promotion_last_5years         -1.241433934    -0.131127097 0.0000097573000000
    ## 5           salary_high         -1.314926658    -0.143944516 0.0000000000000002
    ## 6            salary_low          0.480776151     0.075207864 0.0000000000000002

### Results Interpretation

-   A coefficient in a logit model tells us the change in the log of the
    odds ratio per unit change in the independent variable concerned
    from its mean.

-   Marginal effect of an independent variable gives us the change in
    the expected value of (Di) caused by a one unit increase in X1i
    holding constant the other independent variables in the equation.

**We can therefore interpret the above logit model as follows:**

1.  Holding all other factors constant, every unit increase in an
    employee’s satisfaction level decreases the log of the odds ratio
    (logit) of leaving the company by 3.757770499. Additionally, a unit
    increase in satisfaction level reduces the probability of an
    employee leaving the company by 0.585600940 (58.56%), holding other
    factors constant. `satisfaction_level` is statistically significant
    at 5% level of significance, therefore, a good predictor of employee
    retention.

2.  Holding all other factors constant, every unit increase in an
    employee’s average monthly hours increases the log of the odds ratio
    of leaving the company by 0.002696088. Also every unit increase of
    an employee’s average monthly hours increases the probability of
    leaving the company by 0.000420151 (0.042%), holding other factors
    constant. `average_monthly_hours` is statistically significant at 5%
    level of significance, therefore, a good predictor of employee
    retention.

3.  Holding all other factors constant, employees who have received a
    promotion in the last 5 years have a lower logit of leaving the
    company by 1.241433934. Additionally, their probability of leaving
    the company reduces by 0.131127097 (13.11%), ceteris paribus.
    `promotion_last_5years` is statistically significant at 5% level of
    significance, therefore, a good predictor of employee retention.

4.  Holding all other factors constant, employees who receive a high
    salary have a lower logit of leaving the company by 1.314926658,
    their probability of leaving the company reduces by 0.143944516
    (14.39%). `salary_high` is statistically significant at 5% level of
    significance, therefore, a good predictor of employee retention.

5.  Holding all other factors constant, employees who receive a low
    salary have a higher logit of leaving the company by 0.480776151,
    their probability of leaving the company increases by 0.075207864
    (7.52%). `salary_low` is statistically significant at 5% level of
    significance, therefore, a good predictor of employee retention.

### Assessing Model Fit/Goodness of Fit

-   R2 is a metric that we use to measure the goodness of fit of a model
    in a typical regression. However, in Logistic Regression, there is
    no such R2 value. As an alternative, we calculate the McFadden’s R2,
    which has a range of 0 to just below 1. Values that are very close
    to 0 show that the model has no forecasting ability. Values over
    0.40 often signify that a model fits the data well.

-   To compute the McFadden’s R2, we use the `pR2 ()` from the `pscl`
    package.

``` r
# Calculate the pR2
pscl::pR2(logistic_model)["McFadden"]
```

    ## fitting null model for pseudo-r2

    ##  McFadden 
    ## 0.1643235

``` r
# Alternatively, using base R functions
with(summary(logistic_model), 1-deviance/null.deviance)
```

    ## [1] 0.1643235

-   The above output means that pseudo R2 is 0.1643235. In simple terms,
    `satisfaction_level`, `average_montly_hours`,
    `promotion_last_5years` and `salary` can explain for about 16.43
    percent of the probability of an employee leaving the company,
    leaving the rest (83.57 percent) to be explained by other variables
    not in the model. This pseudo R2 square is quite low indicating poor
    predictive power of the model. This calls for model improvement, but
    that is not the major interest in this project.

### Assessing Variable importance

-   Using the `varImp()` function from the `caret` package, I can also
    determine the importance of each explanatory variable used in the
    model.

``` r
# Calculate variable importance
caret::varImp(logistic_model)
```

    ##                          Overall
    ## satisfaction_level     35.493516
    ## average_montly_hours    5.522690
    ## promotion_last_5years1  4.422483
    ## salary_high1            9.030253
    ## salary_low1             9.133005

-   Greater values denote higher importance. Additionally, the values
    should coincide with the model’s P-values. In my situation,
    satisfaction level comes in as the clear leader, followed by salary,
    average monthly hours, and promotion last 5years.

### Checking for Multicollinearity

-   In a multiple regression analysis, multicollinearity is said to
    occur when there is a correlation between several independent
    variables, as per Kim (2019). To determine whether multicollinearity
    is an issue in this scenario, I will utilize the Variance Inflation
    Factor (VIF), a measure of the degree of multicollinearity in
    regression.

``` r
# utilizing the vif function from the car package 
car::vif(logistic_model)
```

    ##    satisfaction_level  average_montly_hours promotion_last_5years 
    ##              1.005144              1.002352              1.002419 
    ##           salary_high            salary_low 
    ##              1.049310              1.051234

-   A VIF score above 4 or 5 often denotes strong multicollinearity
    between an explanatory variable and other explanatory variables. An
    explanatory variable does not have a correlation with other
    explanatory variables if its value is 1.

-   Given that none of my explanatory variables have a VIF greater than
    4, I can infer that multicollinearity is not a problem in my model.

### Use the fitted model to make predictions

-   Having fitted the model, I can utilize it to predict whether an
    employee will leave the company based on their satisfaction level,
    average monthly hours, promotion status, and salary category.

``` r
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
```

    ##   satisfaction_level average_montly_hours promotion_last_5years salary_high
    ## 1               0.64                  250                     1           0
    ## 2               0.64                  250                     0           1
    ##   salary_low salary_medium
    ## 1          1             0
    ## 2          0             0

``` r
# predict probability of leaving
predict(logistic_model, new, type = "response")
```

    ##          1          2 
    ## 0.09436042 0.05647693

-   The probability of leaving of an employee with a satisfaction level
    of 0.64, 250 average monthly hours, has received a promotion in the
    last five years and earns a low salary is 0.09436042 (about 9.44
    percent). Conversely, the probability of leaving of an employee with
    the same satisfaction level and average monthly hours, has not
    received a promotion in the last 5 years and earns a high salary is
    0.05647693 (about 5.65 percent).

-   I can there use the model to calculate the probability of leaving of
    each employee in the test data created in an earlier stage.

``` r
# calculate probabilities using the test data
predicted <- predict(logistic_model, test, type = "response")

# view the first 10 probabilities
head(predicted, 10)
```

    ##         1         2         3         4         5         6         7         8 
    ## 0.1988493 0.3971585 0.7312595 0.4322768 0.3661528 0.1776000 0.1917521 0.7422713 
    ##         9        10 
    ## 0.3811379 0.1440041

### Model Diagnostics

-   It is now time to analyze how well the model performs on the test
    data.Any employee in the test data whose likelihood is 0.5 or above
    will automatically be predicted to leave the company. Using the
    `OptimalCutoff()` function from the `InformationValue` Package, I
    can, however, determine the optimal probability to employ in order
    to optimize the model’s accuracy.

``` r
# Loading the Information value package
library(InformationValue)

# find the optimal cutoff probability to use
optimal <- optimalCutoff(test$left, predicted)[1]

# print optimal 
optimal
```

    ## [1] 0.6200252

-   The results show that 0.6200252 is the appropriate probability
    limit. An employee with a predicted likelihood of at least 0.6200252
    will almost certainly quit the organization, whereas one with a
    forecasted probability of less than 0.6200252 will almost certainly
    stay on.

-   Using the function `misClassError()` from the `InformationValue`
    package, I can also determine the overall misclassification error,
    which is the proportion of all incorrect classifications.

``` r
# calculate misclassification error
misClassError(test$left, predicted, threshold = optimal)
```

    ## [1] 0.1954

-   19.54 percent is the overall misclassification rate for my model. In
    broad sense, the lower the misclassification rate,the better. It
    denotes that the model is capable of predicting the results (whether
    an employee will leave or not).

### Receiver Operating Characteristic Curve

Last but not least, I can plot the ROC curve, which shows the proportion
of true positives the model correctly predicts when the prediction
probability cutoff is dropped from 1 to 0. The higher the area under the
curve (AUC), the more accurately the model is able to predict outcomes
(Kim & Hwang, 2020). Once more, this phase requires the use of the
`InformationValue` package.

``` r
# plot the ROC curve
plotROC(test$left, predicted)
```

![](Project_Report_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

-   The area under the curve is 0.7682, which is a high value, according
    to the graph above. This shows that my logistic regression model is
    effective at predicting whether or not an employee would leave the
    organization.

## Recommendations

According to the findings above, job satisfaction is by far the most
crucial aspect to take into account when developing changes that promote
staff retention. In fact, the investigation revealed that a unit
increase in an employee’s level of satisfaction will lower that
employee’s likelihood of quitting the organization by 58.56%. The
management of this organization may increase employee happiness by
paying competitive salaries, acknowledging achievements, listening to
problems, providing more incentives, and being open and honest. The
management also need to be cognizant of the time that workers spend
working for the organization. Employees should have adequate time for
personal growth. Besides, overworking them just makes them more likely
to leave the organization. Finally, the organization should have a
defined promotion process in place so that staff members never have to
wonder when their next pay raise could be coming.

## References

-   Kim, J. H. (2019). Multicollinearity and misleading statistical
    results. *Korean journal of anesthesiology*, 72(6), 558-569.

-   Kim, J., & Hwang, I. C. (2020). Drawing guidelines for receiver
    operating characteristic curve in preparation of manuscripts.
    *Journal of Korean Medical Science*, 35(24).

## Contact Me

[WhatsApp](https://wa.me/+254778988313)

[Follow me on Twitter](https://twitter.com/mwangi__george)

[Lets connect on
LinkedIn](https://www.linkedin.com/in/georgemwangikenya)
