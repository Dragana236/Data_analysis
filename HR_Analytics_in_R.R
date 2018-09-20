library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)

# Import the recruitment data
(recruitment <- read_csv('recruitment_data.csv'))

# Get an overview of the recruitment data
summary(recruitment)

recruitment %>%
  count(recruiting_source)

names(recruitment)

# Find the average sales quota attainment for each recruiting source
avg_sales <- recruitment %>%
  group_by(recruiting_source) %>%
  summarise(avg_sales_quota_pct  = mean(sales_quota_pct)) %>%
  arrange(desc(avg_sales_quota_pct))

# Plot the bar chart
avg_sales %>%
  ggplot(aes(x = recruiting_source, y = avg_sales_quota_pct)) +
  geom_col()


# Find the average attrition for each recruiting source, sorted from lowest attrition rate to highest
avg_attrition <- recruitment %>%
  group_by(recruiting_source) %>%
  summarize(attrition_rate = mean(attrition)) %>%
  arrange(desc(attrition_rate))

# Plot the bar chart
avg_attrition %>%
  ggplot(aes(x = recruiting_source, y = attrition_rate)) +
  geom_col()

# Find the hiring source that produces hires with best performance ratings
recruitment %>%
  group_by(recruiting_source) %>%
  summarise(highest_performance = max(performance_rating)) %>%
  arrange(desc(highest_performance))

# What is Driving Low Employee Engagement?
# Import the data
(survey <- read_csv('survey_data.csv'))

# Get an overview of the data
summary(survey)

# Examine the counts of the department variable
survey %>%
  count(department)

# Which departments have the lowest engagement scores?
survey %>%
  group_by(department) %>%
  summarise(avg_engagement = mean(engagement)) %>%
  arrange(avg_engagement)
# Create the disengaged variable and assign the result to survey
survey <- survey %>%
  mutate(disengaged = ifelse(engagement <= 2, 1, 0))
survey

# Summarize the three variables by department
survey_summary <- survey %>%
  group_by(department) %>%
  summarize(pct_disengaged = mean(disengaged),
            avg_salary = mean(salary),
            avg_vacation_days = mean(vacation_days_taken))

# Gather data for plotting
survey_gathered <- survey_summary %>%
  gather(key = 'measure', value = 'value', 2:4)

survey_gathered

# Create three bar charts
survey_gathered %>%
  ggplot(aes(x = measure, y = value, fill = department)) +
  geom_col(position = 'dodge')

# Create three faceted bar charts
survey_gathered %>%
  ggplot(aes(x = measure, y = value, fill = department)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ measure, scales = 'free')

# Add the in_sales variable
survey <- survey %>%
  mutate(in_sales = ifelse(department == 'Sales', 'Sales', 'Other'))

survey

# Test the hypothesis using survey_sales
chisq.test(survey$in_sales, survey$disengaged)

# Test the hypothesis using the survey_sales data
t.test(vacation_days_taken ~ in_sales, data = survey)

# Are new hires getting paid too much?
(pay <- read_csv('fair_pay.csv'))


# Check average salary of new hires and non-new hires
pay %>%
  group_by(new_hire) %>%
  summarize(avg_salary = mean(salary))

# Perform the correct statistical test
t.test(salary ~ new_hire, data = pay) %>%
  tidy()

# Create a stacked bar chart
pay %>%
  ggplot(aes(x = new_hire, fill = job_level)) +
  geom_bar()

# Create a 100% filled stacked bar chart
pay %>%
  ggplot(aes(x = new_hire, fill = job_level)) +
  geom_bar(position = 'fill')

# Calculate the average salary for each group of interest
pay %>%
  group_by(new_hire, job_level) %>%
  summarize(avg_salary = mean(salary)) %>%
  ggplot(aes(x = new_hire, y = avg_salary)) +
  geom_col() +
  facet_wrap(~ job_level)

# Filter the data to include only hourly employees  
pay_filter <- pay %>%
  filter(job_level == 'Hourly')

t.test(salary ~ new_hire, data = pay_filter) %>%
  tidy()

# Run the simple regression

model_simple <- lm(salary ~ new_hire, data = pay)

# Display the summary of model_simple

model_simple %>%
  summary()

# Display a tidy summary
model_simple %>%
  tidy()

# Run the multiple regression
model_multiple <- lm(salary ~ new_hire + job_level, data = pay)

# Display the summary of model_multiple
model_multiple %>%
  summary()

# Display a tidy summary
model_multiple %>%
  tidy()

# Run the multiple regression taking into account departments
model_multiple <- lm(salary ~ new_hire + department, data = pay)

# Display a tidy summary
model_multiple %>%
  tidy()

# Are performance ratings being given consistently?
hr_data <- read_csv('hr_data.csv')

head(hr_data)

performance_data <- read_csv('performance_data.csv')
head(performance_data)

# Join the two tables
joined_data <- left_join(hr_data, performance_data, by = 'employee_id')

head(joined_data)

joined_data %>%
  group_by(gender) %>%
  summarise(avg_rating = mean(rating))

# Add the high_performer column
performance <- joined_data %>%
  mutate(high_performers = ifelse(rating >= 4, 1, 0))

performance

performance %>%
  group_by(gender) %>%
  summarise(avg = mean(high_performers))

chisq.test(performance$gender, performance$high_performers)


# Do the same test, and tidy the output
chisq.test(performance$gender, performance$high_performers) %>%
  tidy()

# Visualize the distribution of high_performer by gender
performance %>%
  ggplot(aes(x = gender, fill = factor(high_performers))) +
  geom_bar(position = 'fill')

# Visualize the distribution of all ratings by gender
performance %>%
  ggplot(aes(x = gender, fill = factor(rating))) +
  geom_bar(position = 'fill')

performance %>%
  ggplot(aes(x = gender, fill = job_level)) +
  geom_bar(position = 'fill')

# Test whether men and women have different job level distributions
chisq.test(performance$gender, performance$job_level)

# Visualize the distribution of high_performer by gender, faceted by job level
performance %>%
  ggplot(aes(x = gender, fill = factor(high_performers))) +
  geom_bar(position = 'fill') + 
  facet_wrap(~ job_level)

join_data2 <- left_join(performance, pay, by = 'employee_id')
head(join_data2)


join_data2 %>%
  ggplot(aes(x = salary, y = high_performers)) +
  geom_point() 

join_data2 %>%
  ggplot(aes(x = salary, y = high_performers)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_smooth(method = 'glm', se = FALSE, 
              color = 'red',
              method.args = list(family = 'binomial'))

# Run a simple logistic regression
logistic_simple <- glm(high_performers ~ gender, 
                       family = 'binomial', data = performance)

logistic_simple %>%
  tidy()

# Run a multiple logistic regression

logistic_multiple  <- glm(high_performers ~ gender + job_level,
                          family = 'binomial',
                          data = performance)

logistic_multiple %>%
  tidy()


# Import the data 
(hr_data <- read_csv('hr_2.csv'))
(accident_data <- read_csv('accident_data.csv'))


hr_joined <- left_join(hr_data, accident_data, 
                       by = c('employee_id', 'year'))


hr_joined <- hr_joined %>%
  mutate(had_accident = ifelse(is.na(accident_type), 0, 1))

head(hr_joined)

hr_joined <- left_join(hr_data, accident_data, 
                       by = c('employee_id', 'year')) %>%
  mutate(had_accident = ifelse(is.na(accident_type), 0, 1))


# Find accident rate for each year
hr_joined %>%
  group_by(year) %>%
  summarise(accident_rate = mean(had_accident))

# Test difference in accident rate between years
chisq.test(hr_joined$year, hr_joined$had_accident) %>%
  tidy()

# Which location had the highest acccident rate?
hr_joined %>% 
  group_by(location) %>%
  summarize(accident_rate = mean(had_accident)) %>%
  arrange(desc(accident_rate))

# Compare annual accident rates by location
accident_rates <- hr_joined %>% 
  group_by(location, year) %>%
  summarize(accident_rate = mean(had_accident)) %>%
  arrange(desc(accident_rate))

accident_rates %>%
  ggplot(aes(x = factor(year), y = accident_rate)) +
  geom_col()+
  facet_wrap(~location)

# Filter out the other locations
southfield <- hr_joined %>%
  filter(location == 'Southfield')


southfield %>%
  group_by(year) %>%
  summarise(average_overtime_hours = mean(overtime_hours))

t.test(overtime_hours ~ year, data = southfield) %>%
  tidy()

# Import the survey data
survey_data <- read_csv('survey_2.csv')

# Create the safety dataset

safety <- left_join(hr_joined, survey_data, by = c("year", "employee_id")) %>%
  mutate(disengaged = ifelse(engagement <= 2, 1, 0)) %>%
  mutate(year = factor(year))

head(safety)  

# Visualize the difference in % disengaged by year in Southfield
southfield %>%
  ggplot(aes(x = year, fill = factor(disengaged))) +
  geom_bar(position = 'fill')

# Test whether one year had significantly more disengaged employees
chisq.test(southfield$year, southfield$disengaged)

# Filter out Southfield
other_locs <- safety %>%
  filter(location != 'Southfield')

# Test whether one year had significantly more overtime hours worked
t.test(overtime_hours ~ year, data = other_locs) %>%
  tidy()

# Test whether one year had significantly more disengaged employees
chisq.test(other_locs$year, other_locs$disengaged)

# Use multiple regression to test the impact of year and disengaged on accident rate in Southfield

regression <- glm(had_accident ~ year + disengaged, family = "binomial", data = southfield) 

# Examine the output
regression %>%
  tidy()
