library(readr)
library(ggplot2)
library(dplyr)

(cars <- read_csv('cars.csv'))

# Learn data structure
glimpse(cars)

# Plot a dotplot
ggplot(cars, aes(x = weight)) +
  geom_dotplot(dotsize = 0.4)

# Plot a histogram
ggplot(cars, aes(x = weight)) +
  geom_histogram()

# Plot a density plot
ggplot(cars, aes(x = weight)) +
  geom_density()

# Plot a boxplot
ggplot(cars, aes(x = 1, y = weight)) +
  geom_boxplot() +
  coord_flip()

# Side-by-side boxplot
cars %>% 
  filter(ncyl %in% c(4, 6, 8)) %>%
  ggplot(aes(x = as.factor(ncyl), y = city_mpg)) +
  geom_boxplot()

# Create overlaid density plots 
cars %>% 
  filter(ncyl %in% c(4, 6, 8)) %>%
  ggplot(aes(x = city_mpg, fill = as.factor(ncyl))) +
  geom_density(alpha = .3)

# Faceted histogram
ggplot(cars, aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_wrap(~pickup)

# Marginal distribution
ggplot(cars, aes(x = hwy_mpg)) +
  geom_histogram()

# Only engines that are smaller than 2 liters
cars %>%
  filter(eng_size < 2) %>%
  ggplot(aes(hwy_mpg)) +
  geom_histogram() +
  ggtitle('Distribution of hwy_mpg for small engines')


cars %>%
  filter(eng_size < 2) %>%
  ggplot(aes(hwy_mpg)) +
  geom_histogram(binwidth = 5)

cars %>%
  filter(eng_size < 2) %>%
  ggplot(aes(hwy_mpg)) +
  geom_histogram(binwidth = 0.4) +
  ggtitle('binwidth = 0.4')

cars %>%
  filter(eng_size < 2) %>%
  ggplot(aes(hwy_mpg)) +
  geom_density()

cars %>%
  filter(eng_size < 2) %>%
  ggplot(aes(hwy_mpg)) +
  geom_density(bw = 5)

# Visualization in higher dimensions
ggplot(cars, aes(x = msrp)) + 
  geom_density() +
  facet_grid(pickup ~ rear_wheel)

ggplot(cars, aes(x = msrp)) + 
  geom_density() +
  facet_grid(pickup ~ rear_wheel, labeller = label_both)

table(cars$rear_wheel, cars$pickup)

# Numerical Summary of Continuous Data

# Mean
(x <- head(cars$eng_size, 11))
sum(x)/11
mean(x)

# Median
sort(x)
median(x)

# Mode
# Plot a dotplot
ggplot(cars[1:11,], aes(x = eng_size)) +
  geom_dotplot(dotsize = 3)

table(x)

table(cars$ncyl)

cars %>%
  mutate(small_cyl = ncyl %in% c(3, 4)) %>%
  group_by(small_cyl) %>%
  summarise(mean(eng_size),
            median(eng_size))

cars %>%
  mutate(small_cyl = ncyl %in% c(3, 4)) %>%
  ggplot(aes(x = small_cyl, y = eng_size)) +
  geom_boxplot()

# Measures of variability
x - mean(x)

sum(x - mean(x))
sum((x - mean(x))^2)
sum((x - mean(x))^2)/11
sum((x - mean(x))^2)/(11-1)
var(x)

sd(x)

# Interquartile range
summary(x)
IQR(x)

min(x)

# Range 
diff(range(x))

cars %>%
  group_by(ncyl) %>%
  summarize(sd(eng_size),
            IQR(eng_size),
            n())

ggplot(cars, aes(x = eng_size, fill = as.factor(ncyl))) +
  geom_density(alpha = 0.3)

# Shape and transformations
ggplot(cars, aes(x = dealer_cost)) + # Distribution of delaer_cost is right skewed
  geom_density()

ggplot(cars, aes(x = dealer_cost, fill = all_wheel)) +
  geom_density(alpha = .3)

ggplot(cars, aes(x = log(dealer_cost), fill = all_wheel)) +
  geom_density(alpha = .3)

ggplot(cars, aes(x = dealer_cost, fill = all_wheel)) +
  geom_density(alpha = .3) +
  scale_x_log10()

ggplot(cars, aes(x = all_wheel, y = dealer_cost)) +
  geom_boxplot() +
  coord_flip()


cars %>%
  count(all_wheel)

# Indicating outlier
cars %>%
  mutate(is_outlier = dealer_cost > 60000) %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = dealer_cost, fill = all_wheel)) +
  geom_density(alpha = .3)

# Another way of filtering non-outliers
cars %>%
  filter(dealer_cost < 60000) %>%
  ggplot(aes(x = dealer_cost, fill = all_wheel)) +
  geom_density(alpha = .3)

# Filter only outliers
cars %>%
  mutate(is_outlier = dealer_cost > 60000) %>%
  filter(is_outlier) %>%
  arrange(desc(dealer_cost)) %>%
  summarise(sum(sports_car), sum(all_wheel))

sum(cars$sports_car)
