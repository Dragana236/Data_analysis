library(openintro)
library(dplyr)
library(ggplot2)

email <- openintro::email
email <- email %>% as_tibble() %>% 
  mutate(spam = as.factor(spam))

email

# Spam and num_char
# Compute summary statistics
email %>%
  group_by(spam) %>%
  summarise(median(num_char),
            IQR(num_char))

email %>%   #skewed distribution
  ggplot(aes(x = num_char)) +
  geom_density()


email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) +
  geom_boxplot()

email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = log_num_char, fill = spam)) +
  geom_density(alpha = 0.3)

# Spam and !!!
# Compute center and spread for exclaim_mess by spam
email %>%   # highly skewed distribution
  ggplot(aes(x = exclaim_mess)) +
  geom_density()

email %>%
  group_by(spam) %>%
  summarise(median(exclaim_mess),
            IQR(exclaim_mess))


email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = log_exclaim_mess)) +
  geom_histogram() +
  facet_wrap(~spam)


# Alternative plot: side-by-side box plots
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = 1, y = log_exclaim_mess)) +
  geom_boxplot() +
  facet_wrap(~spam)

# or
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = spam, y = log_exclaim_mess)) +
  geom_boxplot()

# Alternative plot: Overlaid density plots
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = log_exclaim_mess, fill = spam)) +
  geom_density(alpha = .3)

# Zero inflation strategies
email %>%
  mutate(zero = exclaim_mess == 0) %>%
  ggplot(aes(x = zero)) +
  geom_bar() +
  facet_wrap(~ spam)

email1 <- email %>%
  mutate(zero = exclaim_mess == 0)

email1 %>%  # still not factor
  select(zero) %>%
  is.factor()

# Convert zero variable to factor
email1 <- email1 %>%
  mutate(zero = factor(zero, levels = c('TRUE', 'FALSE')))

email1 %>%
  ggplot(aes(x = zero)) +
  geom_bar() +
  facet_wrap(~spam)


email %>%
  mutate(zero = exclaim_mess == 0) %>%
  ggplot(aes(x = zero, fill = spam)) +
  geom_bar()

email %>%
  mutate(zero = exclaim_mess == 0) %>%
  ggplot(aes(x = zero, fill = spam)) +
  geom_bar(position = 'fill')


# Email and images
table(email$image)

# Create plot of proportion of spam by image
email %>%
  mutate(has_image = image > 0) %>%
  ggplot(aes(x = has_image, fill = spam)) +
  geom_bar(position = 'fill')

email %>%
  mutate(has_image = image > 0) %>%
  ggplot(aes(x = spam, fill = has_image)) +
  geom_bar(position = 'fill')

sum(email$num_char < 0)

# Test if images count as attachments
sum(email$image > email$attach)

# Within non-spam emails, is the typical length of emails 
# shorter for those that were sent to multiple people?
email %>%
    filter(spam == 0) %>%
  group_by(to_multiple) %>%
  summarise(median(num_char))

email %>%
  filter(dollar > 0) %>%
  group_by(spam) %>%
  summarise(median(dollar))

# If we encounter an email with greater than 10 occurrences of the word
# 'dollar', is it more likely to be spam or not-spam? 
email %>%
  filter(dollar > 10) %>%
  ggplot(aes(x = spam)) +
  geom_bar()

# What's in a number?
table(email$number)

levels(email$number)

# Construct plot of number
ggplot(email, aes(x = number)) +
  geom_bar() +
  facet_wrap(~spam)
