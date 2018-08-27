library(gapminder)
library(ggplot2)
library(dplyr)

head(gapminder)

# Change year variable into factor
gapminder <- gapminder %>%
  mutate(year = as.factor(year))

dim(gapminder)  #1704 6

levels(gapminder$continent)

(summary <- gapminder %>%
    group_by(continent, year) %>%
    summarize(n()))

# Contingency Table
table(gapminder$continent, gapminder$year)

(summary <- gapminder %>%
    group_by(continent) %>%
    summarize(n()))

# or
gapminder %>%
  count(continent)

# or 
table(gapminder$continent)

# or
summary(gapminder$continent)

ggplot(gapminder, aes(x = continent, fill = year)) +
  geom_bar()


options(scipen = 999, digits = 3) # Simplify display format

# Proportions
tab_cnt <- table(gapminder$continent, gapminder$year)
prop.table(tab_cnt)

sum(prop.table(tab_cnt))

prop.table(tab_cnt, 1)

prop.table(tab_cnt, 2)


ggplot(gapminder, aes(x = continent, fill = year)) +
  geom_bar(position = 'fill') + 
  ylab('proportion')

# Marginal distribution
table(gapminder$continent)

ggplot(gapminder, aes(x = continent)) +
  geom_bar()

gapminder %>%
  group_by(continent) %>%
  summarise(mean = mean(lifeExp)) %>%
  ggplot(aes(x = continent, y = mean)) +
  geom_col()

# Faceting
ggplot(gapminder, aes(x = continent)) + 
  geom_bar() +
  facet_wrap( ~ year)

##### Different Number of Values #####
comics <- as_tibble(read.csv('Comics.csv'))

head(comics)

comics %>%
  group_by(id, align) %>%
  summarize(n())

levels(comics$id)

levels(comics$align)

tab1 <- table(comics$id, comics$align)
prop.table(tab1)

prop.table(tab1, 1)
prop.table(tab1, 2)

ggplot(comics, aes(x = id, fill = align)) + 
  geom_bar()

ggplot(comics, aes(x = id, fill = align)) +
  geom_bar(position = 'fill') +
  ylab('proportions')

# condition on alignment
ggplot(comics, aes(x = align, fill = id)) +
  geom_bar(position = 'fill') +
  ylab('proportions')

# marginal distribution
table(comics$id)

ggplot(comics, aes(x = id)) +
  geom_bar()


ggplot(comics, aes(x = id)) +
  geom_bar() + 
  facet_wrap(~ align)

levels(comics$gender)

table(comics$align, comics$gender)

# Drop levels
comics <- comics %>%
  filter(align != "Reformed Criminals") %>%
  droplevels()

tab <- table(comics$align, comics$gender)

# Stacked bar chart
ggplot(comics, aes(x = align, fill = gender)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(comics, aes(x = align, fill = gender)) + 
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90))

ggplot(comics, aes(x = gender, fill = align)) + 
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90))

prop.table(tab)
prop.table(tab, 2)

ggplot(comics, aes(x = align, fill = gender)) + 
  geom_bar(position = 'fill') +
  ylab('proportion')

# Change the order of the levels in align
comics$align <- factor(comics$align, 
                       levels = c("Bad", "Neutral", "Good"))


ggplot(comics, aes(x = align)) +
  geom_bar()

# Create a barchart of align faceted by gender
ggplot(comics, aes(x = align)) +
  geom_bar() + 
  facet_wrap(~ gender)
