#Chapter_05_02_one_categorical.R

library(tidyverse)
library(moderndive)
library(skimr)
library(gapminder)

library(mvtnorm)
library(broom)
library(kableExtra)
library(patchwork)

# 001	
# ======
gapminder2007 <- gapminder %>%
  filter(year == 2007) %>%
  select(country, lifeExp, continent, gdpPercap)

head(gapminder2007)

lifeExp_by_continent <- gapminder2007 %>%
  group_by(continent) %>%
  summarize(median = median(lifeExp), 
            mean = mean(lifeExp)
	)
	lifeExp_by_continent

	# Ch 5.2.2
	# 002	 MODEL
# =======
	# 1 cat variable, but give all the group offsets
lifeExp_model <- lm(lifeExp ~ continent, data = gapminder2007)
get_regression_table(lifeExp_model)

# model and values for B
lifeExp_model
summary(lifeExp_model)

#1st 5 countries
head(resid(lifeExp_model))



# lm returns list; 
# ===================
mod  <- lifeExp_model


# mod is a list
names(mod)
# combine original data + model
t  <- broom::augment(mod, data=gapminder2007)

t %>% select( country, lifeExp, continent, .fitted, .resid)



