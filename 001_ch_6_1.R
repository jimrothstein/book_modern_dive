##  modern dive,  001_ch_6_1.R

library(ggplot2)
library(dplyr)
library(moderndive)
library(gapminder)
library(skimr)


evals_ch6 <- evals %>%
    select(score, bty_avg, age)

## examine 5 
set.seed(2019)
evals_ch6 %>% 
    dplyr::sample_n(5)

## summarize
evals_ch6 %>% 
    select(score, bty_avg) %>% 
    skimr::skim()

## correlation , 3 ways

## tibble
evals_ch6 %>% 
    select(score, bty_avg) %>% 
    moderndive::get_correlation(formula=score~bty_avg)

cor(evals_ch6$score,evals$bty_avg)

##  list ("matrix" )
evals_ch6 %>% 
    select(score, bty_avg) %>% 
    cor()
##

##  plot, add jitter, simple line (no se)

## try 1
ggplot(evals_ch6, aes(x = jitter(bty_avg), y = jitter(score) )) +
    geom_point() +
    labs(x = "Beauty Score", y = "Teaching Score", 
         title = "Relationship of teaching and beauty scores") +
    geom_smooth(method="lm", se=FALSE)

## try 2 (better)

ggplot(evals_ch6, aes(x = bty_avg, y = score )) +
    geom_jitter(position = "jitter") +
    labs(x = "Beauty Score", y = "Teaching Score", 
         title = "Relationship of teaching and beauty scores") +
    geom_smooth(method="lm", se=FALSE)



## regression
model <- lm(score ~ bty_avg, data = evals_ch6)
model
summary(model)

## nicer format for se, p
moderndive::get_regression_table(model)


## 6.1.3

## add prediction
(y_hat <- coef(model)[1] + coef(model)[2]*evals_ch6$bty_avg)

evals_ch6   <- tibble::add_column(evals_ch6, y_hat)

## nicer (adds predict & residual)
regression_points <- get_regression_points(model)

## plot residuals (sortof normal?)
regression_points %>%
    ggplot(aes(residual)) +
    geom_histogram()


## BEGin 6.2
