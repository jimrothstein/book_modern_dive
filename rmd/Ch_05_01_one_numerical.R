# 05_01_one_numerical.R

# Basic Regression {#regression}

library(tidyverse)
library(moderndive)
library(skimr)
library(gapminder)



# Packages needed internally, but not in text.
qibrary(mvtnorm)
library(broom)
library(kableExtra)
library(patchwork)


evals_ch5 <- evals %>%
  select(ID, score, bty_avg, age)

glimpse(evals_ch5)

evals_ch5 %>%
  sample_n(size = 5)


evals_ch5 %>%
  summarize(mean_bty_avg = mean(bty_avg), mean_score = mean(score),
            median_bty_avg = median(bty_avg), median_score = median(score))

evals_ch5 %>% select(score, bty_avg) %>% skim()


}

evals_ch5 %>% 
  get_correlation(formula = score ~ bty_avg)

evals_ch5 %>% 
  summarize(correlation = cor(score, bty_avg))

cor_ch5 <- evals_ch5 %>%
  summarize(correlation = cor(score, bty_avg)) %>% 
  round(3) %>% 
  pull()

ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score", 
       y = "Teaching Score",
       title = "Scatterplot of relationship of teaching and beauty scores")



### Simple linear regression {#model1table}

# Fit regression model:
score_model <- lm(score ~ bty_avg, data = evals_ch5)
# Get regression table:
get_regression_table(score_model)

score_model <- lm(score ~ bty_avg, data = evals_ch5)
evals_line <- score_model %>%
  get_regression_table() %>%
  pull(estimate)



# Fit regression model:
score_model <- lm(score ~ bty_avg, data = evals_ch5)
# Get regression tabl


get_regression_table(score_model)


t  <- broom::augment(score_model)

# Ch5.3.2
# sum of residuals^2    #132
# ====================
sum((t$.resid)^2)


summary(score_model)

# Conf Intervals of coeficients
# ===============================
ans <- tidy(score_model, conf.int = TRUE)
ans[,4:7]


# ALTERNATIV:  Find B0, B1
# ==========================
# Need corr, sy, sx
cor  <- cor(t$score, t$bty_avg)
cor

s_x = sd(t$bty_avg)
s_y  <- sd(t$score)
B1  <-  cor*(s_y/s_x)
B1   # 0.0666  (correct!)
