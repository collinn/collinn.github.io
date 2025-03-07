
library(dplyr)
library(ggplot2)


## Question 1
penguins <- read.csv("https://collinn.github.io/data/penguins.csv")

## Using ifelse
penguins <- mutate(penguins, size = ifelse(body_mass_g > median(body_mass_g)), 
                   "Large", "Small")

## Question 3
college <- read.csv("https://collinn.github.io/data/college2019.csv")

## Standardize by region
college <- group_by(college, Region) %>% 
  mutate(act_z = (ACT_median - mean(ACT_median)) / sd(ACT_median))

ggplot(college, aes(Region, act_z)) +
  geom_boxplot()

group_by(college, Region) %>% 
  summarize(min(ACT_median), max(ACT_median))

## Question 4
tips <- read.csv("https://collinn.github.io/data/tips.csv")

tf <- filter(tips, size > 1, day %in% c("Sat", "Sun"))
tf <- mutate(tf, tipPercent = tip / total_bill)

group_by(tf, sex, smoker) %>% 
  summarize(mean(tipPercent), N = n())


## Question 8

cc <- group_by(college, Region) %>% 
  summarize(Adm_Rate = mean(Adm_Rate), 
            Debt_median = mean(Debt_median))

ggplot(college, aes(Adm_Rate, Debt_median)) + 
  geom_point(alpha = 0.25) + 
  geom_point(data = cc, aes(Adm_Rate, Debt_median, fill = Region), 
             size = 4, shape = 21) + 
  theme(legend.position = "bottom")
