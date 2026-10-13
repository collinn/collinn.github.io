library(dplyr)

afterlife <- readRDS(url("https://collinn.github.io/data/afterlife_tables.rds"))
full <- afterlife$full

fit <- glm(yes / n ~ gender + race, family = binomial, data = full, weights = n) 
fit2 <- glm(yes / n ~ gender*race, family = binomial, data = full, weights = n) 

anova(fit, fit2, type = "LRT")

## Today :)

crabs <- read.csv("https://collinn.github.io/data/crabs.csv")

## y is yes/no have satellite
fit <- glm(y ~ width, family = binomial, data = crabs)

df <- predict(fit, 
              newdata = data.frame(width = seq(18, 35, by = 0.1)), 
              type = "response")
df <- cbind(y = df, data.frame(width = seq(18, 35, by = 0.1))

library(ggplot2)
ggplot(df, aes(width , y )) + geom_line()

fit2 <- glm(y ~ width + factor(color), family = binomial, data = crabs)

df <- expand.grid(width = seq(18, 34, by = 0.01), 
                  color = 1:4)
df <- cbind(y = predict(fit2, newdata = df, type = "resp"), 
            df)

ggplot(df, aes(width, y, color = factor(color))) + geom_line()

anova(fit, fit2, test = "LRT")


fit3 <- glm(y ~ width + color, family = binomial, data = crabs)

crabs$c4 <- 1 * (crabs$color == 4)

fit4 <- glm(y ~ width + c4, family = binomial, data = crabs)

df <- expand.grid(width = quantile(crabs$width)[c(1, 3, 5)],
                  c4 = c(0, 1, mean(crabs$c4)))

df <- cbind(y = predict(fit4, newdata = df, type = "resp"), 
            df)


