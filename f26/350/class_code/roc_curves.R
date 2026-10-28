
## Need to install these
install.packages(c("pROC"))
install.packages(c("plotROC"))

crab <- read.csv("https://collinn.github.io/data/crabs.csv")

library(ggplot2)
library(dplyr)
library(plotROC)
library(pROC)



fit <- glm(y~ width, family = binomial,data = crab)
crab$fitted <- fitted(fit)

x <- seq(min(crab$width), max(crab$width), by = 0.01)
y <- predict(fit, newdata = data.frame(width = x), type = "resp")
df <- data.frame(fitted = y, width = x)

ggplot(crab, aes(width, y)) + 
  geom_jitter(width = 0,height = 0.02) +
  geom_line(data = df, aes(x = width, y = fitted), 
            color = "steelblue")


crab$pred70 <- (crab$fitted > 0.7)*1

## Confusionmatrix
(xx <- xtabs(~ y + pred70,data = crab))


## proc
rocp <- roc(y ~ fitted, data = crab)
with(rocp, cbind(thresholds, specificities, sensitivities)) |> tail()

plot.roc(rocp, legacy.axes = TRUE,
         xlim = c(1,0))
ggplot(crab) + geom_roc(aes(d = y, m = fitted)) +
  labs(y = "Sensitivity", x = "1 - Specificity")


