

## Datasets available in R
library(help = "datasets")

#### Muscle mass example ####

## Read text file from website
muscle <- fread("http://people.uncw.edu/simmonssj/STT%20412_512/STT%20512/CH01PR27.txt")

## Create column names
colnames(muscle) <- c("mass", "age")

## lm == linear model
?lm

## Syntax is outcome ~ variable
fit <- lm(mass ~ age, data = muscle)

## Look at output
summary(fit)

#### Iris example ####

## Look at dataset
head(iris) # dataset already in R

## Run Model, include extra variables with output ~ var1 + var2
fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
summary(fit)

## Using output ~ . will include all variables in the dataset
fit <- lm(Sepal.Length ~ ., data = iris)
summary(fit) # this adds the 'Species' variable

#### Car example ####

head(mtcars) # dataset in R

## Original
fit1 <- lm(mpg ~ wt, data = mtcars)
summary(fit1)

## Add carb and disp
fit2 <- lm(mpg ~ wt + carb + disp, data = mtcars)
summary(fit2)

## Not discussed in class, but you can use `anova` function to
# perform analysis of variance on linear models
# This helps tell us if one model explains a statistically significant amount of extra variance
# here, we see that the second model does explain more, with a p-value of 0.014
?anova
anova(fit1, fit2)
