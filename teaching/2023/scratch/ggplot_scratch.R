library(ggplot2)

## This does not combine them in legend, which is correct
# and because in aes, it is taking drv from mpg and assigning shape to different categorical
ggplot(mpg, aes(displ, cty, color = class, shape = drv)) + 
  geom_point() + ggtitle("1")

# This should be the same? maybe not in legend though
# and it chooses different values for shape, probably because character
# and also no legend
ggplot(mpg, aes(displ, cty, color = class)) + 
  geom_point(shape = mpg$drv)

# Yes, this is the same. The aes can go wherever
ggplot(mpg, aes(displ, cty)) + 
  geom_point(aes(color = class, shape = drv)) + ggtitle("2")

ggplot(mpg2, aes(displ, cty, shape = factor(year), color = manufacturer)) +
  geom_point() + facet_wrap(class~cyl)

###################


# Inside of aes, color is given a static thing so it scales as a constant
# NOTE: the values in aes have nothing to do with scaled values. "red" and "blue" are basically just treated as factors in aes
ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color = rep(c("red", "blue"), length.out = nrow(mpg))))

# this overrides scaling and demands color be blue
ggplot(mpg, aes(displ, hwy)) + geom_point(color = "blue")
ggplot(mpg, aes(displ, hwy)) + geom_point(color = rep(c("red", "blue"), length.out = nrow(mpg))) # here, they take on the actual colors i tell them to in that order


ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  facet_wrap(~cyl)

ggplot(mpg, aes(displ, hwy)) +
  geom_smooth(span = 0.2)

