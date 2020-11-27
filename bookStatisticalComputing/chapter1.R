# In this script i go to write the examples from book:
# Statistical Computing with R (second edition)

# Formula
formula0 <- lm(rock$peri ~ 0) # null model
formula1 <- lm(rock$peri ~ 1) # only intercept
formula2 <- lm(rock$peri ~ rock$area) # both parameters
formula3 <- lm(rock$peri ~ 0 + rock$area) # without intercept
formula4 <- lm(rock$peri ~ 1 + rock$area) # both parameters
summary(formula0)
summary(formula1)
summary(formula2)
summary(formula3)
summary(formula4)
