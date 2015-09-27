library(car)
library(knitr)
library(Hmisc)

# variance inflation factor


data(mtcars)

names(mtcars)

str(mtcars)

# look at all combinations of variables
pairs(mtcars, main = "mtcars data")

# convert discreat variables to factors
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)


kable(describe(mtcars))



# boxplot of mpg vs. transmission type
boxplot(mpg ~ am, data = mtcars,
        xlab = "Transmission", 
        ylab = "MPG",
        main = "MPG vs Transmission",
        names = c("Automatic", "Manual"))



# simple model using transmission only
fit1 <- lm(mpg ~ am, data = mtcars)
summary(fit1)[4]
coef(summary(fit1))[2,1]

# model using all data as predictors
fit2 = lm(mpg ~ ., data = mtcars)
summary(fit2)[4]
vif(fit2)

# stepwise AIC algorithm
fit3 <- step(lm(mpg ~ ., data = mtcars), trace = 0)
summary(fit3)[4]
vif(fit3)


# compare to just using am
comp <- anova(fit1, fit3)
comp
model <- as.character(fit3$call)[2]

t <- t.test(mpg ~ am, data=mtcars)

par(mfrow=c(2, 2))
plot(fit2)



