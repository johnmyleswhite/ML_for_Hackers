# File-Name:       chapter06.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/oreilly.csv
# Packages Used:   ggplot2, glmnet, tm, boot

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

library('ggplot2')

# First snippet
set.seed(1)

x <- seq(-10, 10, by = 0.01)
y <- 1 - x ^ 2 + rnorm(length(x), 0, 5)

ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) + 
  geom_point() +
  geom_smooth(se = FALSE)

# Second code snippet
x.squared <- x ^ 2

# Third code snippet
ggplot(data.frame(XSquared = x.squared, Y = y), aes(x = XSquared, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Fourth code snippet
summary(lm(y ~ x))$r.squared
#[1] 2.973e-06

summary(lm(y ~ x.squared))$r.squared
#[1] 0.9707

# Fifth code snippet
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)

ggplot(df, aes(x = X, y = Y)) +
  geom_point()

# Sixth code snippet
summary(lm(Y ~ X, data = df))

#Call:
#lm(formula = Y ~ X, data = df)
#
#Residuals:
# Min 1Q Median 3Q Max
#-1.00376 -0.41253 -0.00409 0.40664 0.85874
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.94111 0.09057 10.39 <2e-16 ***
#X -1.86189 0.15648 -11.90 <2e-16 ***
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.4585 on 99 degrees of freedom
#Multiple R-squared: 0.5885, Adjusted R-squared: 0.5843
#F-statistic: 141.6 on 1 and 99 DF, p-value: < 2.2e-16

# Seventh code snippet
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Eighth code snippet
df <- transform(df, X2 = X ^ 2)
df <- transform(df, X3 = X ^ 3)

summary(lm(Y ~ X + X2 + X3, data = df))

#Call:
#lm(formula = Y ~ X + X2 + X3, data = df)
#
#Residuals:
# Min 1Q Median 3Q Max
#-0.32331 -0.08538 0.00652 0.08320 0.20239
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) -0.16341 0.04425 -3.693 0.000367 ***
#X 11.67844 0.38513 30.323 < 2e-16 ***
#X2 -33.94179 0.89748 -37.819 < 2e-16 ***
#X3 22.59349 0.58979 38.308 < 2e-16 ***
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.1153 on 97 degrees of freedom
#Multiple R-squared: 0.9745, Adjusted R-squared: 0.9737
#F-statistic: 1235 on 3 and 97 DF, p-value: < 2.2e-16

# Ninth code snippet
df <- transform(df, X4 = X ^ 4)
df <- transform(df, X5 = X ^ 5)
df <- transform(df, X6 = X ^ 6)
df <- transform(df, X7 = X ^ 7)
df <- transform(df, X8 = X ^ 8)
df <- transform(df, X9 = X ^ 9)
df <- transform(df, X10 = X ^ 10)
df <- transform(df, X11 = X ^ 11)
df <- transform(df, X12 = X ^ 12)
df <- transform(df, X13 = X ^ 13)
df <- transform(df, X14 = X ^ 14)
df <- transform(df, X15 = X ^ 15)

summary(lm(Y ~ X + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14,
           data = df))

#Call:
#lm(formula = Y ~ X + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 +
# X10 + X11 + X12 + X13 + X14, data = df)
#
#Residuals:
# Min 1Q Median 3Q Max
#-0.242662 -0.038179 0.002771 0.052484 0.210917
#
#Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) -6.909e-02 8.413e-02 -0.821 0.414
#X 1.494e+01 1.056e+01 1.415 0.161
#X2 -2.609e+02 4.275e+02 -0.610 0.543
#X3 3.764e+03 7.863e+03 0.479 0.633
#X4 -3.203e+04 8.020e+04 -0.399 0.691
#X5 1.717e+05 5.050e+05 0.340 0.735
#X6 -6.225e+05 2.089e+06 -0.298 0.766
#X7 1.587e+06 5.881e+06 0.270 0.788
#X8 -2.889e+06 1.146e+07 -0.252 0.801
#X9 3.752e+06 1.544e+07 0.243 0.809
#X10 -3.398e+06 1.414e+07 -0.240 0.811
#X11 2.039e+06 8.384e+06 0.243 0.808
#X12 -7.276e+05 2.906e+06 -0.250 0.803
#X13 1.166e+05 4.467e+05 0.261 0.795
#X14 NA NA NA NA
#
#Residual standard error: 0.09079 on 87 degrees of freedom
#Multiple R-squared: 0.9858, Adjusted R-squared: 0.9837
#F-statistic: 465.2 on 13 and 87 DF, p-value: < 2.2e-16

# Tenth code snippet
summary(lm(Y ~ poly(X, degree = 14), data = df))

#Call:
#lm(formula = Y ~ poly(X, degree = 14), data = df)
#
#Residuals:
# Min 1Q Median 3Q Max
#-0.232557 -0.042933 0.002159 0.051021 0.209959
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.010167 0.009038 1.125 0.2638
#poly(X, degree = 14)1 -5.455362 0.090827 -60.063 < 2e-16 ***
#poly(X, degree = 14)2 -0.039389 0.090827 -0.434 0.6656
#poly(X, degree = 14)3 4.418054 0.090827 48.642 < 2e-16 ***
#poly(X, degree = 14)4 -0.047966 0.090827 -0.528 0.5988
#poly(X, degree = 14)5 -0.706451 0.090827 -7.778 1.48e-11 ***
#poly(X, degree = 14)6 -0.204221 0.090827 -2.248 0.0271 *
#poly(X, degree = 14)7 -0.051341 0.090827 -0.565 0.5734
#poly(X, degree = 14)8 -0.031001 0.090827 -0.341 0.7337
#poly(X, degree = 14)9 0.077232 0.090827 0.850 0.3975
#poly(X, degree = 14)10 0.048088 0.090827 0.529 0.5979
#poly(X, degree = 14)11 0.129990 0.090827 1.431 0.1560
#poly(X, degree = 14)12 0.024726 0.090827 0.272 0.7861
#poly(X, degree = 14)13 0.023706 0.090827 0.261 0.7947
#poly(X, degree = 14)14 0.087906 0.090827 0.968 0.3358
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.09083 on 86 degrees of freedom
#Multiple R-squared: 0.986, Adjusted R-squared: 0.9837
#F-statistic: 431.7 on 14 and 86 DF, p-value: < 2.2e-16

# Eleventh code snippet
poly.fit <- lm(Y ~ poly(X, degree = 1), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

poly.fit <- lm(Y ~ poly(X, degree = 3), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

poly.fit <- lm(Y ~ poly(X, degree = 5), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

poly.fit <- lm(Y ~ poly(X, degree = 25), data = df)
df <- transform(df, PredictedY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

# Twelfth code snippet
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

# Thirteenth code snippet
n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

# Fourteenth code snippet
rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

# Fifteenth code snippet
performance <- data.frame()

for (d in 1:12)
{
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)

  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(poly.fit))))

  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Test',
                                  RMSE = rmse(test.y, predict(poly.fit,
                                                              newdata = test.df))))
}

# Sixteenth code snippet
ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()

# Seventeenth code snippet
lm.fit <- lm(y ~ x)
model.complexity <- sum(coef(lm.fit) ^ 2)

# Eighteenth code snippet
lm.fit <- lm(y ~ x)
l2.model.complexity <- sum(coef(lm.fit) ^ 2)
l1.model.complexity <- sum(abs(coef(lm.fit)))

# Ninteenth code snippet
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

# Twentieth code snippet
x <- as.matrix(cbind(x,rev(x)))

library('glmnet')

glmnet(x, y)

#Call: glmnet(x = x, y = y)
#
# Df %Dev Lambda
# [1,] 0 0.00000 0.542800
# [2,] 1 0.09991 0.494600
# [3,] 1 0.18290 0.450700
# [4,] 1 0.25170 0.410600
# [5,] 1 0.30890 0.374200
#...
#[51,] 1 0.58840 0.005182
#[52,] 1 0.58840 0.004721
#[53,] 1 0.58850 0.004302
#[54,] 1 0.58850 0.003920
#[55,] 1 0.58850 0.003571

# Twenty-first code snippet
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

df <- data.frame(X = x, Y = y)

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

# Twenty-second code snippet
library('glmnet')

glmnet.fit <- with(training.df, glmnet(poly(X, degree = 10), Y))

lambdas <- glmnet.fit$lambda

performance <- data.frame()

for (lambda in lambdas)
{
  performance <- rbind(performance,
                       data.frame(Lambda = lambda,
                                  RMSE = rmse(test.y,
                                              with(test.df,
                                                   predict(glmnet.fit,
                                                           poly(X, degree = 10),
                                                           s = lambda)))))
}

# Twenty-third code snippet
ggplot(performance, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line()

# Alternative plot not shown in the book.
ggplot(performance, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  scale_x_log10()

# Twenty-fourth code snippet
best.lambda <- with(performance, Lambda[which(RMSE == min(RMSE))])

glmnet.fit <- with(df, glmnet(poly(X, degree = 10), Y))

# Twenty-fifth code snippet
coef(glmnet.fit, s = best.lambda)

#11 x 1 sparse Matrix of class "dgCMatrix"
# 1
#(Intercept) 0.0101667
#1 -5.2132586
#2 0.0000000
#3 4.1759498
#4 0.0000000
#5 -0.4643476
#6 0.0000000
#7 0.0000000
#8 0.0000000
#9 0.0000000
#10 0.0000000

# Twenty-sixth code snippet
ranks <- read.csv(file.path('data', 'oreilly.csv'),
                  stringsAsFactors = FALSE)

library('tm')

documents <- data.frame(Text = ranks$Long.Desc.)
row.names(documents) <- 1:nrow(documents)

corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(corpus)

# Twenty-seventh code snippet
x <- as.matrix(dtm)
y <- rev(1:100)

# Twenty-eighth code snippet
set.seed(1)

library('glmnet')

# Twenty-ninth code snippet
performance <- data.frame()

for (lambda in c(0.1, 0.25, 0.5, 1, 2, 5))
{
  for (i in 1:50)
  {
    indices <- sample(1:100, 80)
    
    training.x <- x[indices, ]
    training.y <- y[indices]
    
    test.x <- x[-indices, ]
    test.y <- y[-indices]
    
    glm.fit <- glmnet(training.x, training.y)
    
    predicted.y <- predict(glm.fit, test.x, s = lambda)
    
    rmse <- sqrt(mean((predicted.y - test.y) ^ 2))

    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    RMSE = rmse))
  }
}

# Thirtieth code snippet
ggplot(performance, aes(x = Lambda, y = RMSE)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point')

# Thirty-first code snippet
y <- rep(c(1, 0), each = 50)

# Thirty-second code snippet
regularized.fit <- glmnet(x, y, family = 'binomial')

# Thirty-third code snippet
regularized.fit <- glmnet(x, y)

regularized.fit <- glmnet(x, y, family = 'gaussian')

regularized.fit <- glmnet(x, y, family = 'binomial')

# Thirty-fourth code snippet
predict(regularized.fit, newx = x, s = 0.001)
#1 4.884576
#2 6.281354
#3 4.892129
#...
#98 -5.958003
#99 -5.677161
#100 -4.956271

# Thirty-fifth code snippet
ifelse(predict(regularized.fit, newx = x, s = 0.001) > 0, 1, 0)
#1 1
#2 1
#3 1
#...
#98 0
#99 0
#100 0

# Thirty-sixth code snippet
library('boot')

inv.logit(predict(regularized.fit, newx = x, s = 0.001))
#1 0.992494427
#2 0.998132627
#3 0.992550485
#...
#98 0.002578403
#99 0.003411583
#100 0.006989922

# Thirty-seventh code snippet
set.seed(1)

performance <- data.frame()

for (i in 1:250)
{
  indices <- sample(1:100, 80)
  
  training.x <- x[indices, ]
  training.y <- y[indices]
  
  test.x <- x[-indices, ]
  test.y <- y[-indices]
  
  for (lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1))
  {
    glm.fit <- glmnet(training.x, training.y, family = 'binomial')
    
    predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0)
    
    error.rate <- mean(predicted.y != test.y)

    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    ErrorRate = error.rate))
  }
}

# Thirty-eighth code snippet
ggplot(performance, aes(x = Lambda, y = ErrorRate)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point') +
  scale_x_log10()
