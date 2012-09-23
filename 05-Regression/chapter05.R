# File-Name:       chapter05.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/longevity.csv
# Packages Used:   ggplot2

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
ages <- read.csv(file.path('data', 'longevity.csv'))

ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +
  geom_density() +
  facet_grid(Smokes ~ .)

# Second snippet
ages <- read.csv(file.path('data', 'longevity.csv'))

guess <- 73

with(ages, mean((AgeAtDeath - guess) ^ 2))
#[1] 32.991

# Third snippet
ages <- read.csv(file.path('data', 'longevity.csv'))

guess.accuracy <- data.frame()

for (guess in seq(63, 83, by = 1))
{
  prediction.error <- with(ages,
                           mean((AgeAtDeath - guess) ^ 2))
  guess.accuracy <- rbind(guess.accuracy,
                          data.frame(Guess = guess,
                                     Error = prediction.error))
}

ggplot(guess.accuracy, aes(x = Guess, y = Error)) +
  geom_point() +
  geom_line()

# Fourth snippet
ages <- read.csv(file.path('data', 'longevity.csv'))

constant.guess <- with(ages, mean(AgeAtDeath))

with(ages, sqrt(mean((AgeAtDeath - constant.guess) ^ 2)))

smokers.guess <- with(subset(ages, Smokes == 1),
                      mean(AgeAtDeath))

non.smokers.guess <- with(subset(ages, Smokes == 0),
                          mean(AgeAtDeath))

ages <- transform(ages,
                  NewPrediction = ifelse(Smokes == 0,
                                         non.smokers.guess,
                                         smokers.guess))

with(ages, sqrt(mean((AgeAtDeath - NewPrediction) ^ 2)))

# Fifth snippet
library('ggplot2')

heights.weights <- read.csv(file.path('data',
                                      '01_heights_weights_genders.csv'),
                            header = TRUE,
                            sep = ',')

ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Sixth snippet
fitted.regression <- lm(Weight ~ Height,
                        data = heights.weights)

coef(fitted.regression)
#(Intercept) Height
#-350.737192 7.717288

# Seventh snippet
intercept <- coef(fitted.regression)[1]
slope <- coef(fitted.regression)[2]

# predicted.weight <- intercept + slope * observed.height
# predicted.weight == -350.737192 + 7.717288 * observed.height

# Eighth snippet
predict(fitted.regression)

# Ninth snippet
true.values <- with(heights.weights, Weight)
errors <- true.values - predict(fitted.regression)

# Tenth snippet
residuals(fitted.regression)

# Eleventh snippet
plot(fitted.regression, which = 1)

# Twelfth snippet
x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

plot(fitted.regression, which = 1)

# Thirteenth snippet
x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
sum(squared.errors)
#[1] 528

# Fourteenth snippet
x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
mse
#[1] 52.8

# Fifteenth snippet
x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
rmse <- sqrt(mse)
rmse
#[1] 7.266361

# Sixteenth snippet
# There was an error in the book for this example.
# R-squared is a ratio of MSE's, not RMSE's.
mean.mse <- 1.09209343
model.mse <- 0.954544

r2 <- 1 - (model.mse / mean.mse)
r2
#[1] 0.1259502

# Seventeenth snippet
top.1000.sites <- read.csv(file.path('data', 'top_1000_sites.tsv'),
                           sep = '\t',
                           stringsAsFactors = FALSE)

ggplot(top.1000.sites, aes(x = PageViews, y = UniqueVisitors)) +
  geom_point()
ggsave(file.path("images", "page_views_vs_visitors.pdf"))

# Eighteenth snippet
ggplot(top.1000.sites, aes(x = PageViews)) +
  geom_density()

# Ninteenth snippet
ggplot(top.1000.sites, aes(x = log(PageViews))) +
  geom_density()

# Twentieth snippet
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
  geom_point()
ggsave(file.path("images", "log_page_views_vs_log_visitors.pdf"))

# Twenty-first snippet
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
ggsave(file.path("images", "log_page_views_vs_log_visitors_with_lm.pdf"))

# Twenty-second snippet
lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),
             data = top.1000.sites)

# Twenty-third snippet
summary(lm.fit)

#Call:
#lm(formula = log(PageViews) ~ log(UniqueVisitors), data = top.1000.sites)
#
#Residuals:
# Min 1Q Median 3Q Max
#-2.1825 -0.7986 -0.0741 0.6467 5.1549
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) -2.83441 0.75201 -3.769 0.000173 ***
#log(UniqueVisitors) 1.33628 0.04568 29.251 < 2e-16 ***
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.084 on 998 degrees of freedom
#Multiple R-squared: 0.4616, Adjusted R-squared: 0.4611
#F-statistic: 855.6 on 1 and 998 DF, p-value: < 2.2e-16

# Twenty-fourth snippet
lm.fit <- lm(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + InEnglish,
             data = top.1000.sites)
summary(lm.fit)

#Call:
#lm(formula = log(PageViews) ~ HasAdvertising + log(UniqueVisitors) +
# InEnglish, data = top.1000.sites)
#
#Residuals:
# Min 1Q Median 3Q Max
#-2.4283 -0.7685 -0.0632 0.6298 5.4133
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept) -1.94502 1.14777 -1.695 0.09046 .
#HasAdvertisingYes 0.30595 0.09170 3.336 0.00088 ***
#log(UniqueVisitors) 1.26507 0.07053 17.936 < 2e-16 ***
#InEnglishNo 0.83468 0.20860 4.001 6.77e-05 ***
#InEnglishYes -0.16913 0.20424 -0.828 0.40780
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.067 on 995 degrees of freedom
#Multiple R-squared: 0.4798, Adjusted R-squared: 0.4777
#F-statistic: 229.4 on 4 and 995 DF, p-value: < 2.2e-16

# Twenty-fifth snippet
lm.fit <- lm(log(PageViews) ~ HasAdvertising,
             data = top.1000.sites)
summary(lm.fit)$r.squared
#[1] 0.01073766

lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),
             data = top.1000.sites)
summary(lm.fit)$r.squared
#[1] 0.4615985

lm.fit <- lm(log(PageViews) ~ InEnglish,
             data = top.1000.sites)
summary(lm.fit)$r.squared
#[1] 0.03122206

# Twenty-sixth snippet
x <- 1:10
y <- x ^ 2

ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Twenty-seventh snippet
cor(x, y)
#[1] 0.9745586

# Twenty-eighth snippet
coef(lm(scale(y) ~ scale(x)))
# (Intercept) scale(x)
#-1.386469e-16 9.745586e-01
