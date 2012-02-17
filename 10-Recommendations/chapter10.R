# File-Name:       chapter10.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/example.csv, data/installations.csv
# Packages Used:   class, reshape

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

# First code snippet
df <- read.csv(file.path('data', 'example_data.csv'))

head(df)
# X Y Label
#1 2.373546 5.398106 0
#2 3.183643 4.387974 0
#3 2.164371 5.341120 0
#4 4.595281 3.870637 0
#5 3.329508 6.433024 0
#6 2.179532 6.980400 0

# Second code snippet
distance.matrix <- function(df)
{
  distance <- matrix(rep(NA, nrow(df) ^ 2), nrow = nrow(df))
  
  for (i in 1:nrow(df))
  {
    for (j in 1:nrow(df))
    {
      distance[i, j] <- sqrt((df[i, 'X'] - df[j, 'X']) ^ 2 + (df[i, 'Y'] - df[j, 'Y']) ^ 2)
    }
  }

  return(distance)
}

# Third code snippet
k.nearest.neighbors <- function(i, distance, k = 5)
{
  return(order(distance[i, ])[2:(k + 1)])
}

# Fourth code snippet
knn <- function(df, k = 5)
{
  distance <- distance.matrix(df)
  
  predictions <- rep(NA, nrow(df))
  
  for (i in 1:nrow(df))
  {
    indices <- k.nearest.neighbors(i, distance, k = k)
    predictions[i] <- ifelse(mean(df[indices, 'Label']) > 0.5, 1, 0)
  }
  
  return(predictions)
}

# Fifth code snippet
df <- transform(df, kNNPredictions = knn(df))

sum(with(df, Label != kNNPredictions))
#[1] 7

nrow(df)
#[1] 100

# Sixth code snippet
rm('knn') # In case you still have our implementation in memory.

library('class')

df <- read.csv(file.path('data', 'example_data.csv'))

n <- nrow(df)

set.seed(1)

indices <- sort(sample(1:n, n * (1 / 2)))

training.x <- df[indices, 1:2]
test.x <- df[-indices, 1:2]

training.y <- df[indices, 3]
test.y <- df[-indices, 3]

# There's a bug here!
predicted.y <- knn(training.x, test.x, training.y, k = 5)

sum(predicted.y != test.y)
#[1] 7

length(test.y)
#[1] 50

# Seventh code snippet
logit.model <- glm(Label ~ X + Y, data = df[indices, ])

predictions <- as.numeric(predict(logit.model, newdata = df[-indices, ]) > 0)

sum(predictions != test.y)
#[1] 16

# Eighth code snippet
installations <- read.csv(file.path('data', 'installations.csv'))

head(installations)
# Package User Installed
#1 abind 1 1
#2 AcceptanceSampling 1 0
#3 ACCLMA 1 0
#4 accuracy 1 1
#5 acepack 1 0
#6 aCGH.Spline 1 0

# Ninth code snippet
library('reshape')

user.package.matrix <- cast(installations, User ~ Package, value = 'Installed')

user.package.matrix[, 1]
# [1] 1 3 4 5 6 7 8 9 11 13 14 15 16 19 21 23 25 26 27 28 29 30 31 33 34
#[26] 35 36 37 40 41 42 43 44 45 46 47 48 49 50 51 54 55 56 57 58 59 60 61 62 63
#[51] 64 65

user.package.matrix[, 2]
# [1] 1 1 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1
#[39] 1 1 1 1 1 1 1 1 0 1 1 1 1 1

row.names(user.package.matrix) <- user.package.matrix[, 1]

user.package.matrix <- user.package.matrix[, -1]

# Tenth code snippet
similarities <- cor(user.package.matrix)

nrow(similarities)
#[1] 2487
ncol(similarities)
#[1] 2487
similarities[1, 1]
#[1] 1
similarities[1, 2]
#[1] -0.04822428

# Eleventh code snippet
distances <- -log((similarities / 2) + 0.5)

# Twelfth code snippet
k.nearest.neighbors <- function(i, distances, k = 25)
{
  return(order(distances[i, ])[2:(k + 1)])
}

# Thirteenth code snippet
installation.probability <- function(user, package, user.package.matrix, distances, k = 25)
{
  neighbors <- k.nearest.neighbors(package, distances, k = k)
  
  return(mean(sapply(neighbors, function (neighbor) {user.package.matrix[user, neighbor]})))
}

installation.probability(1, 1, user.package.matrix, distances)
#[1] 0.76

# Fourteenth code snippet
most.probable.packages <- function(user, user.package.matrix, distances, k = 25)
{
  return(order(sapply(1:ncol(user.package.matrix),
               function (package)
               {
                 installation.probability(user,
                                          package,
                                          user.package.matrix,
                                          distances,
                                          k = k)
               }),
         decreasing = TRUE))
}

user <- 1

listing <- most.probable.packages(user, user.package.matrix, distances)

colnames(user.package.matrix)[listing[1:10]]
