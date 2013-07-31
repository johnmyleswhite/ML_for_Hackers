# File-Name:       chapter08.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/DJI.csv, data/stock_prices.csv
# Packages Used:   ggplot2, lubridate, reshape

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

library('ggplot2')

# First code snippet
prices <- read.csv(file.path('data', 'stock_prices.csv'),
                   stringsAsFactors = FALSE)

prices[1, ]
# Date Stock Close
#1 2011-05-25 DTE 51.12

# Second code snippet
library('lubridate')

prices <- transform(prices, Date = ymd(Date))

# Third code snippet
library('reshape')

date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

# Fourth code snippet
prices <- subset(prices, Date != ymd('2002-02-01'))
prices <- subset(prices, Stock != 'DDR')

date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

# Fifth code snippet
cor.matrix <- cor(date.stock.matrix[, 2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

ggplot(data.frame(Correlation = correlations),
  aes(x = Correlation, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')

# Sixth code snippet
pca <- princomp(date.stock.matrix[, 2:ncol(date.stock.matrix)])

# Seventh code snippet
#Call:
#princomp(x = date.stock.matrix[, 2:ncol(date.stock.matrix)])
#Standard deviations:
#Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7
#29.1001249 20.4403404 12.6726924 11.4636450 8.4963820 8.1969345 5.5438308
#Comp.8 Comp.9 Comp.10 Comp.11 Comp.12 Comp.13 Comp.14
#5.1300931 4.7786752 4.2575099 3.3050931 2.6197715 2.4986181 2.1746125
#Comp.15 Comp.16 Comp.17 Comp.18 Comp.19 Comp.20 Comp.21
#1.9469475 1.8706240 1.6984043 1.6344116 1.2327471 1.1280913 0.9877634
#Comp.22 Comp.23 Comp.24
#0.8583681 0.7390626 0.4347983
#24 variables and 2366 observations.

# Eighth code snippet
principal.component <- pca$loadings[, 1]

# Ninth code snippet
loadings <- as.numeric(principal.component)

ggplot(data.frame(Loading = loadings),
  aes(x = Loading, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')

# Tenth code snippet
market.index <- predict(pca)[, 1]

# Eleventh code snippet
dji.prices <- read.csv(file.path('data', 'DJI.csv'),
                       stringsAsFactors = FALSE)
dji.prices <- transform(dji.prices, Date = ymd(Date))

# Twelfth code snippet
dji.prices <- subset(dji.prices, Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))

# Thirteenth code snippet
dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))

# Fourteenth code snippet
comparison <- data.frame(Date = dates,
                         MarketIndex = market.index,
                         DJI = dji)

ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Fifteenth code snippet
comparison <- transform(comparison, MarketIndex = -1 * MarketIndex)

# Sixteenth code snippet
ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Seventeenth code snippet
alt.comparison <- melt(comparison, id.vars = 'Date')

names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison,
       aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()

# Eighteenth code snippet
comparison <- transform(comparison, MarketIndex = scale(MarketIndex))
comparison <- transform(comparison, DJI = scale(DJI))

alt.comparison <- melt(comparison, id.vars = 'Date')

names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()
