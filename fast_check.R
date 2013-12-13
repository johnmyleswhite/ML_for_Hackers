cat("Checking Chapter 1 - Introduction\n\n")
setwd('01-Introduction')
source('ufo_sightings.R')
setwd('..')

cat("Checking Chapter 2 - Exploration\n\n")
setwd('02-Exploration')
source('chapter02.R')
setwd('..')

cat("Checking Chapter 3 - Classification\n\n")
setwd('03-Classification')
source('email_classify.R')
setwd('..')

cat("Checking Chapter 4 - Ranking\n\n")
setwd('04-Ranking')
source('priority_inbox.R')
setwd('..')

cat("Checking Chapter 5 - Regression\n\n")
setwd('05-Regression')
source('chapter05.R')
setwd('..')

cat("Checking Chapter 6 - Regularization\n\n")
setwd('06-Regularization')
source('chapter06.R')
setwd('..')

cat("Checking Chapter 7 - Optimization\n\n")
setwd('07-Optimization')
source('chapter07.R')
setwd('..')

cat("Checking Chapter 8 - PCA\n\n")
setwd('08-PCA')
source('chapter08.R')
setwd('..')

cat("Checking Chapter 9 - MDS\n\n")
setwd('09-MDS')
source('chapter09.R')
setwd('..')

cat("Checking Chapter 10 - Recommendations\n\n")
setwd('10-Recommendations')
source('chapter10.R')
setwd('..')

#cat("Checking Chapter 11 - SNA\n\n")
#setwd('11-SNA')
#source('chapter09.R')
#setwd('..')

cat("Checking Chapter 12 - Model Comparison\n\n")
setwd('12-Model_Comparison')
source('chapter12.R')
setwd('..')
