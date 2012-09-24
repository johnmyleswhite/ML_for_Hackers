# File-Name:       package_installer.R           
# Date:            2012-02-10                         
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Install all of the packages needed for the Machine Learning for Hackers case studies
# Data Used:       n/a
# Packages Used:   n/a

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# Create a vector containing all of the packages that will be used in the case studies
# (in no particular order)

options(repos=structure(c(CRAN="http://cran.stat.auckland.ac.nz/")))

cran.packages <- c("e1071",
                   "ggplot2",
                   "glmnet",
                   "Hmisc",
                   "igraph",
                   "lme4",
                   "lubridate",
                   "plyr",
                   "RCurl",
                   "reshape",
                   "RJSONIO",
                   "scales",
                   "tm",
                   "XML")

cat("This script will now attempt to install all of the R packages used in 'Machine Learning for Hackers'")

for(p in cran.packages) {
    if(!suppressWarnings(require(p, character.only = TRUE, quietly = TRUE))) {
        cat(paste(p, "missing, will attempt to install\n"))
        install.packages(p, dependencies = TRUE, type = "source")
    }
    else {
        cat(paste(p, "installed OK\n"))
    }
}

print("### All required packages installed ###")

