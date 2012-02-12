# File-Name:       twitter_net.R
# Date:            2012-02-10
# Author:          Drew Conway (drew.cownway@nyu.edu)
# Purpose:         File 2 for code in Chapter 11.  In this short file we write code for generating the 
#                   the ego-network for a given Twitter user.  Once the network object has been built we
#                   add some vertex level attributs, and clean the graph by extracting the 2-corre.  Finally,
#                   we perform a hierarchical clustering analysis of the nodes as a distance-based community
#                   detection.  We then add these node partitions to the nodes, and save the files as GaphML.
# Data Used:       n/a
# Packages Used:   igraph, see 01_google_sg.R

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

######################################################
####                                              ####
####            WARNING TO THE READER             ####
####                                              ####
#### AS OF 2012-01-19 IT APPEARS THAT TWITTER.COM ####
#### HAS CHANGED HOW IT INTERACTS WITH THE GOOGLE ####
#### SOCIALGRAPH API, AND THUS THIS CODE WILL     ####
#### PRODUCE ERRORS. IT IS LEFT FOR EXPOSITION,   ####
#### AND SO THE READER CAN SEE HOW THE DATA WAS   ####
#### ORIGINALLY PRODUCED. USE AT YOUR OWN RISK!   ####
######################################################

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

# Load libraries
library(igraph)

# NOTE: These code blocks are left commented becasue running them will cause errors.
# You can run this code with the example data provided for this chatper.

# Load functions for building networks from 'google_sg.R'
# source('01_google_sg.R')
# 
# # First build the two-round snowball for a given user, supplied at the command-line
# cat("Twitter user as seed: ")
# user.in <- file("stdin")
# user <- readLines(user.in, n = 1)
# close(user.in)

# NOTE TO WINDOWS USERS: You may not be able to run this script
# from the DOS shell.  In this case, you should just set this
# variable to whatever Twitter user you would like to build
# the network for and run this script as you have done before.
# 
user <- 'johnmyleswhite' 

# dir.create(paste("data/", user, sep = ""), showWarnings = FALSE)

user.net <- suppressWarnings(read.graph(paste("data/", user, "/", user, "_net.graphml", sep = ""), format = "graphml"))
# user.net <- twitter.snowball(user, k = 2)

# Fix labels for working with Gephi
user.net  <- set.vertex.attribute(user.net, "Label", value = get.vertex.attribute(user.net, "name"))

# Next, extract the 2-core, and remove pendants generated as a result
user.cores <- graph.coreness(user.net, mode = "in")
user.clean <- subgraph(user.net, which(user.cores > 1) - 1)

# Finally, extract ego.net
user.ego <- subgraph(user.net, c(0, neighbors(user.net, user, mode = "out")))

# Add hierarchical clustering data to network
user.sp <- shortest.paths(user.ego)
user.hc <- hclust(dist(user.sp))

png(paste('../images/', user, '_dendrogram.png', sep=''), width=1680, height=1050)
plot(user.hc)
dev.off()

# Add first 10 non-trival HC partitions
for(i in 2:10) {    
    user.cluster <- as.character(cutree(user.hc, k = i))
    user.cluster[1] <- "0"
    user.ego <- set.vertex.attribute(user.ego, name = paste("HC", i, sep = ""), value = user.cluster)
}

# Add k-means clustering data to network
for(i in 2:10) {
    user.km <- kmeans(dist(user.sp), centers = i)
    user.cluster <- as.character(user.km$cluster)
    user.cluster[1] <- "0"
    user.ego <- set.vertex.attribute(user.ego, name = paste("KM", i, sep = ""), value = user.cluster)
}

# Write files as GraphML format
write.graph(user.net, paste("data/", user, "/", user, "_net.graphml", sep = ""), format = "graphml")
write.graph(user.clean, paste("data/", user, "/", user, "_clean.graphml", sep = ""), format = "graphml")
write.graph(user.ego, paste("data/", user, "/", user, "_ego.graphml", sep = ""), format = "graphml")