# File-Name:       twitter_rec.R
# Date:            2012-02-10
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         File 3 for code in Chapter 9.  In the final piece of this case study we design a
#                   simple social graph reccommendation system based on Twitter data.  Using the
#                   data generated in the previous files, we can make recommendations for users 
#                   based on the density of follows for unfollwed users in the social graph, as
#                   well as community-specific reccomendations based on the distance-based
#                   communities defined in the previous step.
# Data Used:       data/*.graphml
# Packages Used:   igraph

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

# Change only if you want to run this code for a new
# user's data, rather than the pre-loaded data.

user <- "drewconway"

# If you would like to generate the data for this user now
# run this following commands
# source("01_google_sg.R")
# user.graph <- twitter.snowball(user)

# If you would like to work with one of the pre-loaded data
# sets run the following commands
user.graph <- suppressWarnings(read.graph(paste("data/", user, "/", user, "_net.graphml", sep = ""), format = "graphml"))

# One simple method for suggesting followes is to attempt to "close triangles" among a user and their friends.
# This requires very little graph theory, and is more about doing effecient counting of edges.  We
# need to find which user within the current set of unfollowed users is most followed by the user's friends.

friends <- V(user.graph)$name[neighbors(user.graph, user, mode = "out") + 1]
user.el <- get.edgelist(user.graph)

# Now, who am I currently not following that most of my friends are following?
# This logical switch is actually going to do most of the heavy-lifitng for us.
non.friends <- sapply(1:nrow(user.el), function(i) ifelse(any(user.el[i,] == user | 
    !user.el[i,1] %in% friends) | user.el[i,2] %in% friends, FALSE, TRUE))

non.friends.el <- user.el[which(non.friends == TRUE),]
friends.count <- table(non.friends.el[,2])

# Report the results
friends.followers <- data.frame(list(Twitter.Users = names(friends.count), 
    Friends.Following=as.numeric(friends.count)), stringsAsFactors = FALSE)
    
friends.followers$Friends.Norm <- friends.followers$Friends.Following / length(friends)
friends.followers <- friends.followers[with(friends.followers, order(Twitter.Users)),]

head(friends.followers)

write.csv(friends.followers, paste("data/", user, "/", user, "_friends_rec.csv", sep=""), row.names=FALSE)

# Beyond global triangle closure?  We can reccommend users to follow based on the communities 
# detected in the previous step.

user.ego <- suppressWarnings(read.graph(paste("data/", user, "/", user, "_ego.graphml", sep = ""), format = "graphml"))
friends.partitions <- cbind(V(user.ego)$HC8, V(user.ego)$name)

# This function takes a single partition ID number within our graph and finds the Twitter user
# who most of the members of that partition are following that the seed user is NOT following.
partition.follows <- function(i) {
    friends.in <- friends.partitions[which(friends.partitions[,1] == i),2]
    partition.non.follow <- non.friends.el[which(!is.na(match(non.friends.el[,1], friends.in))),]
    # If there are no matches for non-followers, return NA
    if(nrow(partition.non.follow) < 2) {
        return(c(i, NA))
    }
    # If there are, return the most popualr user followed by members of this partition
    else {
        partition.favorite <- table(partition.non.follow[,2])
        partition.favorite <- partition.favorite[order(-partition.favorite)]
        return(c(i, names(partition.favorite)[1]))
    }
}

# Run the partition.follow function over all parition, and remove the NAs and duplicate reccomendations
partition.recs <- t(sapply(unique(friends.partitions[,1]), partition.follows))
partition.recs <- partition.recs[!is.na(partition.recs[,2]) & !duplicated(partition.recs[,2]),]

# Get the node index for the entire graph, plus reccommended users
new.friends <- as.character(c(V(user.ego)$name, partition.recs[,2]))
new.index <- match(new.friends, V(user.graph)$name) - 1

# Take a new subgraph, which includes the new reccommendations
partition.graph <- subgraph(user.graph, new.index)

# Add some vertex attribute data for the visualization
all.partition <- rbind(cbind(get.vertex.attribute(user.ego, "HC8"), V(user.ego)$name), partition.recs)
all.index <- match(as.character(all.partition[,2]), V(partition.graph)$name) - 1
 
partition.graph <- set.vertex.attribute(partition.graph, "REC", index = all.index, value = all.partition[,1])

vertex.sizes <- c("3", rep("1", vcount(user.ego)-1), rep("2", nrow(partition.recs)))
partition.graph <- set.vertex.attribute(partition.graph, "SIZE", index = all.index, value = vertex.sizes)

# Save the resutls as GraphML
write.graph(partition.graph, paste("data/", user, "/", user, "_rec.graphml",sep = ""), format = "graphml")

