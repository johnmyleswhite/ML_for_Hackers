# File-Name:       google_sg.R           
# Date:            2012-02-10                             
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         File 1 for code from Chapter 11.  This file contains a set of functions for building 
#                   igraph network object from the Twitter social graphs.  As the initial set of code
#                   used in this case, we will write functions that query the Google SocialGraph API
#                   download the data, parse it, and build out the network objects.  This will later
#                   be used to generate graphs of specific users and perform the analysis.
# Data Used:       Accessed via the Google SocialGraph API, source: http://code.google.com/apis/socialgraph/
# Packages Used:   igraph, RCurl, RJSONIO

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
library(RCurl)
library(RJSONIO)

# Functions for building an ego-net for a given Twitter
# user from the data available on the Google Social
# Graph API

# Highest-level function for parsing GSG API JSON
twitter.network <- function(user) {
    api.url <- paste("https://socialgraph.googleapis.com/lookup?q=http://twitter.com/",
        user, "&edo=1&edi=1", sep = "")
    api.get <- getURL(api.url)
    # To guard against web-request issues, we create this loop
    # to ensure we actually get something back from getURL.
    while(grepl("Service Unavailable. Please try again later.", api.get)) {
        api.get <- getURL(api.url)
    }
    api.json <- fromJSON(api.get)
    return(build.ego(api.json))
}

# This function does most of the work, by building out the ego-network
# relationships for the given user.
build.ego <- function(json) {
    # Extract only the Twitter nodes
    ego <- find.twitter(names(json$nodes))
    # Build the in- and out-degree edgelist for the user
    nodes.out <- names(json$nodes[[1]]$nodes_referenced)
    if(length(nodes.out) > 0) {
        # No connections, at all
        twitter.friends <- find.twitter(nodes.out)
        if(length(twitter.friends) > 0) {
              # No twitter connections
            friends <- cbind(ego, twitter.friends)
        }
        else {
            friends <- c(integer(0), integer(0))
        }
    }
    else {
        friends <- c(integer(0), integer(0))
    }
    nodes.in <- names(json$nodes[[1]]$nodes_referenced_by)
    if(length(nodes.in) > 0) {
        twitter.followers <- find.twitter(nodes.in)
        if(length(twitter.followers) > 0) {
            followers <- cbind(twitter.followers, ego)
        }
        else {
            followers <- c(integer(0), integer(0))
        }
    }
    else {
        followers <- c(integer(0), integer(0))
    }
    ego.el <- rbind(friends, followers)
    return(ego.el)
}

# Some of the nodes returned by GSG are not Twitter, but other
# social graph website. As such, we create a secondary function
# to extract only these nodes.
find.twitter <- function(node.vector) {
    twitter.nodes <- node.vector[grepl("http://twitter.com/", node.vector, fixed = TRUE)]
    if(length(twitter.nodes) > 0) {
        twitter.users <- strsplit(twitter.nodes, "/")
        user.vec <- sapply(1:length(twitter.users),
            function(i) (ifelse(twitter.users[[i]][4] == "account", NA, twitter.users[[i]][4])))
        return(user.vec[which(!is.na(user.vec))])
    }
    else {
        return(character(0))
    }
}

# Next, we will build the function for generating the "snowball search"
# in Twitter from a seed user.  For our purposes, we will always only
# do a search two hops from the seed, but we will build the function
# such that larger searches are possible.
twitter.snowball <- function(seed, k=2) {
    # Get the ego-net for the seed user. We will build onto
    # this network to create the full snowball search.
    snowball.el <- twitter.network(seed)

    # Use neighbors as seeds in the next round of the snowball
    new.seeds <- get.seeds(snowball.el, seed)
    rounds <- 1  # We have now completed the first round of the snowball!

    # A record of all nodes hit, this is done to reduce the amount of
    # API calls done.
    all.nodes <- seed

  
    # Begin the snowball search...
    while(rounds < k) {
        next.seeds <- c()
        for(user in new.seeds) {
            # Only get network data if we haven't already visited this node
            if(!user %in% all.nodes) {
                user.el <- twitter.network(user)
                if(dim(user.el)[2] > 0) {
                    snowball.el <- rbind(snowball.el, user.el)
                    next.seeds <- c(next.seeds, get.seeds(user.el, user))
                    all.nodes <- c(all.nodes, user)
                }
            }
        }
        new.seeds <- unique(next.seeds)
        new.seeds <- new.seeds[!which(new.seeds %in% all.nodes)]
        rounds <- rounds + 1
    }
    # It is likely that this process has created duplicate rows.
    # As a matter of house-keeping we will remove them because
    # the true Twitter social graph does not contain parallel edges.
    snowball.el <- snowball.el[!duplicated(snowball.el),]
    return(graph.edgelist(snowball.el))
}

# A small helper function to return the unique set of new seeds
# from one iteration of the snowball search.
get.seeds <- function(snowball.el, seed) {
    new.seeds <- unique(c(snowball.el[,1], snowball.el[,2]))
    return(new.seeds[which(new.seeds != seed)])
}
  
