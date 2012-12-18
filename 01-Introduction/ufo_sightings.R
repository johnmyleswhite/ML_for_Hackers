# File-Name:       ufo_sightings.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Code for Chapter 1.  In this case we will review some of the basic
#                   R functions and coding paradigms we will use throughout this book.
#                   This includes loading, viewing, and cleaning raw data; as well as
#                   some basic visualization.  This specific case we will use data from
#                   reported UFO sightings to investigate what, if any, seasonal trends
#                   exists in the data.
# Data Used:       http://www.infochimps.com/datasets/60000-documented-ufo-sightings-with-text-descriptions-and-metada
# Packages Used:   ggplot2, plyr, scales

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

# Load libraries and data
library(ggplot2)    # We'll use ggplot2 for all of our visualizations
library(plyr)       # For data manipulation
library(scales)     # We'll need to fix date formats in plots

# This is a tab-delimited file, so we use 'read.delim' and set the separator as a tab character.
# We also have to alter two defaults; first, we want the strings to not be converted to
# factor types; and, this data has does not have header labels in the first row, so
# we want to keep the first row as data.
ufo <- read.delim(file.path("data", "ufo", "ufo_awesome.tsv"),
                  sep = "\t",
                  stringsAsFactors = FALSE,
                  header = FALSE, 
                  na.strings = "")
# This is a large text file (75MB), so this may take a moment

# Inspect the data frame
summary(ufo)
head(ufo)

# From the data's description file, we will set the column names accordingly using 
# the 'names' function
names(ufo) <- c("DateOccurred", "DateReported",
                "Location", "ShortDescription",
                "Duration", "LongDescription")

# To work with the dates, we will need to convert the YYYYMMDD string to an R Date
# type using the 'strptime' function

# But, something has gone wrong with the data. For now, we'll just ignore the errata
# by removing those entries that have not parsed correctly.  We know that the date 
# strings are always 8 characters long, and any deviation from this would indicate
# a row to ignore.  We will use the 'ifelse' function to construct a vector of
# Booleans indicating the problem rows
good.rows <- ifelse(nchar(ufo$DateOccurred) != 8 |
                    nchar(ufo$DateReported) != 8,
                    FALSE,
                    TRUE)
length(which(!good.rows))      # While 731 rows may seem like a lot, out of over 60K
ufo <- ufo[good.rows, ]        # it is only about 0.6% of the total number of records.

# Now we can convert the strings to Date objects and work with them properly
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format = "%Y%m%d")

# It will be useful to create separate columns for both town and state from the Location 
# column.  To do so we will use the 'strsplit' function to perform the regex.
# Note: not every entry in Location is of the form 'City, State'.  We use the
# 'tryCatch' function to simply return [NA, NA] when this is the case.  Next,
# we remove the leading white-space from both the city and state strings with 'gsub'
get.location <- function(l)
{
  split.location <- tryCatch(strsplit(l, ",")[[1]],
                             error = function(e) return(c(NA, NA)))
  clean.location <- gsub("^ ","",split.location)
  if (length(clean.location) > 2)
  {
    return(c(NA,NA))
  }
  else
  {
    return(clean.location)
  }
}

# We use 'lapply' to return a list with [City, State] vector as each element
city.state <- lapply(ufo$Location, get.location)

# We use 'do.call' to collapse the list to an N-by-2 matrix
location.matrix <- do.call(rbind, city.state)

# Add the city and state data to ufo data frame. We can do this using the 'transform'
# function.
ufo <- transform(ufo,
                 USCity = location.matrix[, 1],
                 USState = location.matrix[, 2],
                 stringsAsFactors = FALSE)

# Next step, we will strip out non-US incidents

# Insert NA's where there are non-US cities
ufo$USState <- state.abb[match(ufo$USState, state.abb)]

# Finally, we'll use 'subset' to examine only events in the United States and convert 
# states to factors, i.e., a categorical variable.
ufo.us <- subset(ufo, !is.na(USState))

# Now, we are ready to do some analysis!  First, take a look at the post-processed data
summary(ufo.us)
head(ufo.us)

# The summary functions shows us that the data actually go back a very long time (1440!).  So, 
# we will want to take a quick look at the date to see where the majority of the data exists.
# We can do this by creating a histogram of frequencies for UFO sightings over time
quick.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram() + 
  scale_x_date(breaks = "50 years")
  
ggsave(plot = quick.hist,
       filename = file.path("images", "quick_hist.pdf"),
       height = 6,
       width = 8)

# First, we notice that there are many very old entries in the data.  For our purposes, we will only look
# at incidents that occurred from 1990 to the most recent
ufo.us <- subset(ufo.us, DateOccurred >= as.Date("1990-01-01"))

# Let's look at the histogram now
new.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram(aes(fill='white', color='red')) +
  scale_fill_manual(values=c('white'='white'), guide="none") +
  scale_color_manual(values=c('red'='red'), guide="none") +
  scale_x_date(breaks = "50 years")

ggsave(plot = new.hist,
       filename = file.path("images", "new_hist.pdf"),
       height = 6,
       width = 8)

# Now that we have the data we want, let's look at some aggregations.  We will use
# the 'ddply' funtion in the plyr package. But first, we create a column of just
# the Year-Month of each incident.
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format = "%Y-%m")

# This will return the number of sightings of UFO by Year-Month and state for the whole time-series
sightings.counts <- ddply(ufo.us, .(USState,YearMonth), nrow)

# As we might expect, there are several Year-Month and state combinations for which there are no 
# UFO sightings.  We need to count these as zero so we can go back and fill those in.
# First, we will create a new vector that has all of the Year-Month dates in it that span the 
# range of our time-series (1990-2010)
date.range <- seq.Date(from = as.Date(min(ufo.us$DateOccurred)),
                       to = as.Date(max(ufo.us$DateOccurred)),
                       by = "month")
date.strings <- strftime(date.range, "%Y-%m")

# To fill in the missing dates from the 'sightings.counts' data frame we will need to create a separate data
# frame with a column of states and Year-Months.
states.dates <- lapply(state.abb, function(s) cbind(s, date.strings))
states.dates <- data.frame(do.call(rbind, states.dates),
                           stringsAsFactors = FALSE)

# We use 'merge' to take the counts we have and merge them with the missing dates.  Note, we have to specify
# the columns from each data frame we are using to do the merge, and set 'all' to TRUE, which will fill in 
# this missing dates from the original data frame with NA.
all.sightings <- merge(states.dates,
                       sightings.counts,
                       by.x = c("s", "date.strings"),
                       by.y = c("USState", "YearMonth"),
                       all = TRUE)

# Now we just need to clean up the merged data frame a bit
# Set the column names to something meaningful
names(all.sightings) <- c("State", "YearMonth", "Sightings")

# Covert the NAs to 0's, what we really wanted
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0

# Reset the character Year-Month to a Date objects
all.sightings$YearMonth <- as.Date(rep(date.range, length(state.abb)))

# Capitalize the State abbreviation and set as factor
all.sightings$State <- as.factor(all.sightings$State)

# There are lots of ways we could test the seasonality of of these sightings, but one basic method is to 
# inspect the trends visually.  We now construct a plot that will show these trends for all 50 U.S. states
# over the time-series.

# First we have to create a ggplot2 object and then create a geom layer, which in this case is a line.
# Additional points of note:
# (1) facet_wrap() will create separate plots for each state on a 10x5 grid.
# (2) theme_bw() changes the default ggplot2 style from grey to white (personal preference).
# (3) scale_color_manual() sets the line color to dark blue.
# (4) scale_x_date() scales the x-axis as a date, with major lines every 5 years.
# (5) xlab() and ylab() set axis labels.
# (6) opts() sets a title for the plot

state.plot <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")

# Save the plot as a PDF
ggsave(plot = state.plot,
       filename = file.path("images", "ufo_sightings.pdf"),
       width = 14,
       height = 8.5)


# Create a new graph where the number of signtings is normailzed by the state population
state.pop <- read.csv(file.path('data/census.csv'), stringsAsFactors=FALSE)

state.pop$abbs <- sapply(state.pop$State, function(x) state.abb[grep(paste('^', x, sep=''), state.name)])
all.sightings$Sightings.Norm <- sapply(1:nrow(all.sightings), 
    function(i) all.sightings$Sightings[i] / state.pop$X2000[which(state.pop$abbs== all.sightings$State[i])])
    
    
state.plot.norm <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings.Norm)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Per Capita Number of Sightings (2000 Census)") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")
  
  
# Save the plot as a PDF
ggsave(plot = state.plot.norm,
     filename = file.path("images", "ufo_sightings_norm.pdf"),
     width = 14,
     height = 8.5)
