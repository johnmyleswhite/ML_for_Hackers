# File-Name:       email_classify.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Code for Chapter 3. In this case we introduce the notion of binary classification.
#                   In machine learning this is a method for determining what of two categories a 
#                   given observation belongs to.  To show this, we will create a simple naive Bayes 
#                   classifier for SPAM email detection, and visualize the results.
# Data Used:       Email messages contained in data/ directory, source: http://spamassassin.apache.org/publiccorpus/
# Packages Used:   tm, ggplot2

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

# Load libraries
library('tm')
library('ggplot2')

# Set the global paths
spam.path <- file.path("data", "spam")
spam2.path <- file.path("data", "spam_2")
easyham.path <- file.path("data", "easy_ham")
easyham2.path <- file.path("data", "easy_ham_2")
hardham.path <- file.path("data", "hard_ham")
hardham2.path <- file.path("data", "hard_ham_2")

# Create motivating plot
x <- runif(1000, 0, 40)
y1 <- cbind(runif(100, 0, 10), 1)
y2 <- cbind(runif(800, 10, 30), 2)
y3 <- cbind(runif(100, 30, 40), 1)

val <- data.frame(cbind(x, rbind(y1, y2, y3)),
                  stringsAsFactors = TRUE)

ex1 <- ggplot(val, aes(x, V2)) +
  geom_jitter(aes(shape = as.factor(V3)),
                  position = position_jitter(height = 2)) +
  scale_shape_discrete(guide = "none", solid = FALSE) +
  geom_hline(aes(yintercept = c(10,30)), linetype = 2) +
  theme_bw() +
  xlab("X") +
  ylab("Y")
ggsave(plot = ex1,
       filename = file.path("images", "00_Ex1.pdf"),
       height = 10,
       width = 10)

# Return a single element vector of just the email body
# This is a very simple approach, as we are only using 
# words as features
get.msg <- function(path)
{
  con <- file(path, open = "rt", encoding = "latin1")
  text <- readLines(con)
  # The message always begins after the first full line break
  msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
  close(con)
  return(paste(msg, collapse = "\n"))
}

# Create a TermDocumentMatrix (TDM) from the corpus of SPAM email.
# The TDM control can be modified, and the sparsity level can be 
# altered.  This TDM is used to create the feature set used to do 
# train our classifier.
get.tdm <- function(doc.vec)
{
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE,
                  minDocFreq = 2)
  doc.corpus <- Corpus(VectorSource(doc.vec))
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

# This function takes a file path to an email file and a string, 
# the term parameter, and returns the count of that term in 
# the email body.
count.word <- function(path, term)
{
  msg <- get.msg(path)
  msg.corpus <- Corpus(VectorSource(msg))
  # Hard-coded TDM control
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE)
  msg.tdm <- TermDocumentMatrix(msg.corpus, control)
  word.freq <- rowSums(as.matrix(msg.tdm))
  term.freq <- word.freq[which(names(word.freq) == term)]
  # We use ifelse here because term.freq = NA if nothing is found
  return(ifelse(length(term.freq) > 0, term.freq, 0))
}

# This is the our workhorse function for classifying email.  It takes 
# two required paramters: a file path to an email to classify, and
# a data frame of the trained data.  The function also takes two 
# optional parameters.  First, a prior over the probability that an email
# is SPAM, which we set to 0.5 (naive), and constant value for the
# probability on words in the email that are not in our training data.
# The function returns the naive Bayes probability that the given email
# is SPAM.  
classify.email <- function(path, training.df, prior = 0.5, c = 1e-6)
{
  # Here, we use many of the support functions to get the
  # email text data in a workable format
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  # Find intersections of words
  msg.match <- intersect(names(msg.freq), training.df$term)
  # Now, we just perform the naive Bayes calculation
  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  else
  {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}


# With all of our support functions written, we can perform the classification.
# First, we create document corpus for spam messages

# Get all the SPAM-y email into a single vector
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs,
                   function(p) get.msg(file.path(spam.path, p)))

# Create a DocumentTermMatrix from that vector
spam.tdm <- get.tdm(all.spam)

# Create a data frame that provides the feature set from the training SPAM data
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),
                      stringsAsFactors = FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)
spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i)
                          {
                            length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)
                          })
spam.density <- spam.df$frequency / sum(spam.df$frequency)

# Add the term density and occurrence rate
spam.df <- transform(spam.df,
                     density = spam.density,
                     occurrence = spam.occurrence)

# Now do the same for the EASY HAM email
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs[1:length(spam.docs)],
                      function(p) get.msg(file.path(easyham.path, p)))

easyham.tdm <- get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),
                         stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)
easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                            function(i)
                            {
                              length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)
                            })
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)

easyham.df <- transform(easyham.df,
                        density = easyham.density,
                        occurrence = easyham.occurrence)

# Run classifer against HARD HAM
hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]

hardham.spamtest <- sapply(hardham.docs,
                           function(p) classify.email(file.path(hardham.path, p), training.df = spam.df))
    
hardham.hamtest <- sapply(hardham.docs,
                          function(p) classify.email(file.path(hardham.path, p), training.df = easyham.df))
    
hardham.res <- ifelse(hardham.spamtest > hardham.hamtest,
                      TRUE,
                      FALSE)
summary(hardham.res)

# Find counts of just terms 'html' and 'table' in all SPAM and EASYHAM docs, and create figure
html.spam <- sapply(spam.docs,
                    function(p) count.word(file.path(spam.path, p), "html"))
table.spam <- sapply(spam.docs,
                     function(p) count.word(file.path(spam.path, p), "table"))
spam.init <- cbind(html.spam, table.spam, "SPAM")

html.easyham <- sapply(easyham.docs,
                       function(p) count.word(file.path(easyham.path, p), "html"))
table.easyham <- sapply(easyham.docs,
                        function(p) count.word(file.path(easyham.path, p), "table"))
easyham.init <- cbind(html.easyham, table.easyham, "EASYHAM")

init.df <- data.frame(rbind(spam.init, easyham.init),
                      stringsAsFactors = FALSE)
names(init.df) <- c("html", "table", "type")
init.df$html <- as.numeric(init.df$html)
init.df$table <- as.numeric(init.df$table)
init.df$type <- as.factor(init.df$type)

init.plot1 <- ggplot(init.df, aes(x = html, y = table)) +
  geom_point(aes(shape = type)) +
  scale_shape_manual(values = c("SPAM" = 1, "EASYHAM" = 3), name = "Email Type") +
  xlab("Frequency of 'html'") +
  ylab("Freqeuncy of 'table'") +
  stat_abline(yintersept = 0, slope = 1) +
  theme_bw()
ggsave(plot = init.plot1,
       filename = file.path("images", "01_init_plot1.pdf"),
       width = 10,
       height = 10)
    
init.plot2 <- ggplot(init.df, aes(x = html, y = table)) +
  geom_point(aes(shape = type), position = "jitter") +
  scale_shape_manual(values = c("SPAM" = 1, "EASYHAM" = 3), name = "Email Type") +
  xlab("Frequency of 'html'") +
  ylab("Freqeuncy of 'table'") +
  stat_abline(yintersept = 0, slope = 1) +
  theme_bw()
ggsave(plot = init.plot2,
       filename = file.path("images", "02_init_plot2.pdf"),
       width = 10,
       height = 10)

# Finally, attempt to classify the HARDHAM data using the classifer developed above.
# The rule is to classify a message as SPAM if Pr(email) = SPAM > Pr(email) = HAM
spam.classifier <- function(path)
{
  pr.spam <- classify.email(path, spam.df)
  pr.ham <- classify.email(path, easyham.df)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}

# Get lists of all the email messages
easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs != "cmds")]

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs != "cmds")]

spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs != "cmds")]

# Classify them all!
easyham2.class <- suppressWarnings(lapply(easyham2.docs,
                                   function(p)
                                   {
                                     spam.classifier(file.path(easyham2.path, p))
                                   }))
hardham2.class <- suppressWarnings(lapply(hardham2.docs,
                                   function(p)
                                   {
                                     spam.classifier(file.path(hardham2.path, p))
                                   }))
spam2.class <- suppressWarnings(lapply(spam2.docs,
                                function(p)
                                {
                                  spam.classifier(file.path(spam2.path, p))
                                }))

# Create a single, final, data frame with all of the classification data in it
easyham2.matrix <- do.call(rbind, easyham2.class)
easyham2.final <- cbind(easyham2.matrix, "EASYHAM")

hardham2.matrix <- do.call(rbind, hardham2.class)
hardham2.final <- cbind(hardham2.matrix, "HARDHAM")

spam2.matrix <- do.call(rbind, spam2.class)
spam2.final <- cbind(spam2.matrix, "SPAM")

class.matrix <- rbind(easyham2.final, hardham2.final, spam2.final)
class.df <- data.frame(class.matrix, stringsAsFactors = FALSE)
names(class.df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")
class.df$Pr.SPAM <- as.numeric(class.df$Pr.SPAM)
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)
class.df$Class <- as.logical(as.numeric(class.df$Class))
class.df$Type <- as.factor(class.df$Type)

# Create final plot of results
class.plot <- ggplot(class.df, aes(x = log(Pr.HAM), log(Pr.SPAM))) +
    geom_point(aes(shape = Type, alpha = 0.5)) +
    stat_abline(yintercept = 0, slope = 1) +
    scale_shape_manual(values = c("EASYHAM" = 1,
                                  "HARDHAM" = 2,
                                  "SPAM" = 3),
                       name = "Email Type") +
    scale_alpha(guide = "none") +
    xlab("log[Pr(HAM)]") +
    ylab("log[Pr(SPAM)]") +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(plot = class.plot,
       filename = file.path("images", "03_final_classification.pdf"),
       height = 10,
       width = 10)

get.results <- function(bool.vector)
{
  results <- c(length(bool.vector[which(bool.vector == FALSE)]) / length(bool.vector),
               length(bool.vector[which(bool.vector == TRUE)]) / length(bool.vector))
  return(results)
}

# Save results as a 2x3 table
easyham2.col <- get.results(subset(class.df, Type == "EASYHAM")$Class)
hardham2.col <- get.results(subset(class.df, Type == "HARDHAM")$Class)
spam2.col <- get.results(subset(class.df, Type == "SPAM")$Class)

class.res <- rbind(easyham2.col, hardham2.col, spam2.col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)

# Save the training data for use in Chapter 4
write.csv(spam.df, file.path("data", "spam_df.csv"), row.names = FALSE)
write.csv(easyham.df, file.path("data", "easyham_df.csv"), row.names = FALSE)
