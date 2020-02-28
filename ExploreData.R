library(ngram)
library(tm)
library(tokenizers)
library(ggplot2)


# read the lines of each file
con_blogs <- file("D:/D/Training/Coursera/Capstone/Data_To_be_Used/Coursera-SwiftKey/final/en_US/en_US.blogs.txt","r")
blogsFileSizeInMB   <- file.size("D:/D/Training/Coursera/Capstone/Data_To_be_Used/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")/1048576
blogsFileSizeInMB
blogs<- readLines(con_blogs)
blogsLineCount = length(blogs)
blogsLineCount
close(con_blogs)

con_tweets <- file("D:/D/Training/Coursera/Capstone/Data_To_be_Used/Coursera-SwiftKey/final/en_US/en_US.twitter.txt","r")
tweetsFileSizeInMB   <- file.size("D:/D/Training/Coursera/Capstone/Data_To_be_Used/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")/1048576
tweetsFileSizeInMB
tweets<- readLines(con_tweets)
tweetsLineCount = length(tweets)
tweetsLineCount
close(con_tweets)

con_news <- file("D:/D/Training/Coursera/Capstone/Data_To_be_Used/Coursera-SwiftKey/final/en_US/en_US.news.txt","r")
newsFileSizeInMB   <- file.size("D:/D/Training/Coursera/Capstone/Data_To_be_Used/Coursera-SwiftKey/final/en_US/en_US.news.txt")/1048576
newsFileSizeInMB
news<- readLines(con_news)
newsLineCount = length(news)
newsLineCount
close(con_news)


# Binomial sampling of the data and create the relevant files
sample.fun <- function(data, percent)
{
  return(data[as.logical(rbinom(length(data),1,percent))])
}
# Set the desired sample percentage
percentage <- 0.05

blogs   <- sample.fun(blogs, percentage)
news   <- sample.fun(news, percentage)
tweets   <- sample.fun(tweets, percentage)


# Creating my corpus
corpuslist <- c(blogs,tweets,news)
corpusData<-Corpus(VectorSource(list(corpuslist)))


# Cleaning corpus data

#1. Remove extra whitespace
corpusData <- tm_map(corpusData, stripWhitespace)

#2 Convert all letters to lower case
corpusData <- tm_map(corpusData, content_transformer(tolower))

#3 Remove Punctuation
corpusData <- tm_map(corpusData,removePunctuation)

#4 Remove numbers
corpusData <- tm_map(corpusData, removeNumbers)


# Save the corpus 
writeCorpus(corpusData, path="D:/D/Training/Coursera/Capstone/FinalProject/", filenames="corpusData.txt")

corpus_string <- concatenate ( lapply ( corpusData , "[", 1) )

#Use ngram to find which words have highest frequency and save it in RData files for future reference

ng1 <- ngram(corpus_string, n=1)
phrasetable_ng1 <- get.phrasetable(ng1)
write.csv(phrasetable_ng1,file="D:/D/Training/Coursera/Capstone/FinalProject/onegram.csv",row.names=FALSE)
onegram <- read.csv("D:/D/Training/Coursera/Capstone/FinalProject/onegram.csv",stringsAsFactors = F)
saveRDS(onegram, "D:/D/Training/Coursera/Capstone/FinalProject/onegram.RData")

ng2 <- ngram(corpus_string, n=2)
phrasetable_ng2 <- get.phrasetable(ng2)
write.csv(phrasetable_ng2,file="D:/D/Training/Coursera/Capstone/FinalProject/twogram.csv",row.names=FALSE)
twogram <- read.csv("D:/D/Training/Coursera/Capstone/FinalProject/twogram.csv",stringsAsFactors = F)
saveRDS(twogram, "D:/D/Training/Coursera/Capstone/FinalProject/twogram.RData")

ng3 <- ngram(corpus_string, n=3)
phrasetable_ng3 <- get.phrasetable(ng3)
write.csv(phrasetable_ng3,file="D:/D/Training/Coursera/Capstone/FinalProject/threegram.csv",row.names=FALSE)
threegram <- read.csv("D:/D/Training/Coursera/Capstone/FinalProject/threegram.csv",stringsAsFactors = F)
saveRDS(threegram, "D:/D/Training/Coursera/Capstone/FinalProject/threegram.RData")

