#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

library(shiny)
library(stringr)
library(ngram)
library(tm)

# Read the Stored RData files. 
threegram <- readRDS("Data/threegram.RData");
twogram <- readRDS("Data/twogram.RData");
onegram <- readRDS("Data/onegram.RData");
cat(file=stderr(), "Reading data  Done", "\n")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

      NextWord <- function(inputtext) {
        
        # Cleaning input data
       
        #1 Convert all letters to lower case
        inputData <- tolower(inputtext)
        
        #2 Remove Punctuation
        inputData <- removePunctuation(inputData)
        
        #3 Remove numbers
        inputData <- removeNumbers(inputData)
        
        #Split the input string into words
        inputDataSplit <- strsplit(inputData, " ")[[1]]
        cat(file=stderr(), "Preprocessing Done", "\n")
        
        # Algorithm
        
        # In this app we use trigram, bigram and unigram for prediction.
        
        length_input <- length(inputDataSplit)
        
        # If the input text has more than two words, consider only the last two words
        
        if (length_input >= 2) {
            Wordsconsidered <- tail(inputDataSplit,2)
            lastTwoWords <- paste(Wordsconsidered[1],Wordsconsidered[2 ], sep = " ")
           
            # In trigram(which is sorted according to frequecy), search for the occurance starting with the last two words
            subset_ng3 <- head(threegram[grep(paste("^",lastTwoWords, " ", sep = ""),threegram$ngrams),1],1)
            
            # If the search results in NULL, then use only the last word and call the function NextWord
            if(length(subset_ng3) == 0){
                cat(file=stderr(), "String NOT found in Trigram", "\n")
                lastoneword <- Wordsconsidered[length_input ]
                result <- NextWord(lastoneword)
            }
            
            # If the string is found, then the predicted word will be the last word of the string that is found in the trigram(highest frequency)
            else {
                cat(file=stderr(), "String found in Trigram", "\n")
                predictedString <- subset_ng3
                predictedStringSplit <- strsplit(predictedString, " ")
                predictedword <- predictedStringSplit[[1]][3]
                predictedword
            }
        }
        
        # If the number of words in the sting passed to the function is 1, then search for the string in bigram((which is sorted according to frequecy)
        else if (length_input == 1) {
            lastoneword <- inputDataSplit[1]
            subset_ng2 <- head(twogram[grep(paste("^",lastoneword, " ", sep = "" ),twogram$ngrams),1],1)
            
            # If the string is not found in bigram, then the predicted word is the highest frequency word in unigram
            if(length(subset_ng2) == 0) {
                cat(file=stderr(), "No Match: Most commom unigram used", "\n")
                subset_ng1 <- onegram[1,1]
                predictedword <- subset_ng1
                predictedword
            }
            
            # If the string is found, then the predicted word will be the last word of the string that is found in the bigram(highest frequency)
            else {
                cat(file=stderr(), "String found in Bigram", "\n")
                predictedString <- subset_ng2
                predictedStringSplit <- strsplit(predictedString, " ")
                predictedword <- predictedStringSplit[[1]][2]
                predictedword
            }
        }
    }
    
      
       observeEvent(input$text_entered,
          once= TRUE,
          handlerExpr = {
          output$text1 <- renderText({
          if (input$text_entered == 0)
              return("")
          isolate ({
            nextpredictedword <- NextWord(input$TextInput)
            nextpredictedword
          })
          })
        }
      )
      
     # observeEvent(
      #  eventExpr = input[["text_entered"]],
      #  handlerExpr = {
       #   output$text1 <- renderText({
        #    nextpredictedword <- ""
         #   nextpredictedword <- NextWord(input$TextInput)
            #nextpredictedword
          #})
        #}
      #)
})
