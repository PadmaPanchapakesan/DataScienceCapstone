
library(shiny)


shinyUI(navbarPage(

    # Application title
    titlePanel(""),

    
    tabPanel("TextPredictor",
    # Sidebar to take user input
    sidebarLayout(
        sidebarPanel(
            textInput("TextInput", "Enter any text in English and press Submit", value =""),
            actionButton(
                inputId = "text_entered",
                label = "Submit"
            )
        ),
        
        # Predict the next word
        mainPanel(
            h3("Suggestion for the next word is : "),
            textOutput("text1"),
            tags$head(tags$style("#text1{color: blue;
                                 font-size: 30px;
                                 font-style: italic;
                                 }"
                                )
                     )
            )
    )
    ),
    tabPanel("About TextPredictor",
             mainPanel(
                 includeMarkdown("Data/AboutTextPredictor.Rmd")
             )
    )
    
))
