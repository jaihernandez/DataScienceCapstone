library(shiny)
library(wordcloud)
library(shinydashboard)
library(dplyr)
library(tm)
library(stringr)
library(wordcloud)
library(textclean)
library(DT)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Data Science Capstone - Next Word Prediction"),
    
    # Sidebar with a slider input for number of vacation spots
    sidebarLayout(
        sidebarPanel(
            textInput("text.input", "Enter word or phrase:", value = "science"),
            sliderInput("bins",
                        "Maximum Number of Words:",
                        min = 1,
                        max = 50,
                        value = 5)
        ),
        
        # Output!
        mainPanel(
            
            box( width = 12, 
                     style = "border: .7px solid LightGray; margin: 0px; padding: 20px;",
            fluidRow(column(2, h3("Prediction:"), style = "padding: 5px;"), 
                     strong(textOutput("top.pred"), style = "font-size: 1.3em; color:#036ffc; text-align: left; padding: 0.016px;")
            # fluidRow(column(2,
            #                 strong(textOutput("top.pred"), style = "font-size: 1.3em; color:#86BC25; text-align: left;"),
            #                 
            #                 )
                
            )),
            fluidRow(),
            br(),
            box(width = 6, title = "Top Predictions",
                style = "border: .7px solid LightGray; margin: 0px; padding: 20px;",
                fluidRow(
                    div(dataTableOutput("best.df"), style = "font-size: 90%; width: 70%;")
                    )
                ),
            box(width = 6,
                title = "Top Predictions Word Cloud",
                style = "border: .7px solid LightGray; margin: 0px; padding: 20px;",
                fluidRow(
                    plotOutput("wordPlot")
                )
            ),
            br()
            
        )
        
        
    )
    
   
    
    
    
))

