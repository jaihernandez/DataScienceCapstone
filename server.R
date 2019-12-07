

source("./genPreds.R")



shinyServer(function(input, output) {

    
    OneGram <- readRDS("./datasets/OneGramFinal.rds")
    TwoGram <- readRDS("./datasets/TwoGramFinal.rds")
    TriGram <- readRDS("./datasets/TriGramFinal.rds")
    FourGram <- readRDS("./datasets/FourGramFinal.rds")
    
    
  
    
    words.predicted.df <- reactive({
        
        clean.input <- cleanData(input$text.input)

        prediction.df <- predWord.4grm(clean.input, OneGram = OneGram,
                                        TwoGram = TwoGram, TriGram = TriGram,
                                        FourGram = FourGram) 
        
        prediction.df <- top_n(prediction.df, input$bins)
        
        
        })
    
    
    pred <- reactive({
        
        pred <- top_n(words.predicted.df(), 1) %>%
            select(Predicted)

        pred <- as.character(pred[1,1])
        
        pred 
        
    })

    
    output$top.pred <- renderText({
        pred()

    })
    
    output$best.df <- renderDataTable({
        df <- words.predicted.df()
        
        is.num <- sapply(df, is.numeric)
        df[is.num] <- lapply(df[is.num], round, 8)
        
        df
 
    }, options = list(searching = F, paging = F, info = F)
    
    
    )
    
    
    
    output$wordPlot <- renderPlot({

            wordcloud(words = words.predicted.df()$Predicted, freq = words.predicted.df()$'GoodTuring Prob',
                  scale=c(4,0.5),
                      colors=brewer.pal(8, "Dark2"))
    })
    
})