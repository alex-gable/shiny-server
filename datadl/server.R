# Server

# Data Downloader App
# Last edited 1-28-2016
# Manny

# Load libraries
require(RSQLite)
require(shiny)

shinyServer(function(input, output, session) {
  
  # Button (HTML)
  output$button <- renderUI(
    tags$a(href = input$select, target = "_parent", 
      tags$button("Download",
        tags$style(type = "text/css",
                          "button {
                           border: 0;
                           background: #0087CC;
                           border-radius: 4px;
                           box-shadow: 0 5px 0 #006599;
                           color: #FFFFFF;
                           cursor: pointer;
                           font: inherit;
                           margin: 0;
                           outline: 0;
                           width: 100%;
                           padding: 12px 20px;
                           transition: all .ls linear;
                           }",
                   
                          "button:active {
                           box-shadow: 0 2px 0 #006599;
                           transform: translateY(3px);
                           }"))))
  
  # Description (HTML)
  output$text <- renderUI({
   
    if (input$select == "http://159.203.24.113:3838/data/pbp20152016.Rda") {
      text <- "Play-By-Play: 2015-2016 (.RData)"
    } else if (input$select == "http://159.203.24.113:3838/data/pbp20082009.Rda") {
      text <- "Play-By-Play: 2008-2009 (.RData)"
    } else if (input$select == "http://159.203.24.113:3838/data/pbp20072008.Rda") {
      text <- "Play-By-Play: 2007-2008 (.RData)"
    } else if (input$select == "http://159.203.24.113:3838/data/roster.Rda") {
      text <- "Roster: Full (.RData)"
    } else {
      text <- "Please select the file you wish to download from the directory above."
    }
    
    tags$div(class = "separator",
      tags$h5(text,
        tags$style(type = "text/css",
                          "h5 {
                           text-align: center;
                           width: 100%;
                           height: 100px;
                           }",
                   
                          ".separator {
                           margin: 0 auto;
                           }"
              )))
    
  })
  
})
