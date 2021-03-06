# User Interface

# Corsica Combo App
# Last edited 1-26-2016
# Manny

shinyUI(navbarPage("Combos", id = "tab", inverse = F, windowTitle = "Corsica | Combos",
                   
                   tabPanel("Lines", value = "line",
                            
                            tags$head(tags$style(".container-fluid {font-size: 13px; color: #2B547E; background-color: #E8E8E8;}")),
                            tags$head(tags$style(".rightAlign{float:right;}")),
                            tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}", ".shiny-output-error:before { visibility: hidden;}"),
                            tags$style(".navbar-default {background-color: #4863A0; border-color: #ffffff;}"),
                            tags$style(".navbar-default .navbar-nav li a {background-color: #4863A0; color: #ffffff;}"),
                            tags$style(".navbar-default .navbar-nav .active a {background-color: #ffffff; color: #4863A0;}"),
                            tags$style(".navbar-default .navbar-brand {background-color: #4863A0; color: #ffffff;}"),
                            tags$style(".dataTable thead tr {background-color: #4863A0; color: #ffffff;}"),
                            
                            # Header text
                            fluidRow(
                              column(6, h2("Line Stats")),
                              column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                            ),
                            
                            # Help text
                            fluidRow(
                              column(6, helpText("Loading the data may take a few seconds. Thanks for your patience.")),
                              column(6, helpText("", class = "rightAlign"))
                            ),
                            
                            # Input row 1
                            fluidRow(
                              column(2, uiOutput("l1")),
                              column(2, selectInput("lstrength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "3v3"), selected = "5v5")),
                              column(2, selectInput("ladjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
                              column(2, selectInput("ltype", "Season Type", choices = c("Regular", "Playoffs", "Both"), selected = "Regular")),
                              column(2, uiOutput("l3")),
                              column(2, checkboxInput("laggregate", "Aggregate Seasons", value = TRUE))
                            ),
                            
                            # Input row 2
                            fluidRow(
                              column(2, uiOutput("l2")),
                              column(2, selectInput("lreport", "Report", choices = c("On-Ice", "Off-Ice", "Relative", "Individual", "Context", "Counts"), selected = "On-Ice")),
                              column(3, sliderInput("ltoi", "TOI Minimum", min = 0, max = 5000, value = 50, step = 50)),
                              column(3, uiOutput("lname")),
                              column(2, downloadButton("ldl", "Download File"))
                            ),
                            
                            # Output
                            DT::dataTableOutput("t1")
                            
                            ),
                   
                   tabPanel("Pairings", value = "pair",
                            
                            # Header text
                            fluidRow(
                              column(6, h2("Pairing Stats")),
                              column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                            ),
                            
                            # Help text
                            fluidRow(
                              column(6, helpText("Loading the data may take a few seconds. Thanks for your patience.")),
                              column(6, helpText("", class = "rightAlign"))
                            ),
                            
                            # Input row 1
                            fluidRow(
                              column(2, uiOutput("p1")),
                              column(2, selectInput("pstrength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "3v3"), selected = "5v5")),
                              column(2, selectInput("padjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
                              column(2, selectInput("ptype", "Season Type", choices = c("Regular", "Playoffs", "Both"), selected = "Regular")),
                              column(2, uiOutput("p3")),
                              column(2, checkboxInput("paggregate", "Aggregate Seasons", value = TRUE))
                            ),
                            
                            # Input row 2
                            fluidRow(
                              column(2, uiOutput("p2")),
                              column(2, selectInput("preport", "Report", choices = c("On-Ice", "Off-Ice", "Relative", "Individual", "Context", "Counts"), selected = "On-Ice")),
                              column(3, sliderInput("ptoi", "TOI Minimum", min = 0, max = 5000, value = 50, step = 50)),
                              column(3, uiOutput("pname")),
                              column(2, downloadButton("pdl", "Download File"))
                            ),
                            
                            # Output
                            DT::dataTableOutput("t2")
                            
                            )
                   
))

                   
