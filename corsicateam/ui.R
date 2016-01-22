# User Interface

# Corsica Team App
# Last edited 1-18-2016
# Manny

shinyUI(fluidPage(
  tags$head(tags$style(".container-fluid {font-size: 13px; color: #2B547E; background-color: #E8E8E8;}")),
  tags$head(tags$style(".rightAlign{float:right;}")),
  tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}", ".shiny-output-error:before { visibility: hidden;}"),
  tags$style(".dataTable thead tr {background-color: #4863A0; color: #ffffff;}"),
  
  # Header text
  fluidRow(
    column(6, h2("Team Stats")),
    column(6, h2("Corsica", class = "rightAlign"))
  ),
  
  # Help text
  fluidRow(
    column(6, helpText("Loading the data may take a few seconds. Thanks for your patience.")),
    column(6, helpText("Hockey stats for the informed fan", class = "rightAlign"))
  ),
  
  # Input row 1
  fluidRow(
    column(2, uiOutput("s1")),
    column(2, selectInput("strength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4"), selected = "5v5")),
    column(2, selectInput("venue", "Venue", choices = c("Any", "Home", "Away"), selected = "Any")),
    column(2, selectInput("type", "Season Type", choices = c("Regular", "Playoffs", "Both"), selected = "Regular")),
    column(3, selectInput("adjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None"))
  ),
  
  fluidRow(
    column(2, uiOutput("s2")),
    column(2, selectInput("score", "Score State", choices = c("Any", "Leading", "Trailing", "Even", "+3", "+2", "+1", "-1", "-2", "-3"), selected = "Any")),
    column(2, selectInput("report", "Report", choices = c("On-Ice", "Context", "Counts"), selected = "On-Ice")),
    column(2, downloadButton("dl", "Download File")),
    column(3, checkboxInput("aggregate", "Aggregate Seasons", value = TRUE))
  ),
  
  # Table output
  DT::dataTableOutput("t1")
  
))