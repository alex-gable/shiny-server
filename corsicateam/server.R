# Server

# Corsica Team App
# Last edited 1-23-2016
# Manny

# Load libraries
library(shiny)
library(dplyr)
library(repmis)
library(Kmisc)
library(DT)

# Load data
load("teamload.Rda") # Remote
#load("~/Documents/github/shiny-server/corsicateam/teamload.Rda") # Local

data <- sumteam

shinyServer(function(input, output) {
  
  # Season inputs
  output$s1 <- renderUI(selectInput("s1", "From", choices = sort(unique(data$Season), decreasing = TRUE), selected = as.character(max(as.numeric(data$Season)))))
  output$s2 <- renderUI(selectInput("s2", "To", choices = sort(unique(data$Season), decreasing = TRUE), selected = as.character(max(as.numeric(data$Season)))))
  
  # Subset data
  subdata <- reactive({
    
    # Season input
    seasonvector <- as.character(seq(from = as.numeric(input$s1), to = as.numeric(input$s2), by = 10001))
  
    # Strength input
    if (input$strength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4", "XvX")
    } else {
      strengthvector <- input$strength
    }
    
    # Venue input
    if (input$venue == "Any") {
      venuevector <- c("Home", "Away")
    } else {
      venuevector <- input$venue
    }
    
    # Score input
    if (input$score == "Any") {
      scorevector <- c(-3:3)
    } else if (input$score == "Leading") {
      scorevector <- c(1:3)
    } else if (input$score == "Trailing") {
      scorevector <- c(-3:-1)
    } else if (input$score == "Even") {
      scorevector <- 0
    } else {
      scorevector <- as.numeric(input$score)
    }
    
    # Type input
    if (input$type == "Both") {
      typevector <- c("Regular", "Playoffs")
    } else {
      typevector <- input$type
    }
    
    # Filter
    if (input$aggregate == TRUE) {
      teamgp <- filter(data, Season %in% seasonvector & Season.Type %in% typevector) %>% group_by(Team, Season, Season.Type) %>% summarise(GP = max(GP)) %>% data.frame() %>%
        group_by(Team) %>% summarise(GP = sum(GP)) %>% data.frame()
      
      sub <- filter(data, Season %in% seasonvector & {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
                    {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Season.Type %in% typevector & Venue %in% venuevector) %>% 
        group_by(Team) %>% 
        summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  Season.Type = paste(unique(Season.Type), collapse = "/"),
                  GP = teamgp$GP[match(first(Team), teamgp$Team)], TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
                  DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), DISTF = sum(DISTF), DISTA = sum(DISTA)) %>% 
        mutate(CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, 
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, 
               xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100, Adj.FSv. = FSv. - xFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA), 
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DISTF = DISTF/FF, Avg.DISTA = DISTA/FA) %>%
        data.frame()
    } else {
      sub <- filter(data, Season %in% seasonvector & {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
                    {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Season.Type %in% typevector & Venue %in% venuevector) %>% 
        group_by(Team, Season, Season.Type) %>% 
        summarise(GP = max(GP), TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA),
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
                  DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), DISTF = sum(DISTF), DISTA = sum(DISTA)) %>% 
        mutate(CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, 
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, 
               xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100, Adj.FSv. = FSv. - xFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA), 
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DISTF = DISTF/FF, Avg.DISTA = DISTA/FA) %>%
        data.frame()
    }
    
    arrange(sub, Team) %>% select(c(Team, Season, Season.Type, GP, TOI, # /Base
                                    CF, CA, CF60, CA60, CF., CSh., CSv.,
                                    FF, FA, FF60, FA60, FF., FSh., FSv.,
                                    SF, SA, SF60, SA60, SF., Sh., Sv.,
                                    xGF, xGA, xGF60, xGA60, xGF., 
                                    xFSh., xFSv., Adj.FSv.,
                                    GF, GA, GF60, GA60, GF.,
                                    FO., PENDIFF, # /On-Ice
                                    OZS, DZS, NZS, OZS., DZS., NZS., ZSR,
                                    OZF, DZF, NZF, OZF., DZF., NZF., ZFR,
                                    Avg.DISTF, Avg.DISTA, # /Context
                                    FOW, FOL, HF, HA, GVA, TKA, PENT, PEND, # /Counts
                                    ACF, ACA, ACF60, ACA60, ACF.,
                                    AFF, AFA, AFF60, AFA60, AFF., 
                                    ASF, ASA, ASF60, ASA60, ASF.,
                                    AxGF, AxGA, AxGF60, AxGA60, AxGF.,
                                    AGF, AGA, AGF60, AGA60, AGF.,
                                    MCF, MCA, MCF60, MCA60, MCF.,
                                    MFF, MFA, MFF60, MFA60, MFF., 
                                    MSF, MSA, MSF60, MSA60, MSF.,
                                    MxGF, MxGA, MxGF60, MxGA60, MxGF.,
                                    MGF, MGA, MGF60, MGA60, MGF. # /Adjusted
                                    )) %>% data.frame()
    
  })
  
  # Table contents
  table.contents <- reactive({
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t1 <- subdata() %>%
      mutate_each(funs(form), -c(Team, Season, Season.Type, ACF:MGF.))
    
    # Report input
    if (input$report == "On-Ice") {
      reportvector <- which(colnames(t1) %in% c("CF", "CA", "CF60", "CA60", "CF.", "CSh.", "CSv.",
                                                "FF", "FA", "FF60", "FA60", "FF.", "FSh.", "FSv.",
                                                "SF", "SA", "SF60", "SA60", "SF.", "Sh.", "Sv.",
                                                "GF", "GA", "GF60", "GA60", "GF.",
                                                "xGF", "xGA", "xGF60", "xGA60", "xGF.", "xFSh.", "xFSv.", "Adj.FSv.",
                                                "PENDIFF", "FO."))
    } else if (input$report == "Context") {
      reportvector <- which(colnames(t1) %in% c("OZS", "DZS", "NZS", "OZF", "DZF", "NZF", 
                                                "OZS.", "DZS.", "NZS.", "OZF.", "DZF.", "NZF.",
                                                "ZSR", "ZFR", "Avg.DISTF", "Avg.DISTA"))
    } else if (input$report == "Counts") {
      reportvector <- which(colnames(t1) %in% c("CF", "CA", "FF", "FA", "SF", "SA", "GF", "GA", "xGF", "xGA", 
                                                "OZS", "DZS", "NZS", "OZF", "DZF", "NZF", 
                                                "FOW", "FOL", "HF", "HA", "GVA", "TKA", "PENT", "PEND"))
    }
    
    # Adjustment
    if (input$adjust == "Score, Zone and Venue") {
      t1 <- mutate(t1, CF = format(round(ACF, 2), nsmall = 2), CA = format(round(ACA, 2), nsmall = 2), CF60 = format(round(ACF60, 2), nsmall = 2), CA60 = format(round(ACA60, 2), nsmall = 2), CF. = format(round(100*ACF., 2), nsmall = 2),
                   FF = format(round(AFF, 2), nsmall = 2), FA = format(round(AFA, 2), nsmall = 2), FF60 = format(round(AFF60, 2), nsmall = 2), FA60 = format(round(AFA60, 2), nsmall = 2), FF. = format(round(100*AFF., 2), nsmall = 2),
                   SF = format(round(ASF, 2), nsmall = 2), SA = format(round(ASA, 2), nsmall = 2), SF60 = format(round(ASF60, 2), nsmall = 2), SA60 = format(round(ASA60, 2), nsmall = 2), SF. = format(round(100*ASF., 2), nsmall = 2),
                   GF = format(round(AGF, 2), nsmall = 2), GA = format(round(AGA, 2), nsmall = 2), GF60 = format(round(AGF60, 2), nsmall = 2), GA60 = format(round(AGA60, 2), nsmall = 2), GF. = format(round(100*AGF., 2), nsmall = 2),
                   xGF = format(round(AxGF, 2), nsmall = 2), xGA = format(round(AxGA, 2), nsmall = 2), xGF60 = format(round(AxGF60, 2), nsmall = 2), xGA60 = format(round(AxGA60, 2), nsmall = 2), xGF. = format(round(100*AxGF., 2), nsmall = 2))
    } else if (input$adjust == "Score and Venue") {
      t1 <- mutate(t1, CF = format(round(MCF, 2), nsmall = 2), CA = format(round(MCA, 2), nsmall = 2), CF60 = format(round(MCF60, 2), nsmall = 2), CA60 = format(round(MCA60, 2), nsmall = 2), CF. = format(round(100*MCF., 2), nsmall = 2),
                   FF = format(round(MFF, 2), nsmall = 2), FA = format(round(MFA, 2), nsmall = 2), FF60 = format(round(MFF60, 2), nsmall = 2), FA60 = format(round(MFA60, 2), nsmall = 2), FF. = format(round(100*MFF., 2), nsmall = 2),
                   SF = format(round(MSF, 2), nsmall = 2), SA = format(round(MSA, 2), nsmall = 2), SF60 = format(round(MSF60, 2), nsmall = 2), SA60 = format(round(MSA60, 2), nsmall = 2), SF. = format(round(100*MSF., 2), nsmall = 2),
                   GF = format(round(MGF, 2), nsmall = 2), GA = format(round(MGA, 2), nsmall = 2), GF60 = format(round(MGF60, 2), nsmall = 2), GA60 = format(round(MGA60, 2), nsmall = 2), GF. = format(round(100*MGF., 2), nsmall = 2),
                   xGF = format(round(MxGF, 2), nsmall = 2), xGA = format(round(MxGA, 2), nsmall = 2), xGF60 = format(round(MxGF60, 2), nsmall = 2), xGA60 = format(round(MxGA60, 2), nsmall = 2), xGF. = format(round(100*MxGF., 2), nsmall = 2))
    }
    
    colnames(t1) <- gsub("[.]$", "%", colnames(t1))
    select(t1, c(Team, Season, Season.Type, GP, TOI, reportvector))
    
  })
  
  # Table output
  output$t1 <- DT::renderDataTable({
    
    t1 <- table.contents()
    datatable(t1, 
              extensions = list(FixedColumns = list(leftColumns = 2)),
              options = list(searching = F, paging = T, pageLength = 50, scrollX = T, info = FALSE, autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             selection = list(mode = 'single', target = 'row'),
                             dom = 'ltp',
                             scrollCollapse = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4863A0', 'color': '#fff'});",
                               "}")),
              rownames = F) %>%
      formatStyle('Team', fontWeight = "bold")
    
  })
  
  # Output file
  output$dl <- downloadHandler(
    filename = c("stats_for_nerds.csv", 
                 "MadeUpJunk.csv", 
                 "VSM_Rankings.csv",
                 "TOP-SECRET.csv",
                 "randomnumbers.csv",
                 "larry.jpeg.csv",
                 "A.MacDonald_ContractInfo.csv",
                 "SAP_Series_Predictions.csv",
                 "guy_serota_tweets.csv"
    )[sample(1:9, 1)],
    content = function(file) {
      write.csv(table.contents(), file)
    }
  )
  
})
