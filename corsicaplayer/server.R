# Server

# Corsica Player App
# Last edited 1-23-2016
# Manny

# Load libraries
library(shiny)
library(dplyr)
library(repmis)
library(Kmisc)
library(DT)

# Load data
load("playerload.Rda") # Remote
# load("~/Documents/github/apps/corsicaplayer/playertest.Rda") # Local

data <- sumplayer

shinyServer(function(input, output) {
  
  # Season inputs
  output$s1 <- renderUI(selectInput("s1", "From", choices = sort(unique(data$Season), decreasing = TRUE), selected = as.character(max(as.numeric(data$Season)))))
  output$s2 <- renderUI(selectInput("s2", "To", choices = sort(unique(data$Season), decreasing = TRUE), selected = as.character(max(as.numeric(data$Season)))))
  
  # Team input
  output$s3 <- renderUI(selectInput("s3", "Team", choices = c("Any", sort(unique(substr(as.character(data$Team), start = 1, stop = 3))), selected = "Any")))
  
  # Player input
  output$name <- renderUI(selectizeInput("name", "Search Players", choices = unique(as.character(data$Player)), selected = NULL, multiple = TRUE))
  
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
      scorevector <- c(-1:1)
    } else if (input$score == "Leading") {
      scorevector <- 1
    } else if (input$score == "Trailing") {
      scorevector <- -1
    } else if (input$score == "Even") {
      scorevector <- 0
    }
    
    # Type input
    if (input$type == "Both") {
      typevector <- c("Regular", "Playoffs")
    } else {
      typevector <- input$type
    }
    
    # Position input
    if (input$pos == "Any") {
      posvector <- unique(as.character(data$Position))
    } else if (input$pos == "Forward") {
      posvector <- c("R", "C", "L")
    } else if (input$pos == "Defence") {
      posvector <- "D"
    }
    
    # Team input
    if (input$s3 == "Any") {
      teamvector <- ""
    } else {
      teamvector <- input$s3
    }
    
    # Players input
    if (length(input$name) < 1) {
      playervector <- tolower(unique(as.character(data$Player)))
    } else {
      playervector <- tolower(input$name)
    }
    
    # Filter
    if (input$aggregate == TRUE) {
      playergp <- filter(data, Season %in% seasonvector & Season.Type %in% typevector) %>% group_by(Player, Season, Season.Type) %>% summarise(GP = max(GP)) %>% data.frame() %>%
        group_by(Player) %>% summarise(GP = sum(GP)) %>% data.frame()
      
      sub <- filter(data, Season %in% seasonvector & {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
      {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Season.Type %in% typevector & Venue %in% venuevector & 
        Position %in% posvector & grepl(teamvector, Team) == TRUE & tolower(Player) %in% playervector) %>% 
        group_by(Player) %>% 
        summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  Season.Type = paste(unique(Season.Type), collapse = "/"),
                  Position = first(Position), Team = first(Team),
                  GP = playergp$GP[match(first(Player), playergp$Player)], TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                  OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), iDIST = sum(iDIST),
                  iCF = sum(iCF), iFF = sum(iFF), iSF = sum(iSF), ixG = sum(ixG),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF), iHA = sum(iHA),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS)) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               iCF60 = iCF/TOI*60, iCSh. = G/iCF*100, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               iFF60 = iFF/TOI*60, iFSh. = G/iFF*100, OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               iSF60 = iSF/TOI*60, iSh. = G/iSF*100, OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ixG60 = ixG/TOI*60, ixSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
               OxGF60 = OxGF/OTOI*60, OxGA60 = OxGA/OTOI*60, Rel.xGF60 = xGF60 - OxGF60, Rel.xGA60 = xGA60 - OxGA60,
               OxFSh. = OxGF/OFF*100, OxFSv. = (1 - (OxGA/OFA))*100, Rel.xFSh. = xFSh. - OxFSh., Rel.xFSv. = xFSv. - OxFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               OACF. = OACF/(OACF + OACA), Rel.ACF. = ACF. - OACF.,
               OACF60 = OACF/OTOI*60, OACA60 = OACA/OTOI*60, Rel.ACF60 = ACF60 - OACF60, Rel.ACA60 = ACA60 - OACA60,
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               OAFF. = OAFF/(OAFF + OAFA), Rel.AFF. = AFF. - OAFF.,
               OAFF60 = OAFF/OTOI*60, OAFA60 = OAFA/OTOI*60, Rel.AFF60 = AFF60 - OAFF60, Rel.AFA60 = AFA60 - OAFA60,
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               OASF. = OASF/(OASF + OASA), Rel.ASF. = ASF. - OASF.,
               OASF60 = OASF/OTOI*60, OASA60 = OASA/OTOI*60, Rel.ASF60 = ASF60 - OASF60, Rel.ASA60 = ASA60 - OASA60,
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               OAGF. = OAGF/(OAGF + OAGA), Rel.AGF. = AGF. - OAGF.,
               OAGF60 = OAGF/OTOI*60, OAGA60 = OAGA/OTOI*60, Rel.AGF60 = AGF60 - OAGF60, Rel.AGA60 = AGA60 - OAGA60,
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               OAxGF. = OAxGF/(OAxGF + OAxGA), Rel.AxGF. = AxGF. - OAxGF.,
               OAxGF60 = OAxGF/OTOI*60, OAxGA60 = OAxGA/OTOI*60, Rel.AxGF60 = AxGF60 - OAxGF60, Rel.AxGA60 = AxGA60 - OAxGA60,
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA),
               OMCF. = OMCF/(OMCF + OMCA), Rel.MCF. = MCF. - OMCF.,
               OMCF60 = OMCF/OTOI*60, OMCA60 = OMCA/OTOI*60, Rel.MCF60 = MCF60 - OMCF60, Rel.MCA60 = MCA60 - OMCA60,
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               OMFF. = OMFF/(OMFF + OMFA), Rel.MFF. = MFF. - OMFF.,
               OMFF60 = OMFF/OTOI*60, OMFA60 = OMFA/OTOI*60, Rel.MFF60 = MFF60 - OMFF60, Rel.MFA60 = MFA60 - OMFA60,
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               OMSF. = OMSF/(OMSF + OMSA), Rel.MSF. = MSF. - OMSF.,
               OMSF60 = OMSF/OTOI*60, OMSA60 = OMSA/OTOI*60, Rel.MSF60 = MSF60 - OMSF60, Rel.MSA60 = MSA60 - OMSA60,
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               OMGF. = OMGF/(OMGF + OMGA), Rel.MGF. = MGF. - OMGF.,
               OMGF60 = OMGF/OTOI*60, OMGA60 = OMGA/OTOI*60, Rel.MGF60 = MGF60 - OMGF60, Rel.MGA60 = MGA60 - OMGA60,
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OMxGF. = OMxGF/(OMxGF + OMxGA), Rel.MxGF. = MxGF. - OMxGF.,
               OMxGF60 = OMxGF/OTOI*60, OMxGA60 = OMxGA/OTOI*60, Rel.MxGF60 = MxGF60 - OMxGF60, Rel.MxGA60 = MxGA60 - OMxGA60,
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               OZSR = OOZS/(OOZS + ODZS)*100, Rel.ZSR = ZSR - OZSR,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DIST = iDIST/iFF,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT, iFO. = iFOW/(iFOW + iFOL)*100) %>%
        data.frame()
    } else {
      sub <- filter(data, Season %in% seasonvector & {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
      {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Season.Type %in% typevector & Venue %in% venuevector &
        Position %in% posvector & grepl(teamvector, Team) == TRUE & tolower(Player) %in% playervector) %>% 
        group_by(Player, Season, Season.Type) %>% 
        summarise(Position = first(Position), Team = first(Team),
                  GP = max(GP), TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA),
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                  OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), iDIST = sum(iDIST),
                  iCF = sum(iCF), iFF = sum(iFF), iSF = sum(iSF), ixG = sum(ixG),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF), iHA = sum(iHA),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS)) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               iCF60 = iCF/TOI*60, iCSh. = G/iCF*100, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               iFF60 = iFF/TOI*60, iFSh. = G/iFF*100, OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               iSF60 = iSF/TOI*60, iSh. = G/iSF*100, OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ixG60 = ixG/TOI*60, ixSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
               OxGF60 = OxGF/OTOI*60, OxGA60 = OxGA/OTOI*60, Rel.xGF60 = xGF60 - OxGF60, Rel.xGA60 = xGA60 - OxGA60,
               OxFSh. = OxGF/OFF*100, OxFSv. = (1 - (OxGA/OFA))*100, Rel.xFSh. = xFSh. - OxFSh., Rel.xFSv. = xFSv. - OxFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               OACF. = OACF/(OACF + OACA), Rel.ACF. = ACF. - OACF.,
               OACF60 = OACF/OTOI*60, OACA60 = OACA/OTOI*60, Rel.ACF60 = ACF60 - OACF60, Rel.ACA60 = ACA60 - OACA60,
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               OAFF. = OAFF/(OAFF + OAFA), Rel.AFF. = AFF. - OAFF.,
               OAFF60 = OAFF/OTOI*60, OAFA60 = OAFA/OTOI*60, Rel.AFF60 = AFF60 - OAFF60, Rel.AFA60 = AFA60 - OAFA60,
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               OASF. = OASF/(OASF + OASA), Rel.ASF. = ASF. - OASF.,
               OASF60 = OASF/OTOI*60, OASA60 = OASA/OTOI*60, Rel.ASF60 = ASF60 - OASF60, Rel.ASA60 = ASA60 - OASA60,
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               OAGF. = OAGF/(OAGF + OAGA), Rel.AGF. = AGF. - OAGF.,
               OAGF60 = OAGF/OTOI*60, OAGA60 = OAGA/OTOI*60, Rel.AGF60 = AGF60 - OAGF60, Rel.AGA60 = AGA60 - OAGA60,
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               OAxGF. = OAxGF/(OAxGF + OAxGA), Rel.AxGF. = AxGF. - OAxGF.,
               OAxGF60 = OAxGF/OTOI*60, OAxGA60 = OAxGA/OTOI*60, Rel.AxGF60 = AxGF60 - OAxGF60, Rel.AxGA60 = AxGA60 - OAxGA60,
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA),
               OMCF. = OMCF/(OMCF + OMCA), Rel.MCF. = MCF. - OMCF.,
               OMCF60 = OMCF/OTOI*60, OMCA60 = OMCA/OTOI*60, Rel.MCF60 = MCF60 - OMCF60, Rel.MCA60 = MCA60 - OMCA60,
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               OMFF. = OMFF/(OMFF + OMFA), Rel.MFF. = MFF. - OMFF.,
               OMFF60 = OMFF/OTOI*60, OMFA60 = OMFA/OTOI*60, Rel.MFF60 = MFF60 - OMFF60, Rel.MFA60 = MFA60 - OMFA60,
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               OMSF. = OMSF/(OMSF + OMSA), Rel.MSF. = MSF. - OMSF.,
               OMSF60 = OMSF/OTOI*60, OMSA60 = OMSA/OTOI*60, Rel.MSF60 = MSF60 - OMSF60, Rel.MSA60 = MSA60 - OMSA60,
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               OMGF. = OMGF/(OMGF + OMGA), Rel.MGF. = MGF. - OMGF.,
               OMGF60 = OMGF/OTOI*60, OMGA60 = OMGA/OTOI*60, Rel.MGF60 = MGF60 - OMGF60, Rel.MGA60 = MGA60 - OMGA60,
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OMxGF. = OMxGF/(OMxGF + OMxGA), Rel.MxGF. = MxGF. - OMxGF.,
               OMxGF60 = OMxGF/OTOI*60, OMxGA60 = OMxGA/OTOI*60, Rel.MxGF60 = MxGF60 - OMxGF60, Rel.MxGA60 = MxGA60 - OMxGA60,
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               OZSR = OOZS/(OOZS + ODZS)*100, Rel.ZSR = ZSR - OZSR,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DIST = iDIST/iFF,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT, iFO. = iFOW/(iFOW + iFOL)*100) %>%
        data.frame()
    }
    
    arrange(sub, Player) %>% select(c(Player, Season, Season.Type, Position, Team, GP, TOI, OTOI, # /Base
                                      G, A1, A2, A, P, P1,
                                      G60, A160, A260, A60, P60, P160,
                                      iCF, iCF60, iCSh., iFF, iFF60, iFSh.,
                                      iSF, iSF60, iSh., ixG, ixG60, ixSh.,
                                      iHF, iHA, iGVA, iTKA, iBLK, 
                                      iFOW, iFOL, iFO.,
                                      iPENT, iPEND, iPENDIFF, Avg.DIST, # /Individual
                                      CF, CA, CF60, CA60, CF., CSh., CSv.,
                                      FF, FA, FF60, FA60, FF., FSh., FSv.,
                                      SF, SA, SF60, SA60, SF., Sh., Sv.,
                                      xGF, xGA, xGF60, xGA60, xGF., xFSh., xFSv., Adj.FSv.,
                                      GF, GA, GF60, GA60, GF.,
                                      FO., PENDIFF, # /On-Ice
                                      OCF, OCA, OCF60, OCA60, OCF., OCSh., OCSv.,
                                      OFF, OFA, OFF60, OFA60, OFF., OFSh., OFSv.,
                                      OSF, OSA, OSF60, OSA60, OSF., OSh., OSv.,
                                      OxGF, OxGA, OxGF60, OxGA60, OxGF., OxFSh., OxFSv.,
                                      OGF, OGA, OGF60, OGA60, OGF., 
                                      OOZS, ODZS, ONZS, OZSR, # /Off-Ice
                                      Rel.CF60, Rel.CA60, Rel.CF., Rel.CSh., Rel.CSv.,
                                      Rel.FF60, Rel.FA60, Rel.FF., Rel.FSh., Rel.FSv.,
                                      Rel.SF60, Rel.SA60, Rel.SF., Rel.Sh., Rel.Sv.,
                                      Rel.GF60, Rel.GA60, Rel.GF., 
                                      Rel.xGF60, Rel.xGA60, Rel.xGF., Rel.xFSh., Rel.xFSv., 
                                      Rel.ZSR, # /Relative
                                      OZS, DZS, NZS, OTF, OZS., DZS., NZS., ZSR,
                                      OZF, DZF, NZF, OZF., DZF., NZF., ZFR, TOI., # /Context
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
                                      MGF, MGA, MGF60, MGA60, MGF.,
                                      OACF, OACA, OACF60, OACA60, OACF.,
                                      OAFF, OAFA, OAFF60, OAFA60, OAFF., 
                                      OASF, OASA, OASF60, OASA60, OASF.,
                                      OAxGF, OAxGA, OAxGF60, OAxGA60, OAxGF.,
                                      OAGF, OAGA, OAGF60, OAGA60, OAGF.,
                                      OMCF, OMCA, OMCF60, OMCA60, OMCF.,
                                      OMFF, OMFA, OMFF60, OMFA60, OMFF., 
                                      OMSF, OMSA, OMSF60, OMSA60, OMSF.,
                                      OMxGF, OMxGA, OMxGF60, OMxGA60, OMxGF.,
                                      OMGF, OMGA, OMGF60, OMGA60, OMGF.,
                                      Rel.ACF., Rel.AFF., Rel.ASF., Rel.AGF., Rel.AxGF., 
                                      Rel.MCF., Rel.MFF., Rel.MSF., Rel.MGF., Rel.MxGF.,
                                      Rel.ACF60, Rel.ACA60, Rel.AFF60, Rel.AFA60, 
                                      Rel.ASF60, Rel.ASA60, Rel.AGF60, Rel.AGA60, 
                                      Rel.AxGF60, Rel.AxGA60,
                                      Rel.MCF60, Rel.MCA60, Rel.MFF60, Rel.MFA60, 
                                      Rel.MSF60, Rel.MSA60, Rel.MGF60, Rel.MGA60, 
                                      Rel.MxGF60, Rel.MxGA60 # /Adjusted
    )) %>% filter(TOI >= input$toi) %>% data.frame()
    
  })
  
  # Table contents
  table.contents <- reactive({
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t1 <- subdata() %>%
      mutate_each(funs(form), -c(Player, Season, Season.Type, Position, Team, GP, ACF:Rel.MxGA60))
    
    # Report input
    if (input$report == "On-Ice") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "CF", "CA", "CF60", "CA60", "CF.", "CSh.", "CSv.",
                                                "FF", "FA", "FF60", "FA60", "FF.", "FSh.", "FSv.",
                                                "SF", "SA", "SF60", "SA60", "SF.", "Sh.", "Sv.",
                                                "GF", "GA", "GF60", "GA60", "GF.",
                                                "xGF", "xGA", "xGF60", "xGA60", "xGF.", "xFSh.", "xFSv.", "Adj.FSv.",
                                                "PENDIFF", "FO."))
    } else if (input$report == "Off-Ice") {
      reportvector <- which(colnames(t1) %in% c("OTOI",
                                                "OCF", "OCA", "OCF60", "OCA60", "OCF.", "OCSh.", "OCSv.",
                                                "OFF", "OFA", "OFF60", "OFA60", "OFF.", "OFSh.", "OFSv.",
                                                "OSF", "OSA", "OSF60", "OSA60", "OSF.", "OSh.", "OSv.",
                                                "OGF", "OGA", "OGF60", "OGA60", "OGF.",
                                                "OxGF", "OxGA", "OxGF60", "OxGA60", "OxGF.", "OxFSh.", "OxFSv.",
                                                "OOZS", "ODZS", "ONZS", "OZSR"))
    } else if (input$report == "Relative") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "Rel.CF60", "Rel.CA60", "Rel.CF.", "Rel.CSh.", "Rel.CSv.",
                                                "Rel.FF60", "Rel.FA60", "Rel.FF.", "Rel.FSh.", "Rel.FSv.",
                                                "Rel.SF60", "Rel.SA60", "Rel.SF.", "Rel.Sh.", "Rel.Sv.",
                                                "Rel.GF60", "Rel.GA60", "Rel.GF.", 
                                                "Rel.xGF60", "Rel.xGA60", "Rel.xGF.", "Rel.xFSh.", "Rel.xFSv.",
                                                "Rel.ZSR"))
    } else if (input$report == "Individual") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "G", "A1", "A2", "A", "P", "P1",
                                                "G60", "A160", "A260", "A60", "P60", "P160",
                                                "iCF", "iCF60", "iCSh.",
                                                "iFF", "iFF60", "iFSh.",
                                                "iSF", "iSF60", "iSh.",
                                                "ixG", "ixG60", "ixSh.",
                                                "iHF", "iHA", "iGVA", "iTKA", "iBLK",
                                                "iFOW", "iFOL", "iFO.",
                                                "iPENT", "iPEND", "iPENDIFF"))
    } else if (input$report == "Context") {
      reportvector <- which(colnames(t1) %in% c("TOI", "TOI.",
                                                "OZS", "DZS", "NZS", "OTF",
                                                "OZF", "DZF", "NZF", 
                                                "OZS.", "DZS.", "NZS.", "OZF.", "DZF.", "NZF.",
                                                "ZSR", "ZFR", "Avg.DISTA"))
    } else if (input$report == "Counts") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "CF", "CA", "FF", "FA", "SF", "SA", "GF", "GA", "xGF", "xGA", 
                                                "OZS", "DZS", "NZS", "OTF", "OZF", "DZF", "NZF", 
                                                "FOW", "FOL", "HF", "HA", "GVA", "TKA", "PENT", "PEND",
                                                "G", "A1", "A2", "A", "P", "P1", 
                                                "iCF", "iFF", "iSF", "ixG",
                                                "iHF", "iHA", "iGVA", "iTKA", "iBLK", 
                                                "iFOW", "iFOL", "iPENT", "iPEND"))
    }
    
    # Adjustment
    if (input$adjust == "Score, Zone and Venue") {
      t1 <- mutate(t1, CF = as.numeric(format(round(ACF, 2), nsmall = 2)), CA = as.numeric(format(round(ACA, 2), nsmall = 2)), CF60 = as.numeric(format(round(ACF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(ACA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*ACF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.ACF., 2), nsmall = 2)),
                   FF = as.numeric(format(round(AFF, 2), nsmall = 2)), FA = as.numeric(format(round(AFA, 2), nsmall = 2)), FF60 = as.numeric(format(round(AFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(AFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*AFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.AFF., 2), nsmall = 2)),
                   SF = as.numeric(format(round(ASF, 2), nsmall = 2)), SA = as.numeric(format(round(ASA, 2), nsmall = 2)), SF60 = as.numeric(format(round(ASF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(ASA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*ASF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.ASF., 2), nsmall = 2)),
                   GF = as.numeric(format(round(AGF, 2), nsmall = 2)), GA = as.numeric(format(round(AGA, 2), nsmall = 2)), GF60 = as.numeric(format(round(AGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(AGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*AGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.AGF., 2), nsmall = 2)),
                   xGF = as.numeric(format(round(AxGF, 2), nsmall = 2)), xGA = as.numeric(format(round(AxGA, 2), nsmall = 2)), xGF60 = as.numeric(format(round(AxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(AxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*AxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.AxGF., 2), nsmall = 2)),
                   OCF = as.numeric(format(round(OACF, 2), nsmall = 2)), OCA = as.numeric(format(round(OACA, 2), nsmall = 2)), OCF60 = as.numeric(format(round(OACF60, 2), nsmall = 2)), OCA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OCF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OFF = as.numeric(format(round(OAFF, 2), nsmall = 2)), OFA = as.numeric(format(round(OAFA, 2), nsmall = 2)), OFF60 = as.numeric(format(round(OAFF60, 2), nsmall = 2)), OFA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OFF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OSF = as.numeric(format(round(OASF, 2), nsmall = 2)), OSA = as.numeric(format(round(OASA, 2), nsmall = 2)), OSF60 = as.numeric(format(round(OASF60, 2), nsmall = 2)), OSA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OSF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OGF = as.numeric(format(round(OAGF, 2), nsmall = 2)), OGA = as.numeric(format(round(OAGA, 2), nsmall = 2)), OGF60 = as.numeric(format(round(OAGF60, 2), nsmall = 2)), OGA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OGF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OxGF = as.numeric(format(round(OAxGF, 2), nsmall = 2)), OxGA = as.numeric(format(round(OAxGA, 2), nsmall = 2)), OxGF60 = as.numeric(format(round(OAxGF60, 2), nsmall = 2)), OxGA60 = as.numeric(format(round(OAxGA60, 2), nsmall = 2)), OxGF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   Rel.CF60 = as.numeric(format(round(Rel.ACF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.ACA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.AFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.AFA60, 2), nsmall = 2)),
                   Rel.SF60 = as.numeric(format(round(Rel.ASF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.ASA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.AGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.AGA60, 2), nsmall = 2)), 
                   Rel.xGF60 = as.numeric(format(round(Rel.AxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.AxGA60, 2), nsmall = 2)))
    } else if (input$adjust == "Score and Venue") {
      t1 <- mutate(t1, CF = as.numeric(format(round(MCF, 2), nsmall = 2)), CA = as.numeric(format(round(MCA, 2), nsmall = 2)), CF60 = as.numeric(format(round(MCF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(MCA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*MCF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.MCF., 2), nsmall = 2)),
                   FF = as.numeric(format(round(MFF, 2), nsmall = 2)), FA = as.numeric(format(round(MFA, 2), nsmall = 2)), FF60 = as.numeric(format(round(MFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(MFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*MFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.MFF., 2), nsmall = 2)),
                   SF = as.numeric(format(round(MSF, 2), nsmall = 2)), SA = as.numeric(format(round(MSA, 2), nsmall = 2)), SF60 = as.numeric(format(round(MSF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(MSA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*MSF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.MSF., 2), nsmall = 2)),
                   GF = as.numeric(format(round(MGF, 2), nsmall = 2)), GA = as.numeric(format(round(MGA, 2), nsmall = 2)), GF60 = as.numeric(format(round(MGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(MGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*MGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.MGF., 2), nsmall = 2)),
                   xGF = as.numeric(format(round(MxGF, 2), nsmall = 2)), xGA = as.numeric(format(round(MxGA, 2), nsmall = 2)), xGF60 = as.numeric(format(round(MxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(MxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*MxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.MxGF., 2), nsmall = 2)),
                   OCF = as.numeric(format(round(OMCF, 2), nsmall = 2)), OCA = as.numeric(format(round(OMCA, 2), nsmall = 2)), OCF60 = as.numeric(format(round(OMCF60, 2), nsmall = 2)), OCA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OCF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OFF = as.numeric(format(round(OMFF, 2), nsmall = 2)), OFA = as.numeric(format(round(OMFA, 2), nsmall = 2)), OFF60 = as.numeric(format(round(OMFF60, 2), nsmall = 2)), OFA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OFF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OSF = as.numeric(format(round(OMSF, 2), nsmall = 2)), OSA = as.numeric(format(round(OMSA, 2), nsmall = 2)), OSF60 = as.numeric(format(round(OMSF60, 2), nsmall = 2)), OSA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OSF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OGF = as.numeric(format(round(OMGF, 2), nsmall = 2)), OGA = as.numeric(format(round(OMGA, 2), nsmall = 2)), OGF60 = as.numeric(format(round(OMGF60, 2), nsmall = 2)), OGA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OGF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OxGF = as.numeric(format(round(OMxGF, 2), nsmall = 2)), OxGA = as.numeric(format(round(OMxGA, 2), nsmall = 2)), OxGF60 = as.numeric(format(round(OMxGF60, 2), nsmall = 2)), OxGA60 = as.numeric(format(round(OMxGA60, 2), nsmall = 2)), OxGF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   Rel.CF60 = as.numeric(format(round(Rel.MCF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.MCA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.MFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.MFA60, 2), nsmall = 2)),
                   Rel.SF60 = as.numeric(format(round(Rel.MSF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.MSA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.MGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.MGA60, 2), nsmall = 2)), 
                   Rel.xGF60 = as.numeric(format(round(Rel.MxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.MxGA60, 2), nsmall = 2)))
    }
    
    colnames(t1) <- gsub("[.]$", "%", colnames(t1))
    select(t1, c(Player, Season, Season.Type, Position, Team, GP, reportvector))
    
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
      formatStyle('Player', fontWeight = "bold")
    
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
                 "SAP_Series_Predictions.csv"
                 )[sample(1:8, 1)],
    content = function(file) {
      write.csv(table.contents(), file)
    }
  )
  
})
