# Push (Season)
# Last edited 1/31/2016 / Mise-a-jour 1-31-2016
# Manny

## NOTES:
# PUSH CSV VERSIONS
# MIND SIZE OF DOWNLOAD FILES
# PUSH SQLITE FRAGMENTS TO SHINY SERVER

# Load libraries / Charger paquets
library(RSQLite)
library(dplyr)

## Load DB tables / Charger tables de la base de données
# Link to database / Connecter a la base de données
link <- "/srv/shiny-server/corsica.sqlite"
newcon <- dbConnect(SQLite(), link)

# Provide current season / Fournir saison actuelle
season <- "20152016"

# Functions / Fonctions
code <- function(a, b, c) {
  sorted <- sort(c(first(a), first(b), first(c)), decreasing = FALSE)
  p1 <- sorted[1]
  p2 <- sorted[2]
  p3 <- sorted[3]
  return(paste(p1, p2, p3, sep = "-"))
}

start <- Sys.time()

# Read tables / Lire les tables 
roster <- dbReadTable(newcon, "roster")

bio <- dbReadTable(newcon, "bio")

pbpquery <- dbSendQuery(newcon, paste("SELECT * FROM pbp WHERE Season = ", season, sep = ""))
pbp <- fetch(pbpquery, -1)

teamquery <- dbSendQuery(newcon, paste("SELECT * FROM team WHERE Season = ", season, sep = ""))
team <- fetch(teamquery, -1)

goaliequery <- dbSendQuery(newcon, paste("SELECT * FROM goalie WHERE Season = ", season, sep = ""))
goalie <- fetch(goaliequery, -1)

playerquery <- dbSendQuery(newcon, paste("SELECT * FROM player WHERE Season = ", season, sep = ""))
player <- fetch(playerquery, -1)

comboquery <- dbSendQuery(newcon, paste("SELECT * FROM combo WHERE Season = ", season, sep = ""))
combo <- fetch(comboquery, -1)

# Load current files / Charger fichiers 
load("~/Documents/github/shiny-server/corsicateam/teamload.Rda")
load("~/Documents/github/shiny-server/corsicagoalie/goalieload.Rda")
load("~/Documents/github/shiny-server/corsicaplayer/playerload.Rda")
load("~/Documents/github/shiny-server/corsicacombo/pairload.Rda")
load("~/Documents/github/shiny-server/corsicacombo/lineload.Rda")

# Remove current season / Éliminer saison actuelle
teamhist <- filter(sumteam, Season != season) %>% data.frame()
goaliehist <- filter(sumgoalie, Season != season) %>% data.frame()
playerhist <- filter(sumplayer, Season != season) %>% data.frame()
pairhist <- filter(sumpair, Season != season) %>% data.frame()
linehist <- filter(sumline, Season != season) %>% data.frame()

## Aggregate stats / Agréger les statistiques

# Roster / Formation
roster <- group_by(roster, Full.Name, Season, Season.Type) %>% 
  summarise(Team = paste(unique(Team), collapse = "/"), Number = paste(unique(Number), collapse = "/"), Team.Num = paste(unique(Team.Num), collapse = "/"), 
            Position = paste(unique(Position), collapse = "/"), Last.Name = first(Last.Name), First.Name = first(First.Name)) %>%
  data.frame() %>%
  merge(bio, by.x = "Player.Code", by.y = "Player.Code", all.x = TRUE) %>%
  data.frame()

# Team / Équipes
teamgp <- group_by(team, Team, Season, Season.Type) %>% summarise(GP = length(unique(Newcode))) %>% data.frame() %>%
  mutate(Code = paste(Team, Season, Season.Type, sep = ".")) %>% data.frame()

# Group leftover strength states / Combiner états de forces mineurs 
team$Strength.State[which(team$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4", "0v0") == FALSE)] <- "XvX" 

sumteam <- filter(team, Strength.State != "0v0") %>% group_by(Team, Season, Venue, Strength.State, Score.Cat, Season.Type) %>%
  summarise(GP = teamgp$GP[match(paste(first(Team), first(Season), first(Season.Type), sep = "."), teamgp$Code)], 
            TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
            xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
            AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
            DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
            PENT = sum(PENT), PEND = sum(PEND), DISTF = sum(DISTF), DISTA = sum(DISTA),
            RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% data.frame()

# Goalie / Gardiens
goaliegp <- group_by(goalie, Player, Season, Season.Type) %>% summarise(GP = length(unique(Newcode))) %>% data.frame() %>%
  mutate(Code = paste(Player, Season, Season.Type, sep = ".")) %>% data.frame()

# Group leftover strength states / Combiner états de forces mineurs 
goalie$Strength.State[which(goalie$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4", "0v0") == FALSE)] <- "XvX" 

sumgoalie <- filter(goalie, Strength.State != "0v0") %>% group_by(Player, Season, Venue, Strength.State, Score.Cat, Season.Type) %>%
  summarise(GP = goaliegp$GP[match(paste(first(Player), first(Season), first(Season.Type), sep = "."), goaliegp$Code)], 
            Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), 
            GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
            AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
            DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
            PENT = sum(PENT), PEND = sum(PEND), DISTA = sum(DISTA), G = sum(G), A1 = sum(na.omit(A1)), A2 = sum(na.omit(A2)), SOA = sum(SOA), SOG = sum(SOG),
            iPENT = sum(iPENT), iPEND = sum(na.omit(iPEND)),
            RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
            iRB = sum(iRB), iRS = sum(iRS)) %>% data.frame()

# Player / Joueurs 
playergp <- group_by(player, Player, Season, Season.Type) %>% summarise(GP = length(unique(Newcode))) %>% data.frame() %>%
  mutate(Code = paste(Player, Season, Season.Type, sep = ".")) %>% data.frame()

# Group leftover strength states / Combiner états de forces mineurs 
player$Strength.State[which(player$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "0v0") == FALSE)] <- "XvX"

# Group score states / Combiner états de score
player$Score.Cat[which(player$Score.Cat < 0)] <- -1
player$Score.Cat[which(player$Score.Cat > 0)] <- 1

sumplayer <- filter(player, Strength.State != "0v0") %>% group_by(Player, Season, Venue, Strength.State, Score.Cat, Season.Type) %>%
  summarise(GP = playergp$GP[match(paste(first(Player), first(Season), first(Season.Type), sep = "."), playergp$Code)], 
            Position = roster$Position[match(first(Player), roster$Full.Name)],
            Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), CF = sum(CF), CA = sum(CA), iCF = sum(iCF), FF = sum(FF), FA = sum(FA), iFF = sum(iFF), 
            SF = sum(SF), SA = sum(SA), iSF = sum(iSF), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA), ixG = sum(ixG), 
            ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
            AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
            OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
            FOW = sum(FOW), FOL = sum(FOL), iFOW = sum(iFOW), iFOL = sum(iFOL), HF = sum(HF), HA = sum(HA), iHF = sum(iHF), iHA = sum(iHA), 
            GVA = sum(GVA), TKA = sum(TKA), iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK), PENT = sum(PENT), PEND = sum(PEND), 
            iDIST = sum(iDIST), G = sum(G), A1 = sum(na.omit(A1)), A2 = sum(na.omit(A2)), SOA = sum(SOA), SOG = sum(SOG), iPENT = sum(iPENT), iPEND = sum(na.omit(iPEND)),
            tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
            tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
            tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
            tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
            tOZS = sum(tOZS), tDZS = sum(tDZS), tNZS = sum(tNZS),
            RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
            iRB = sum(iRB), iRS = sum(iRS)) %>% data.frame() %>%
  mutate(OCF = tCF - CF, OCA = tCA - CA,
         OFF = tFF - FF, OFA = tFA - FA,
         OSF = tSF - SF, OSA = tSA - SA,
         OGF = tGF - GF, OGA = tGA - GA,
         OxGF = txGF - xGF, OxGA = txGA - xGA, 
         OACF = tACF - ACF, OACA = tACA - ACA,
         OAFF = tAFF - AFF, OAFA = tAFA - AFA,
         OASF = tASF - ASF, OASA = tASA - ASA,
         OAGF = tAGF - AGF, OAGA = tAGA - AGA,
         OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
         OMCF = tMCF - MCF, OMCA = tMCA - MCA,
         OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
         OMSF = tMSF - MSF, OMSA = tMSA - MSA,
         OMGF = tMGF - MGF, OMGA = tMGA - MGA,
         OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
         OOZS = tOZS - OZS, ODZS = tDZS - DZS, ONZS = tNZS - NZS) %>% data.frame() %>%
  select(-c(tCF:tNZS)) %>%
  data.frame()

# Combo / Combinaisons

# Group leftover strength states / Combiner états de forces mineurs 
combo$Strength.State[which(combo$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "3v3", "0v0") == FALSE)] <- "XvX" # EXCLUDE SHOOTOUT

# List regular combinations / Chercher combinaisons communs
regcombo <- group_by(combo, Combo.Code, Season, Season.Type) %>% summarise(TOI = sum(TOI)) %>% mutate(Newcode = paste(Combo.Code, Season, Season.Type, sep = "-")) %>% filter(TOI >= 25) %>% data.frame()

sumline <- filter(combo, grepl("C|L|R", as.character(P3.POS)) == TRUE & grepl("C|L|R", as.character(P2.POS)) == TRUE & grepl("C|L|R", as.character(P1.POS)) == TRUE & Strength.State != "0v0") %>% 
  group_by(Combo.Code, Season, Strength.State, Season.Type, Venue) %>%
  summarise(Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), P1 = first(P1), P1.POS = first(P1.POS), 
            P2 = first(P2), P2.POS = first(P2.POS), P3 = first(P3), P3.POS = first(P3.POS),
            CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
            SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
            ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
            AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
            OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
            FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
            GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND),
            RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
            P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
            P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
            P3.G = sum(P3.G), P3.A1 = sum(na.omit(P3.A1)), P3.A2 = sum(na.omit(P3.A2)),
            tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
            tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
            tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
            tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
            tOZS = sum(tOZS), tDZS = sum(DZS), tNZS = sum(tNZS)) %>% data.frame() %>%
  mutate(OCF = tCF - CF, OCA = tCA - CA,
         OFF = tFF - FF, OFA = tFA - FA,
         OSF = tSF - SF, OSA = tSA - SA,
         OGF = tGF - GF, OGA = tGA - GA,
         OxGF = txGF - xGF, OxGA = txGA - xGA, 
         OACF = tACF - ACF, OACA = tACA - ACA,
         OAFF = tAFF - AFF, OAFA = tAFA - AFA,
         OASF = tASF - ASF, OASA = tASA - ASA,
         OAGF = tAGF - AGF, OAGA = tAGA - AGA,
         OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
         OMCF = tMCF - MCF, OMCA = tMCA - MCA,
         OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
         OMSF = tMSF - MSF, OMSA = tMSA - MSA,
         OMGF = tMGF - MGF, OMGA = tMGA - MGA,
         OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
         OOZS = tOZS - OZS, ODZS = tDZS - DZS, ONZS = tNZS - NZS,
         Newcode = paste(Combo.Code, Season, Season.Type, sep = "-")) %>%
  select(-c(tCF:tMxGA)) %>% data.frame() %>% 
  filter(Newcode %in% regcombo$Newcode) %>% select(-c(Newcode)) %>% data.frame()

sumpair <- filter(combo, as.character(P3) == "X" & grepl("D", as.character(P2.POS)) == TRUE & grepl("D", as.character(P1.POS)) == TRUE & Strength.State != "0v0") %>% 
  group_by(Combo.Code, Season, Strength.State, Season.Type, Venue) %>%
  summarise(Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), 
            P1 = first(P1), P1.POS = first(P1.POS), P2 = first(P2), P2.POS = first(P2.POS),
            CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
            SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
            ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
            AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
            OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
            FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
            GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND), 
            RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
            P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
            P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
            tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
            tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
            tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
            tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
            tOZS = sum(tOZS), tDZS = sum(DZS), tNZS = sum(tNZS)) %>% data.frame() %>%
  mutate(OCF = tCF - CF, OCA = tCA - CA,
         OFF = tFF - FF, OFA = tFA - FA,
         OSF = tSF - SF, OSA = tSA - SA,
         OGF = tGF - GF, OGA = tGA - GA,
         OxGF = txGF - xGF, OxGA = txGA - xGA, 
         OACF = tACF - ACF, OACA = tACA - ACA,
         OAFF = tAFF - AFF, OAFA = tAFA - AFA,
         OASF = tASF - ASF, OASA = tASA - ASA,
         OAGF = tAGF - AGF, OAGA = tAGA - AGA,
         OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
         OMCF = tMCF - MCF, OMCA = tMCA - MCA,
         OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
         OMSF = tMSF - MSF, OMSA = tMSA - MSA,
         OMGF = tMGF - MGF, OMGA = tMGA - MGA,
         OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
         OOZS = tOZS - OZS, ODZS = tDZS - DZS, ONZS = tNZS - NZS,
         Newcode = paste(Combo.Code, Season, Season.Type, sep = "-")) %>%
  select(-c(tCF:tMxGA)) %>% data.frame() %>%
  filter(Newcode %in% regcombo$Newcode) %>% select(-c(Newcode)) %>% data.frame()
# ASSIST NETWORK

# QoC, QoT

# Build player network / Construire réseau de joueurs
plist <- c(unique(player$Player), "GOALIE")

pmatrix <- cbind(rep(plist, each = length(plist)), rep(plist, times = length(plist))) %>%
  as.data.frame(stringsAsFactors = FALSE)

colnames(pmatrix) <- c("Player", "Without")

pmatrix <- group_by(pmatrix, Player, Without) %>% mutate(Dummy = "X", Season = season, Combo.Code = code(first(Player), first(Without), first(Dummy))) %>% data.frame()

# Condense player and combo tables / Condenser tables de joueurs et combinaisons
player.ref <- filter(player, Strength.State == "5v5") %>% 
  group_by(Player, Season) %>%
  summarise(TOI = sum(TOI),
            tTOI = sum(tTOI),
            CF = sum(CF), CA = sum(CA),
            xGF = sum(xGF), xGA = sum(xGA)) %>%
  mutate(TOI. = TOI/tTOI) %>%
  data.frame()

combo.ref <- filter(combo, Strength.State == "5v5" & P3 == "X") %>% 
  group_by(Combo.Code, Season) %>%
  summarise(CF = sum(CF), CA = sum(CA),
            xGF = sum(xGF), xGA = sum(xGA)) %>%
  data.frame()

# Compile player-without stats / Calculer statistiques pour chaque joueur sans l'autre
player.without <- rbind_list(
  filter(pmatrix, pmatrix$Combo.Code %in% combo.ref$Combo.Code) %>% group_by(Player, Without, Season) %>% 
    mutate(Player.TOI. = first(player.ref$TOI.[match(Player, player.ref$Player)]),
           Player.CF = first(player.ref$CF[match(Player, player.ref$Player)]),
           Player.CA = first(player.ref$CA[match(Player, player.ref$Player)]),
           Player.xGF = first(player.ref$xGF[match(Player, player.ref$Player)]),
           Player.xGA = first(player.ref$xGA[match(Player, player.ref$Player)]),
           Combo.CF = first(combo.ref$CF[match(Combo.Code, combo.ref$Combo.Code)]),
           Combo.CA = first(combo.ref$CA[match(Combo.Code, combo.ref$Combo.Code)]),
           Combo.xGF = first(combo.ref$xGF[match(Combo.Code, combo.ref$Combo.Code)]),
           Combo.xGA = first(combo.ref$xGA[match(Combo.Code, combo.ref$Combo.Code)]),
           Without.CF = Player.CF - Combo.CF,
           Without.CA = Player.CA - Combo.CA,
           Without.CF. = Without.CF/(Without.CF + Without.CA),
           Without.xGF = Player.xGF - Combo.xGF,
           Without.xGA = Player.xGA - Combo.xGA,
           Without.xGF. = Without.xGF/(Without.xGF + Without.xGA)),
  filter(pmatrix, pmatrix$Combo.Code %in% combo.ref$Combo.Code == FALSE) %>% group_by(Player, Without, Season) %>% 
    mutate(Player.TOI. = first(player.ref$TOI.[match(Player, player.ref$Player)]),
           Player.CF = first(player.ref$CF[match(Player, player.ref$Player)]),
           Player.CA = first(player.ref$CA[match(Player, player.ref$Player)]),
           Player.xGF = first(player.ref$xGF[match(Player, player.ref$Player)]),
           Player.xGA = first(player.ref$xGA[match(Player, player.ref$Player)]),
           Combo.CF = 0,
           Combo.CA = 0,
           Combo.xGF = 0,
           Combo.xGA = 0,
           Without.CF = Player.CF - Combo.CF,
           Without.CA = Player.CA - Combo.CA,
           Without.CF. = Without.CF/(Without.CF + Without.CA),
           Without.xGF = Player.xGF - Combo.xGF,
           Without.xGA = Player.xGA - Combo.xGA,
           Without.xGF. = Without.xGF/(Without.xGF + Without.xGA))
) %>% data.frame()

# Calculate TOI-weighted average strength of units / Calculer la force moyenne des groupes
pbp.qual <- select(pbp, c(Season, Date, Game.ID, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Score.Cat, Strength.State, Season.Type, Event.Length)) %>% data.frame()

pbp.qual$h1.num[which(pbp.qual$h1.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$h2.num[which(pbp.qual$h2.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
pbp.qual$h3.num[which(pbp.qual$h3.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$h4.num[which(pbp.qual$h4.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
pbp.qual$h5.num[which(pbp.qual$h5.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$h6.num[which(pbp.qual$h6.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
pbp.qual$a1.num[which(pbp.qual$a1.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$a2.num[which(pbp.qual$a2.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
pbp.qual$a3.num[which(pbp.qual$a3.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$a4.num[which(pbp.qual$a4.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
pbp.qual$a5.num[which(pbp.qual$a5.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$a6.num[which(pbp.qual$a6.num %in% unique(player$Player) == FALSE)] <- "GOALIE"

group1 <- rbind_list(
  group_by(pbp.qual, h1.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = h1.num, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
    summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, h2.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = h2.num, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
    summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, h3.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = h3.num, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
    summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, h4.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = h4.num, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
    summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, h5.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = h5.num, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
    summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, h6.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = h6.num, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
    summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, a1.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = a1.num, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
    summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, a2.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = a2.num, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
    summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, a3.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = a3.num, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
    summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, a4.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = a4.num, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
    summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, a5.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = a5.num, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
    summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
  group_by(pbp.qual, a6.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
    rename(Player = a6.num, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
    summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2))
) %>% data.frame()

group2 <- rbind_list(
  group_by(group1, Player, OP1, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP1) %>%
    summarise(TOI = sum(TOI)),
  group_by(group1, Player, OP2, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP2) %>%
    summarise(TOI = sum(TOI)),
  group_by(group1, Player, OP3, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP3) %>%
    summarise(TOI = sum(TOI)),
  group_by(group1, Player, OP4, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP4) %>%
    summarise(TOI = sum(TOI)),
  group_by(group1, Player, OP5, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP5) %>%
    summarise(TOI = sum(TOI)),
  group_by(group1, Player, OP6, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP6) %>%
    summarise(TOI = sum(TOI))
) %>% filter(Player != "GOALIE" & OP != "GOALIE") %>% data.frame()

QoC <- merge(group2, select(player.without, c(Without, Player, Player.TOI., Without.CF., Without.xGF.)), by.x = c("Player", "OP"), by.y = c("Without", "Player")) %>%
  mutate(W.TOI. = TOI*Player.TOI., W.CF. = TOI*Without.CF., W.xGF. = TOI*Without.xGF.) %>%
  group_by(Player, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>%
  summarise(S.TOI = sum(TOI), S.TOI. = sum(W.TOI.), S.CF. = sum(W.CF.), S.xGF. = sum(W.xGF.)) %>%
  data.frame()

QoT <- rbind_list(
  filter(combo, P3 == "X") %>% 
    group_by(P1, P2, Combo.Code, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(Player = P1, TM = P2) %>%
    summarise(TOI = sum(TOI)) %>% data.frame(),
  filter(combo, P3 == "X") %>% 
    group_by(P1, P2, Combo.Code, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(Player = P2, TM = P1) %>%
    summarise(TOI = sum(TOI)) %>% data.frame()
) %>% data.frame() %>%
  merge(select(player.without, c(Without, Player, Player.TOI., Without.CF., Without.xGF.)), by.x = c("Player", "TM"), by.y = c("Without", "Player")) %>%
  mutate(W.TOI. = TOI*Player.TOI., W.CF. = TOI*Without.CF., W.xGF. = TOI*Without.xGF.) %>%
  group_by(Player, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>%
  summarise(S.TOI = sum(TOI), S.TOI. = sum(W.TOI.), S.CF. = sum(W.CF.), S.xGF. = sum(W.xGF.)) %>%
  data.frame()

sumplayer <- merge(sumplayer, 
                   group_by(QoT, Player, Season, Venue, Strength.State, Score.Cat, Season.Type) %>% 
                     summarise(S.TOIT = sum(na.omit(S.TOI)), S.TOI.T = sum(na.omit(S.TOI.)), S.CF.T = sum(na.omit(S.CF.)), S.xGF.T = sum(na.omit(S.xGF.))) %>%
                     select(c(Player, Season, Venue, Strength.State, Score.Cat, Season.Type, S.TOIT, S.TOI.T, S.CF.T, S.xGF.T)) %>% data.frame(),
                   by.x = c("Player", "Season", "Venue", "Strength.State", "Score.Cat", "Season.Type"), by.y = c("Player", "Season", "Venue", "Strength.State", "Score.Cat", "Season.Type")) %>%
  data.frame() %>% merge(
    group_by(QoC, Player, Season, Venue, Strength.State, Score.Cat, Season.Type) %>% 
      summarise(S.TOIC = sum(na.omit(S.TOI)), S.TOI.C = sum(na.omit(S.TOI.)), S.CF.C = sum(na.omit(S.CF.)), S.xGF.C = sum(na.omit(S.xGF.))) %>%
      select(c(Player, Season, Venue, Strength.State, Score.Cat, Season.Type, S.TOIC, S.TOI.C, S.CF.C, S.xGF.C)) %>% data.frame(),
    by.x = c("Player", "Season", "Venue", "Strength.State", "Score.Cat", "Season.Type"), by.y = c("Player", "Season", "Venue", "Strength.State", "Score.Cat", "Season.Type")) %>%
  data.frame()

# SPLIT F AND D
# USE SCORE OR SCORE+ZONE ADJUSTED MEASURES?
# REGRESS TOWARDS TEAM-MEAN?

# Combine with history / Combiner avec historique
sumteam <- rbind_list(teamhist, sumteam) %>% data.frame()
sumgoalie <- rbind_list(goaliehist, sumgoalie) %>% data.frame()
sumplayer <- rbind_list(playerhist, sumplayer) %>% data.frame()
sumpair <- rbind_list(pairhist, sumpair) %>% data.frame()
sumline <- rbind_list(linehist, sumline) %>% data.frame()

end <- Sys.time()
print(end - start)

################################################################################################################################################################################################################
################################################################################################################################################################################################################
################################################################################################################################################################################################################

## Write to shiny-server repository / Ajouter au dépot shiny-server
# Link to database / Connecter a la base de données
link <- "/srv/shiny-server/fenwicka.sqlite"
newcon <- dbConnect(SQLite(), link)

# PBP / Résumé
save(pbp, file = paste("~/Documents/github/shiny-server/data/pbp", season, ".Rda", sep = ""))

# Roster / Formation
save(roster, file = "~/Documents/github/shiny-server/data/roster.Rda")

# Team / Équipes
# save(sumteam, file = "~/Documents/github/shiny-server/corsicateam/teamload.Rda")
dbWriteTable(newcon, "teamseason", sumteam, overwrite = TRUE)

# Goalie / Gardiens
# save(sumgoalie, file = "~/Documents/github/shiny-server/corsicagoalie/goalieload.Rda")
dbWriteTable(newcon, "goalieseason", sumgoalie, overwrite = TRUE)

# Player / Joueurs
# save(sumplayer, file = "~/Documents/github/shiny-server/corsicaplayer/playerload.Rda")
dbWriteTable(newcon, "playerseason", sumplayer, overwrite = TRUE)

# Combo / Combinaisons
# save(sumpair, file = "~/Documents/github/shiny-server/corsicacombo/pairload.Rda")
dbWriteTable(newcon, "pairseason", sumpair, overwrite = TRUE)

# save(sumline, file = "~/Documents/github/shiny-server/corsicacombo/lineload.Rda")
dbWriteTable(newcon, "lineseason", sumline, overwrite = TRUE)

################################################################################################################################################################################################################
################################################################################################################################################################################################################
################################################################################################################################################################################################################
