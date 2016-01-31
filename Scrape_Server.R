# Scrape
# Last edited 1/31/2016 / Mise-a-jour 1-31-2016
# Manny

## SCRAPE GAMES / ACQUÉRIR MATCHS

# Load libraries
library(rvest)
library(dplyr)
library(RCurl)
library(RSQLite)

# Load functions / Charger les fonctions
load("~/Documents/github/dryscrape/scrape.RData")

# Get game IDs / Chercher codes des matchs
todays.games("20152016", "1/27/2016")

########################################################################################################################################################################################################

start <- Sys.time()

# Compile games / Compiler matchs
scrape(season = "20152016", start = 20001, end = 20004, names = TRUE) # POSTPONED: 20704, 20716, 20720

# FIX POINTS (BENCH ASSISTS) √
# FIX STRENGTH.STATE DEFINITION (EMPTY NET) √
# FIX REBOUND/RUSH DEFINITION
# CHRONOLOGY STILL IFFY

########################################################################################################################################################################################################
########################################################################################################################################################################################################

## CREATE TABLES / CRÉER TABLES

# Load regressions and coefficients / Charger régressions et coefficients 
load("~/Documents/xG_Regressions.RData")
load("~/Documents/coeffs.RData")

# Enhanced PBP / Résumé de match amélioré
pbp.full$Home.Zone <- pbp.full$ev.zone
pbp.full$Home.Zone[which(as.character(pbp.full$ev.team) == as.character(pbp.full$Away.Team) & as.character(pbp.full$ev.zone) == "Off")] <- "Def"
pbp.full$Home.Zone[which(as.character(pbp.full$ev.team) == as.character(pbp.full$Away.Team) & as.character(pbp.full$ev.zone) == "Def")] <- "Off"

pbp.full <- mutate(pbp.full, Newcode = paste(Season, Game.ID, sep = "."), Round.Seconds = round(as.numeric(as.character(Seconds)), 0)) %>% group_by(Game.ID, Round.Seconds) %>% 
  mutate(FOS = sum(Event %in% c("FAC")), ZF = first(as.character(Home.Zone[which(as.character(Event) %in% c("FAC"))]))) %>% data.frame() %>% 
  mutate(Distance = sqrt((89 - abs(as.numeric(as.character(XC))))^2 + as.numeric(as.character(YC))^2), 
         Angle = abs(atan(as.numeric(as.character(YC))/(89 - abs(as.numeric(as.character(XC)))))*(180/pi)),
         is.NZ = 1*(abs(as.numeric(as.character(XC))) <= 22), 
         is.PP = 1*({as.character(ev.team) == as.character(Home.Team) & as.character(Strength.State) %in% c("5v4", "5v3", "4v3")} | {as.character(ev.team) == as.character(Away.Team) & as.character(Strength.State) %in% c("4v5", "3v5", "3v4")}),
         ref = cumsum(Event %in% c("FAC"))) %>% group_by(ref) %>%
  mutate(Since = as.numeric(as.character(Round.Seconds)) - min(as.numeric(as.character(Round.Seconds))), Zone.Start = first(Home.Zone), Since.Cat = 1*(Since > 25), Score.Cat = as.numeric(as.character(Score.Cat)),
         Category1 = (as.numeric(as.character(Score.Cat)) + 160)*(1*(as.character(ev.team) == as.character(Home.Team)) + 13*(as.character(ev.team) == as.character(Away.Team)))*(as.numeric(as.character(Since.Cat))*3 + 1)*(1*(Zone.Start == "Off") + 2*(Zone.Start == "Neu") + 3*(Zone.Start == "Def")),
         Category2 = as.numeric((1*(as.character(ev.team) == as.character(Away.Team)) + 20*(as.character(ev.team) == as.character(Home.Team)))*(Score.Cat + 4))) %>%
  data.frame()

# Fix empty-net states / Réparer états de filet désert 
pbp.full$Strength.State[which(is.na(pbp.full$Home.Goalie) == TRUE | is.na(pbp.full$Away.Goalie) == TRUE)] <- "EvE"
pbp.full$Strength.State[which(as.numeric(as.character(pbp.full$Period)) > 4 & as.numeric(as.character(pbp.full$Game.ID)) < 30000)] <- "0v0"

# Prevent NA event team / Prévenir équipe du jeu NA
pbp.full$ev.team[which(is.na(as.character(pbp.full$ev.team)) == TRUE)] <- "UKN"

# DUMMY CATEGORY FOR NON-5V5 OR NA MATCHING TO 1 / CATÉGORIE FAUSSE POUR NA OU ÉTATS NON-5V5 POUR ASSOCIER A 1
pbp.full$Category1[which(as.character(pbp.full$Strength.State) != "5v5" | pbp.full$Category1 == 0)] <- NA
pbp.full$Category2[which(as.character(pbp.full$Strength.State) != "5v5" | pbp.full$Category2 == 0)] <- NA

pbp.full <- mutate(pbp.full, cweight1 = coeffs1$corsi[match(Category1, coeffs1$category)], fweight1 = coeffs1$fenwick[match(Category1, coeffs1$category)],
                   sweight1 = coeffs1$shot[match(Category1, coeffs1$category)], gweight1 = coeffs1$goal[match(Category1, coeffs1$category)],
                   cweight2 = coeffs2$corsi[match(Category2, coeffs2$category)], fweight2 = coeffs2$fenwick[match(Category2, coeffs2$category)],
                   sweight2 = coeffs2$shot[match(Category2, coeffs2$category)], gweight2 = coeffs2$goal[match(Category2, coeffs2$category)]) %>% data.frame()

pbp.full$a1.num[which(as.numeric(as.character(pbp.full$Period)) >= 5 & as.character(pbp.full$ev.team) == as.character(pbp.full$Away.Team))] <- pbp.full$p1[which(as.numeric(as.character(pbp.full$Period)) >= 5 & as.character(pbp.full$ev.team) == as.character(pbp.full$Away.Team))]
pbp.full$h1.num[which(as.numeric(as.character(pbp.full$Period)) >= 5 & as.character(pbp.full$ev.team) == as.character(pbp.full$Home.Team))] <- pbp.full$p1[which(as.numeric(as.character(pbp.full$Period)) >= 5 & as.character(pbp.full$ev.team) == as.character(pbp.full$Home.Team))]

pbp.full$is.Rebound <- c(0, 1*(pbp.full$Event[-1] %in% c("GOAL", "SHOT", "MISS", "BLOCK") & pbp.full$Event[-nrow(pbp.full)] %in% c("GOAL", "SHOT", "MISS", "BLOCK") & pbp.full$Period[-nrow(pbp.full)] == pbp.full$Period[-1] &
                                 (as.numeric(as.character(pbp.full$Seconds[-1])) - as.numeric(as.character(pbp.full$Seconds[-nrow(pbp.full)]))) <= 2))
pbp.full$is.Rush <- c(0, 1*(pbp.full$Event[-1] %in% c("GOAL", "SHOT", "MISS", "BLOCK") & pbp.full$Period[-nrow(pbp.full)] == pbp.full$Period[-1] &
                              (as.numeric(as.character(pbp.full$Seconds[-1])) - as.numeric(as.character(pbp.full$Seconds[-nrow(pbp.full)]))) <= 4 & 
                              {pbp.full$Event[-nrow(pbp.full)] %in% c("GIVE", "TAKE") | {{pbp.full$ev.team[-nrow(pbp.full)] == pbp.full$ev.team[-1] & pbp.full$ev.zone[-nrow(pbp.full)] == "Def"} |
                              {pbp.full$ev.team[-nrow(pbp.full)] != pbp.full$ev.team[-1] & pbp.full$ev.zone[-nrow(pbp.full)] == "Off"}}}))

pbp.full$is.Rush[which(is.na(pbp.full$is.Rush) == T)] <- 0
pbp.full$is.Rebound[which(is.na(pbp.full$is.Rebound) == T)] <- 0

pbp.full <- rbind_list(filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & is.NZ == 1) %>% mutate(xG = 0.00648),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & is.na(is.NZ) == T) %>% mutate(xG = 0.06639),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Wrist" & is.NZ == 0 & is.Rebound == 0) %>% xG1(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Backhand" & is.NZ == 0 & is.Rebound == 0) %>% xG2(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Slap" & is.NZ == 0 & is.Rebound == 0) %>% xG3(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) %in% c("Tip-In", "Deflected") & is.NZ == 0 & is.Rebound == 0) %>% xG4(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Snap" & is.NZ == 0 & is.Rebound == 0) %>% xG5(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Wrap-around" & is.NZ == 0 & is.Rebound == 0) %>% xG6(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Wrist" & is.NZ == 0 & is.Rebound == 1) %>% xG7(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Backhand" & is.NZ == 0 & is.Rebound == 1) %>% xG8(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Slap" & is.NZ == 0 & is.Rebound == 1) %>% xG9(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) %in% c("Tip-In", "Deflected") & is.NZ == 0 & is.Rebound == 1) %>% xG10(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Snap" & is.NZ == 0 & is.Rebound == 1) %>% xG11(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Wrap-around" & is.NZ == 0 & is.Rebound == 1) %>% xG12(),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) %in% c("Wrist", "Backhand", "Slap", "Tip-In", "Deflected", "Snap", "Wrap-around") == F),
                       filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") == F)) %>% data.frame() %>% 
  group_by(Game.ID) %>% arrange(Seconds) %>% data.frame()

# Replace xG for shots missing coordinates / Remplacer xG pour tirs sans coordinées
pbp.full$xG[which(is.na(pbp.full$xG) == TRUE & as.character(pbp.full$Event) %in% c("GOAL", "SHOT", "MISS"))] <- 0.06639

# Append unique game codes to roster table / Attacher codes de match uniques a la table de formation
roster.full$Newcode <- paste(roster.full$Season, roster.full$Game.ID, sep = ".")

# Functions / Fonctions
code <- function(a, b, c) {
  sorted <- sort(c(first(a), first(b), first(c)), decreasing = FALSE)
  p1 <- sorted[1]
  p2 <- sorted[2]
  p3 <- sorted[3]
  return(paste(p1, p2, p3, sep = "-"))
}

sum1p.home <- function(x) {
  summarise(x,
            Venue = "Home", Team = first(Home.Team), TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
            CF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Home.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Home.Team),
            PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            iPENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
            iPEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
            iCF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK") & as.character(p1) == as.character(Player)),
            iFF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player)),
            iSF = sum(as.character(Event) %in% c("SHOT", "GOAL") & as.character(p1) == as.character(Player)),
            ixG = sum(na.omit(xG*(as.character(p1) == as.character(Player)))),
            iFOW = sum(as.character(Event) %in% c("FAC") & as.character(p2) == as.character(Player) & as.character(ev.team) == Home.Team),
            iFOL = sum(as.character(Event) %in% c("FAC") & as.character(p2) == as.character(Player) & as.character(ev.team) == Away.Team),
            iHF = sum(as.character(Event) %in% c("HIT") & as.character(p1) == as.character(Player)),
            iHA = sum(as.character(Event) %in% c("HIT") & as.character(p2) == as.character(Player)),
            iGVA = sum(as.character(Event) %in% c("GIVE") & as.character(p1) == as.character(Player)), iTKA = sum(as.character(Event) %in% c("TAKE") & as.character(p1) == as.character(Player)),
            iBLK = sum(as.character(Event) %in% c("BLOCK") & as.character(p2) == as.character(Player)),
            OTF = sum(as.character(Event) %in% c("ON") & as.character(p1) == as.character(Player) & FOS < 1),
            DIST = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player)))),
            RBF = sum(as.character(ev.team) == Home.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Home.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1),
            iRB = sum(is.Rebound == 1 & as.character(p1) == as.character(Player)),
            iRS = sum(is.Rush == 1 & as.character(p1) == as.character(Player))) %>% data.frame()
}

sum1p.away <- function(x) {
  summarise(x,
            Venue = "Away", Team = first(Away.Team), TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
            CF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Away.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Away.Team),
            PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            iPENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
            iPEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
            iCF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK") & as.character(p1) == as.character(Player)),
            iFF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player)),
            iSF = sum(as.character(Event) %in% c("SHOT", "GOAL") & as.character(p1) == as.character(Player)),
            ixG = sum(na.omit(xG*(as.character(p1) == as.character(Player)))),
            iFOW = sum(as.character(Event) %in% c("FAC") & as.character(p1) == as.character(Player) & as.character(ev.team) == Away.Team),
            iFOL = sum(as.character(Event) %in% c("FAC") & as.character(p1) == as.character(Player) & as.character(ev.team) == Home.Team),
            iHF = sum(as.character(Event) %in% c("HIT") & as.character(p1) == as.character(Player)),
            iHA = sum(as.character(Event) %in% c("HIT") & as.character(p2) == as.character(Player)),
            iGVA = sum(as.character(Event) %in% c("GIVE") & as.character(p1) == as.character(Player)), iTKA = sum(as.character(Event) %in% c("TAKE") & as.character(p1) == as.character(Player)),
            iBLK = sum(as.character(Event) %in% c("BLOCK") & as.character(p2) == as.character(Player)),
            OTF = sum(as.character(Event) %in% c("ON") & as.character(p1) == as.character(Player) & FOS < 1),
            DIST = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player)))),
            RBF = sum(as.character(ev.team) == Away.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Away.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1),
            iRB = sum(is.Rebound == 1 & as.character(p1) == as.character(Player)),
            iRS = sum(is.Rush == 1 & as.character(p1) == as.character(Player))) %>% data.frame()
}

sum2p.home <- function(x) {
  summarise(x, P3 = "X",
            Venue = "Home", Team = first(Home.Team), TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
            CF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Home.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Home.Team),
            PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            RBF = sum(as.character(ev.team) == Home.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Home.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1),
            P1.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P1)), P1.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1)),
            P1.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1)), P2.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P2)),
            P2.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2)), P2.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2)),
            P3.G = NA, P3.A1 = NA, P3.A2 = NA,
            P1.A1.P2 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1) & as.character(p1) == as.character(P2)), 
            P1.A2.P2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1) & as.character(p1) == as.character(P2)),
            P2.A1.P1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2) & as.character(p1) == as.character(P1)), 
            P2.A2.P1 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2) & as.character(p1) == as.character(P1)),
            P1.A1.P3 = NA, P1.A2.P3 = NA, P2.A1.P3 = NA, P2.A2.P3 = NA,
            P3.A1.P1 = NA, P3.A2.P1 = NA, P3.A1.P2 = NA, P3.A2.P2 = NA) %>% data.frame()
}

sum2p.away <- function(x) {
  summarise(x, P3 = "X",
            Venue = "Away", Team = first(Away.Team), TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
            CF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Away.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Away.Team),
            PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            RBF = sum(as.character(ev.team) == Away.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Away.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1),
            P1.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P1)), P1.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1)),
            P1.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1)), P2.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P2)),
            P2.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2)), P2.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2)),
            P3.G = NA, P3.A1 = NA, P3.A2 = NA,
            P1.A1.P2 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1) & as.character(p1) == as.character(P2)), 
            P1.A2.P2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1) & as.character(p1) == as.character(P2)),
            P2.A1.P1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2) & as.character(p1) == as.character(P1)), 
            P2.A2.P1 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2) & as.character(p1) == as.character(P1)),
            P1.A1.P3 = NA, P1.A2.P3 = NA, P2.A1.P3 = NA, P2.A2.P3 = NA,
            P3.A1.P1 = NA, P3.A2.P1 = NA, P3.A1.P2 = NA, P3.A2.P2 = NA) %>% data.frame()
}

sum3p.home <- function(x) {
  summarise(x,
            Venue = "Home", Team = first(Home.Team), TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
            CF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Home.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Home.Team),
            PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            RBF = sum(as.character(ev.team) == Home.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Home.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1),
            P1.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P1)), P1.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1)),
            P1.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1)), P2.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P2)),
            P2.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2)), P2.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2)),
            P3.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P3)), P3.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P3)),
            P3.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P3))) %>% data.frame()
}

sum3p.away <- function(x) {
  summarise(x,
            Venue = "Away", Team = first(Away.Team), TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
            CF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Away.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Away.Team),
            PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            RBF = sum(as.character(ev.team) == Away.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Away.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1),
            P1.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P1)), P1.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1)),
            P1.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1)), P2.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P2)),
            P2.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2)), P2.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2)),
            P3.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P3)), P3.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P3)),
            P3.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P3))) %>% data.frame()
}

# Create team table / Créer table d'équipes
team.bygame <- rbind_list(group_by(pbp.full, Season, Date, Game.ID, Home.Team, Score.Cat, Strength.State, Season.Type) %>% rename(Team = Home.Team) %>%
                            summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
                                      CF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
                                      FF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
                                      SF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
                                      GF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
                                      xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
                                      ACF = sum(cweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                      AFF = sum(fweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                      ASF = sum(sweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                      AGF = sum(gweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
                                      AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == Team))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
                                      MCF = sum(cweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                      MFF = sum(fweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                      MSF = sum(sweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                      MGF = sum(gweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
                                      MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == Team))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
                                      OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
                                      DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
                                      NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
                                      OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
                                      DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
                                      NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
                                      FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
                                      HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
                                      GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Team),
                                      PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Team & grepl("10 min", as.character(Detail)) == F),
                                      PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
                                      DISTF = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Team))),
                                      DISTA = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Away.Team))),
                                      RBF = sum(as.character(ev.team) == Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
                                      RSF = sum(as.character(ev.team) == Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1)),
                          group_by(pbp.full, Season, Date, Game.ID, Away.Team, Score.Cat, Strength.State, Season.Type) %>% rename(Team = Away.Team) %>%
                            summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
                                      CF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
                                      FF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
                                      SF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
                                      GF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
                                      xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
                                      ACF = sum(cweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                      AFF = sum(fweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                      ASF = sum(sweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                      AGF = sum(gweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
                                      AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == Team))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
                                      MCF = sum(cweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                      MFF = sum(fweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                      MSF = sum(sweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                      MGF = sum(gweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
                                      MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == Team))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
                                      OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
                                      DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
                                      NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
                                      OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
                                      DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
                                      NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
                                      FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
                                      HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
                                      GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Team),
                                      PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Team & grepl("10 min", as.character(Detail)) == F),
                                      PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
                                      DISTF = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Team))),
                                      DISTA = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Home.Team))),
                                      RBF = sum(as.character(ev.team) == Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
                                      RSF = sum(as.character(ev.team) == Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1))) %>%
  mutate(Newcode = paste(Season, Game.ID, sep = ".")) %>% data.frame()
# ADD OFFSIDES/ICINGS?

# Create goalie table / Créer table de gardiens
goalie.bygame <- rbind_list(group_by(pbp.full, Season, Date, Game.ID, Home.Goalie, Score.Cat, Strength.State, Season.Type) %>% rename(Player = Home.Goalie) %>%
                              summarise(Venue = "Home", Team = first(Home.Team), TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
                                        CF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
                                        FF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
                                        SF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
                                        GF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
                                        xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
                                        ACF = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                        AFF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                        ASF = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                        AGF = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
                                        AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
                                        MCF = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                        MFF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                        MSF = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                        MGF = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
                                        MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
                                        OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
                                        DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
                                        NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
                                        OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
                                        DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
                                        NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
                                        FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
                                        HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
                                        GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Home.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Home.Team),
                                        PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
                                        PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
                                        G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(Player)),
                                        A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(Player)),
                                        A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(Player)),
                                        iPENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
                                        iPEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
                                        DISTA = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Away.Team))),
                                        RBF = sum(as.character(ev.team) == Home.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
                                        RSF = sum(as.character(ev.team) == Home.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1)),
                            group_by(pbp.full, Season, Date, Game.ID, Away.Goalie, Score.Cat, Strength.State, Season.Type) %>% rename(Player = Away.Goalie) %>%
                              summarise(Venue = "Away", Team = first(Away.Team), TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2),
                                        CF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
                                        FF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
                                        SF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
                                        GF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
                                        xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
                                        ACF = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                        AFF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                        ASF = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                        AGF = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
                                        AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
                                        MCF = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                        MFF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                        MSF = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                        MGF = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
                                        MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
                                        OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
                                        DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
                                        NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
                                        OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
                                        DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
                                        NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
                                        FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
                                        HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
                                        GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Away.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Away.Team),
                                        PENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
                                        PEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
                                        G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(Player)),
                                        A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(Player)),
                                        A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(Player)),
                                        iPENT = 2*sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
                                        iPEND = 2*sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
                                        DISTA = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Away.Team))),
                                        RBF = sum(as.character(ev.team) == Away.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
                                        RSF = sum(as.character(ev.team) == Away.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1))) %>%
  mutate(Newcode = paste(Season, Game.ID, sep = ".")) %>% data.frame() %>% filter(!is.na(Player)) %>% data.frame()

# Create player table / Créer table de joueurs
player.points <- rbind_list(group_by(pbp.full, Season, Date, Game.ID, p1, Score.Cat, Strength.State, Season.Type) %>% rename(Player = p1) %>%
                              summarise(G = sum(Event %in% c("GOAL")), A1 = 0, A2 = 0),
                            group_by(pbp.full, Season, Date, Game.ID, p2, Score.Cat, Strength.State, Season.Type) %>% rename(Player = p2) %>%
                              summarise(G = 0, A1 = sum(Event %in% c("GOAL")), A2 = 0),
                            group_by(pbp.full, Season, Date, Game.ID, p3, Score.Cat, Strength.State, Season.Type) %>% rename(Player = p3) %>%
                              summarise(G = 0, A1 = 0, A2 = sum(Event %in% c("GOAL")))
) %>%
  data.frame() %>% filter(!is.na(Player)) %>% data.frame() %>%
  group_by(Season, Date, Game.ID, Player, Score.Cat, Strength.State, Season.Type) %>%
  summarise(G = sum(G), A1 = sum(A1), A2 = sum(A2)) %>% data.frame()

player.bygame <- rbind_list(group_by(pbp.full, Season, Date, Game.ID, h1.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h1.num) %>%
                              sum1p.home(),
                            group_by(pbp.full, Season, Date, Game.ID, h2.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h2.num) %>%
                              sum1p.home(),
                            group_by(pbp.full, Season, Date, Game.ID, h3.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h3.num) %>%
                              sum1p.home(),
                            group_by(pbp.full, Season, Date, Game.ID, h4.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h4.num) %>%
                              sum1p.home(),
                            group_by(pbp.full, Season, Date, Game.ID, h5.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h5.num) %>%
                              sum1p.home(),
                            group_by(pbp.full, Season, Date, Game.ID, h6.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h6.num) %>%
                              sum1p.home(),
                            group_by(pbp.full, Season, Date, Game.ID, a1.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a1.num) %>%
                              sum1p.away(),
                            group_by(pbp.full, Season, Date, Game.ID, a2.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a2.num) %>%
                              sum1p.away(),
                            group_by(pbp.full, Season, Date, Game.ID, a3.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a3.num) %>%
                              sum1p.away(),
                            group_by(pbp.full, Season, Date, Game.ID, a4.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a4.num) %>%
                              sum1p.away(),
                            group_by(pbp.full, Season, Date, Game.ID, a5.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a5.num) %>%
                              sum1p.away(),
                            group_by(pbp.full, Season, Date, Game.ID, a6.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a6.num) %>%
                              sum1p.away()) %>%
  data.frame() %>% filter(!is.na(Player)) %>% data.frame() %>%
  group_by(Season, Date, Game.ID, Player, Score.Cat, Strength.State, Season.Type) %>%
  summarise(Position = first(roster.full$Position[match(Player, roster.full$Full.Name)]), Venue = first(Venue), Team = first(Team), TOI = sum(TOI),
            CF = sum(CF), CA = sum(CA), iCF = sum(iCF), 
            FF = sum(FF), FA = sum(FA), iFF = sum(iFF),
            SF = sum(SF), SA = sum(SA), iSF = sum(iSF),
            GF = sum(GF), GA = sum(GA), 
            xGF = sum(xGF), xGA = sum(xGA), ixG = sum(ixG),
            ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA),
            ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA),
            AxGF = sum(AxGF), AxGA = sum(AxGA),
            MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA),
            MxGF = sum(MxGF), MxGA = sum(MxGA),
            OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
            OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
            FOW = sum(FOW), FOL = sum(FOL), iFOW = sum(iFOW), iFOL = sum(iFOL),
            HF = sum(HF), HA = sum(HA), iHF = sum(iHF), iHA = sum(iHA),
            GVA = sum(GVA), TKA = sum(TKA), iGVA = sum(iGVA), iTKA = sum(iTKA),
            iBLK = sum(iBLK),
            PENT = sum(PENT), PEND = sum(PEND), iPENT = sum(iPENT), iPEND = sum(iPEND),
            iDIST = sum(DIST),
            RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
            iRB = sum(iRB), iRS = sum(iRS)) %>% data.frame() %>%
  merge(player.points, by.x = c("Season", "Game.ID", "Strength.State", "Score.Cat", "Player", "Date", "Season.Type"), by.y = c("Season", "Game.ID", "Strength.State", "Score.Cat", "Player", "Date", "Season.Type")) %>%
  data.frame() %>%
  merge(team.bygame %>% select(c(Season, Game.ID, Venue, Strength.State, Score.Cat, TOI, CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA, MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA, OZS, DZS, NZS)) %>% 
          rename(tTOI = TOI, tCF = CF, tCA = CA, tFF = FF, tFA = FA, tSF = SF, tSA = SA, tGF = GF, tGA = GA, txGF = xGF, txGA = xGA, 
                 tACF = ACF, tACA = ACA, tAFF = AFF, tAFA = AFA, tASF = ASF, tASA = ASA, tAGF = AGF, tAGA = AGA, tAxGF = AxGF, tAxGA = AxGA, 
                 tMCF = MCF, tMCA = MCA, tMFF = MFF, tMFA = MFA, tMSF = MSF, tMSA = MSA, tMGF = MGF, tMGA = MGA, tMxGF = MxGF, tMxGA = MxGA,
                 tOZS = OZS, tDZS = DZS, tNZS = NZS) %>% data.frame(),
        by.x = c("Season", "Game.ID", "Venue", "Strength.State", "Score.Cat"), by.y = c("Season", "Game.ID", "Venue", "Strength.State", "Score.Cat")) %>% filter(Position != "G") %>% 
  mutate(Newcode = paste(Season, Game.ID, sep = ".")) %>% data.frame()
# TO ADD: REBOUNDS, RUSHES, ON-ICE OFFSIDE/ICING?
# EXCLUDE ICINGS FROM ZONE STARTS/FINISHES?

## Create combo table / Créer table de combinaisons
# Two-player combos / Combinaisons de deux joueurs
grouped2 <- rbind_list(
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num) %>% rename(P1 = a1.num, P2 = a2.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a3.num) %>% rename(P1 = a1.num, P2 = a3.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a4.num) %>% rename(P1 = a1.num, P2 = a4.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a5.num) %>% rename(P1 = a1.num, P2 = a5.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a3.num) %>% rename(P1 = a2.num, P2 = a3.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a4.num) %>% rename(P1 = a2.num, P2 = a4.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a5.num) %>% rename(P1 = a2.num, P2 = a5.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a4.num) %>% rename(P1 = a3.num, P2 = a4.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a5.num) %>% rename(P1 = a3.num, P2 = a5.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a4.num, a5.num) %>% rename(P1 = a4.num, P2 = a5.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a6.num) %>% rename(P1 = a1.num, P2 = a6.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a6.num) %>% rename(P1 = a2.num, P2 = a6.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a6.num) %>% rename(P1 = a3.num, P2 = a6.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a4.num, a6.num) %>% rename(P1 = a4.num, P2 = a6.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a5.num, a6.num) %>% rename(P1 = a5.num, P2 = a6.num) %>% 
    sum2p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num) %>% rename(P1 = h1.num, P2 = h2.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h3.num) %>% rename(P1 = h1.num, P2 = h3.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h4.num) %>% rename(P1 = h1.num, P2 = h4.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h5.num) %>% rename(P1 = h1.num, P2 = h5.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h3.num) %>% rename(P1 = h2.num, P2 = h3.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h4.num) %>% rename(P1 = h2.num, P2 = h4.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h5.num) %>% rename(P1 = h2.num, P2 = h5.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h4.num) %>% rename(P1 = h3.num, P2 = h4.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h5.num) %>% rename(P1 = h3.num, P2 = h5.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h4.num, h5.num) %>% rename(P1 = h4.num, P2 = h5.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h6.num) %>% rename(P1 = h1.num, P2 = h6.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h6.num) %>% rename(P1 = h2.num, P2 = h6.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h6.num) %>% rename(P1 = h3.num, P2 = h6.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h4.num, h6.num) %>% rename(P1 = h4.num, P2 = h6.num) %>% 
    sum2p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h5.num, h6.num) %>% rename(P1 = h5.num, P2 = h6.num) %>% 
    sum2p.home()
) %>% group_by(Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, P1, P2, P3) %>% mutate(Combo.Code = code(P1, P2, P3), Newcode = paste(Season, Game.ID, sep = ".")) %>% filter(!is.na(P1) & !is.na(P2)) %>% data.frame()

# Three-player combos / Combinaisons de trois joueurs
grouped3 <- rbind_list(
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num, a3.num) %>% rename(P1 = a1.num, P2 = a2.num, P3 = a3.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a3.num, a4.num) %>% rename(P1 = a1.num, P2 = a3.num, P3 = a4.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a4.num, a5.num) %>% rename(P1 = a1.num, P2 = a4.num, P3 = a5.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a3.num, a4.num) %>% rename(P1 = a2.num, P2 = a3.num, P3 = a4.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a4.num, a5.num) %>% rename(P1 = a2.num, P2 = a4.num, P3 = a5.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num, a4.num) %>% rename(P1 = a1.num, P2 = a2.num, P3 = a4.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num, a5.num) %>% rename(P1 = a1.num, P2 = a2.num, P3 = a5.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a3.num, a5.num) %>% rename(P1 = a1.num, P2 = a3.num, P3 = a5.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a3.num, a5.num) %>% rename(P1 = a2.num, P2 = a3.num, P3 = a5.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a4.num, a5.num) %>% rename(P1 = a3.num, P2 = a4.num, P3 = a5.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num, a6.num) %>% rename(P1 = a1.num, P2 = a2.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a3.num, a6.num) %>% rename(P1 = a1.num, P2 = a3.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a4.num, a6.num) %>% rename(P1 = a1.num, P2 = a4.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a5.num, a6.num) %>% rename(P1 = a1.num, P2 = a5.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a3.num, a6.num) %>% rename(P1 = a2.num, P2 = a3.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a4.num, a6.num) %>% rename(P1 = a2.num, P2 = a4.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a5.num, a6.num) %>% rename(P1 = a2.num, P2 = a5.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a4.num, a6.num) %>% rename(P1 = a3.num, P2 = a4.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a5.num, a6.num) %>% rename(P1 = a3.num, P2 = a5.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a4.num, a5.num, a6.num) %>% rename(P1 = a4.num, P2 = a5.num, P3 = a6.num) %>% 
    sum3p.away(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num, h3.num) %>% rename(P1 = h1.num, P2 = h2.num, P3 = h3.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h3.num, h4.num) %>% rename(P1 = h1.num, P2 = h3.num, P3 = h4.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h4.num, h5.num) %>% rename(P1 = h1.num, P2 = h4.num, P3 = h5.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h3.num, h4.num) %>% rename(P1 = h2.num, P2 = h3.num, P3 = h4.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h4.num, h5.num) %>% rename(P1 = h2.num, P2 = h4.num, P3 = h5.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num, h4.num) %>% rename(P1 = h1.num, P2 = h2.num, P3 = h4.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num, h5.num) %>% rename(P1 = h1.num, P2 = h2.num, P3 = h5.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h3.num, h5.num) %>% rename(P1 = h1.num, P2 = h3.num, P3 = h5.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h3.num, h5.num) %>% rename(P1 = h2.num, P2 = h3.num, P3 = h5.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h4.num, h5.num) %>% rename(P1 = h3.num, P2 = h4.num, P3 = h5.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num, h6.num) %>% rename(P1 = h1.num, P2 = h2.num, P3 = h6.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h3.num, h6.num) %>% rename(P1 = h1.num, P2 = h3.num, P3 = h6.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h4.num, h6.num) %>% rename(P1 = h1.num, P2 = h4.num, P3 = h6.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h5.num, h6.num) %>% rename(P1 = h1.num, P2 = h5.num, P3 = h6.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h3.num, h6.num) %>% rename(P1 = h2.num, P2 = h3.num, P3 = h6.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h4.num, h6.num) %>% rename(P1 = h2.num, P2 = h4.num, P3 = h6.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h5.num, h6.num) %>% rename(P1 = h2.num, P2 = h5.num, P3 = h6.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h4.num, h6.num) %>% rename(P1 = h3.num, P2 = h4.num, P3 = h6.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h5.num, h6.num) %>% rename(P1 = h3.num, P2 = h5.num, P3 = h6.num) %>% 
    sum3p.home(),
  group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h4.num, h5.num, h6.num) %>% rename(P1 = h4.num, P2 = h5.num, P3 = h6.num) %>% 
    sum3p.home()
) %>% group_by(Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, P1, P2, P3) %>% mutate(Combo.Code = code(P1, P2, P3), Newcode = paste(Season, Game.ID, sep = ".")) %>% filter(!is.na(P1) & !is.na(P2) & !is.na(P3)) %>% data.frame()

# Combine / Combiner
grouped <- rbind_list(grouped2, grouped3)

combos.bygame <- merge(group_by(grouped, Season, Date, Game.ID, Newcode, Combo.Code, Venue, Score.Cat, Strength.State, Season.Type) %>%
                         summarise(Team = first(Team), TOI = sum(TOI),
                                   P1 = first(P1), P1.POS = first(roster.full$Position[match(P1, roster.full$Full.Name)]),
                                   P2 = first(P2), P2.POS = first(roster.full$Position[match(P2, roster.full$Full.Name)]),
                                   P3 = first(P3), P3.POS = first(roster.full$Position[match(P3, roster.full$Full.Name)]),
                                   P1.G = sum(P1.G), P1.A1 = sum(P1.A1), P1.A2 = sum(P1.A2),
                                   P2.G = sum(P2.G), P2.A1 = sum(P2.A1), P2.A2 = sum(P2.A2),
                                   P3.G = sum(P3.G), P3.A1 = sum(P3.A1), P3.A2 = sum(P3.A2),
                                   P1.A1.P2 = sum(P1.A1.P2), P1.A2.P2 = sum(P1.A2.P2),
                                   P2.A1.P1 = sum(P2.A1.P1), P2.A2.P1 = sum(P2.A2.P1),
                                   P1.A1.P3 = sum(P1.A1.P3), P1.A2.P3 = sum(P1.A2.P3),
                                   P2.A1.P3 = sum(P2.A1.P3), P2.A2.P3 = sum(P2.A2.P3),
                                   P3.A1.P1 = sum(P3.A1.P1), P3.A2.P1 = sum(P3.A2.P1),
                                   P3.A1.P2 = sum(P3.A1.P2), P3.A2.P2 = sum(P3.A2.P2),
                                   CF = sum(CF), CA = sum(CA),
                                   FF = sum(FF), FA = sum(FA),
                                   SF = sum(SF), SA = sum(SA),
                                   GF = sum(GF), GA = sum(GA), 
                                   xGF = sum(xGF), xGA = sum(xGA),
                                   ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA),
                                   ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA),
                                   AxGF = sum(AxGF), AxGA = sum(AxGA),
                                   MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                                   MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA),
                                   MxGF = sum(MxGF), MxGA = sum(MxGA),
                                   OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS),
                                   OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
                                   FOW = sum(FOW), FOL = sum(FOL),
                                   HF = sum(HF), HA = sum(HA),
                                   GVA = sum(GVA), TKA = sum(TKA),
                                   PENT = sum(PENT), PEND = sum(PEND),
                                   RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% data.frame(),
                       team.bygame %>% select(c(Season, Game.ID, Venue, Strength.State, Score.Cat, TOI, CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA, MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA, OZS, DZS, NZS)) %>% 
                         rename(tTOI = TOI, tCF = CF, tCA = CA, tFF = FF, tFA = FA, tSF = SF, tSA = SA, tGF = GF, tGA = GA, txGF = xGF, txGA = xGA, 
                                tACF = ACF, tACA = ACA, tAFF = AFF, tAFA = AFA, tASF = ASF, tASA = ASA, tAGF = AGF, tAGA = AGA, tAxGF = AxGF, tAxGA = AxGA, 
                                tMCF = MCF, tMCA = MCA, tMFF = MFF, tMFA = MFA, tMSF = MSF, tMSA = MSA, tMGF = MGF, tMGA = MGA, tMxGF = MxGF, tMxGA = MxGA,
                                tOZS = OZS, tDZS = DZS, tNZS = NZS) %>% data.frame(),
                       by.x = c("Season", "Game.ID", "Venue", "Strength.State", "Score.Cat"), by.y = c("Season", "Game.ID", "Venue", "Strength.State", "Score.Cat")) %>%
  filter(as.numeric(as.character(TOI)) > 0 & {is.na(P1.POS) | as.character(P1.POS) != "G"} & {is.na(P2.POS) | as.character(P2.POS) != "G"} & {is.na(P3.POS) | as.character(P3.POS) != "G"}) %>% data.frame()
# ORDER PLAYERS
# ADD ASSIST NETWORK

# Trim PBP / Réduire résumé
pbp.full <- select(pbp.full, -c(FOS, ZF, is.NZ, is.PP, ref, Since, Zone.Start, Since.Cat, Category1, Category2)) %>% data.frame()

# Scrape bio data / Acquérir données bio
bios(teams = "all")

end <- Sys.time()
print(end - start)

########################################################################################################################################################################################################
########################################################################################################################################################################################################

## WRITE TO DATABASE / AJOUTER A LA BASE DE DONNÉES

# Link to database / Connecter a la base de données
link <- "/srv/shiny-server/corsica.sqlite"
newcon <- dbConnect(SQLite(), link)

# List games already in database / Chercher matchs déja présents dans la base de données
db.games <- unique(sqliteQuickColumn(newcon, "roster", "Newcode"))

# Replace old data / Remplacer anciennes données
bio <- dbReadTable(newcon, "bio")

newbio <- rbind_list(
  filter(bio, Player.Code %in% unique(bio.full$Player.Code) == FALSE) %>% data.frame(),
  bio.full
) %>% data.frame()

# Remove overlapping games / Éliminer matchs extras
pbp <- filter(pbp.full, Newcode %in% db.games == FALSE)
roster <- filter(roster.full, Newcode %in% db.games == FALSE)
team <- filter(team.bygame, Newcode %in% db.games == FALSE)
goalie <- filter(goalie.bygame, Newcode %in% db.games == FALSE)
player <- filter(player.bygame, Newcode %in% db.games == FALSE)
combo <- filter(combos.bygame, Newcode %in% db.games == FALSE)

# Write tables / Ajouter les tables
dbWriteTable(newcon, "pbp", pbp, overwrite = FALSE, append = TRUE)
dbWriteTable(newcon, "roster", roster, overwrite = FALSE, append = TRUE)
dbWriteTable(newcon, "team", team, overwrite = FALSE, append = TRUE)
dbWriteTable(newcon, "goalie", goalie, overwrite = FALSE, append = TRUE)
dbWriteTable(newcon, "player", player, overwrite = FALSE, append = TRUE)
dbWriteTable(newcon, "combo", combo, overwrite = FALSE, append = TRUE)
dbWriteTable(newcon, "bio", newbio, overwrite = TRUE)

########################################################################################################################################################################################################
########################################################################################################################################################################################################
