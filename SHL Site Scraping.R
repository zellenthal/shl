library(tidyverse)
library(esquisse)
library(rvest)
library(Lahman)
library(reshape2)
library(ggthemes)

#this code is more administrative, sets up team names, IDs, lookup tables 
#for future functions

#vector for team IDs from SHL.se, used to create distinct URLs
teamIDs <- c("f7c8-f7c8isEb3", "1ab8-1ab8bfj7N", "2459-2459QTs1f", "087a-087aTQv9u",
             "752c-752c12zB7Z", "3db0-3db09jXTE", "f51e-f51eM6sbL", "9541-95418PpkP",
             "41c4-41c4BiYZU", "1a71-1a71gTHKh", "8e6f-8e6fUXJvi", "110b-110bJcIAI",
             "dcba-dcbaXSrRU", "259b-259bYGVIp", "82eb-82ebmgaJ8", "ee93-ee93uy4oW",
             "50e6-50e6DYeWM", "936a-936aAY8bT", "31d1-31d1NbSlR", "fe02-fe02mf1FN")

#vector for SHL.se team names, matching up ID from above to frienldy team name
shlTeamNames <- c("AIK", "Brynäs", "Djurgården", "Frölunda", "Färjestad", "HV71", "Karlskrona",
                  "Leksand", "Linköping", "Luleå", "Malmö", "MODO", "Mora", "Oskarshamn", "Örebro",
                  "Rögle", "Skellefteå", "Södertälje", "Timrå", "Växjö")

#vector for SweHockey team names, team notation from stats.swehocky.se
sweHockeyTeams <- c("AIK", "Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK",
                    "HV 71", "Karlskrona HK", "Leksands IF", "Linköping HC", "Luleå HF",
                    "IF Malmö Redhawks", "MODO Hockey", "Mora IK", "IK Oskarshamn",
                    "Örebro HK", "Rögle BK", "Skellefteå AIK", "Södertälje SK", "Timrå IK", 
                    "Växjö Lakers HC")

#vector for weird SHL.se names, sometimes team names also appear like this
shlDoubleTeamNames <- c("AIKAIK", "BrynäsBIF", "DjurgårdenDIF", "FrölundaFHC", "FärjestadFBK",
                        "HV71HV71", "KarlskronaKHK", "LeksandLIF", "LinköpingLHC", "LuleåLHF",
                        "MalmöMIF", "MODOMODO", "MoraMIK", "OskarshamnIKO", "ÖrebroÖRE", 
                        "RögleRBK", "SkellefteåSKE", "xxx", "TimråTIK", "VäxjöVLH")

#have to make this one for the team reports
shlDoubleTeamNames2 <- c("AIKAIK", "BrynäsBIF", "DjurgårdenDIF", "FrölundaFHC", "FärjestadFBK",
                        "HV71HV71", "KarlskronaKHK", "LeksandLIF", "LinköpingLHC", "LuleåLHF",
                        "MalmöMIF", "MODOMODO", "MoraMIK", "OskarshamnIKO", "ÖrebroÖRE", 
                        "RögleRBK", "SkellefteåSKE", "xxx", "TimråTIK", "VäxjöVLH")

#my own abbreviations for naming data frames
shlAbbreviations <- c("aik", "bif", "dif", "fhc", "fbk", "hv", "khk", "lif", 
                      "lhc", "lhf", "mif", "modo", "mik","iko", "ohk", "rbk", 
                      "ske", "ssk", "tik", "vax")

#lookup table for SHL.se team names and team IDs
teamIDLookup <- tibble(shlName = shlTeamNames, teamID = teamIDs)

#1819 teams lookup table only (to loop through just those URLs without breaking things)
teamIDLookup %>%
  slice(2, 3, 4, 5, 6, 9, 10, 11, 13, 15, 16, 17, 19, 20) -> teamIDLookup_1819

#same thing as above, but for 15/16 teams
#teamIDLookup %>%
  #slice(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 15, 16, 17, 20) -> teamIDLookup_1516


#lookup table for SHL team names and SweHockey team names
teamNameDictionary <- tibble(shlName = shlTeamNames, sweName = sweHockeyTeams)

#lookup table for the two SHL.se team names
shlseNameDictionary <- tibble(Lag = shlDoubleTeamNames, shlName = shlTeamNames)

#lookup table for the two SHL.se team names, have to make this one for team scrapes
shlseNameDictionary2 <- tibble(Lag2 = shlDoubleTeamNames2, shlName = shlTeamNames)

#abbreviations I'm using
shlAbbreviationDictionary <- tibble(Lag = shlTeamNames, Abbreviation = shlAbbreviations)

#abbreviation for just 18/19 season teams
shlAbbreviationDictionary %>%
  slice(2, 3, 4, 5, 6, 9, 10, 11, 13, 15, 16, 17, 19, 20) -> shlAbbreviations_1819

#abbreviation for just 15/16 season teams
#shlAbbreviationDictionary %>%
  #slice(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 15, 16, 17, 20) -> shlAbbreviations_1516


#read in the 18/19 SHL schedule
#probably need to transform date into an actual date
shl1819_schedule <- read_csv("schedule1819.csv")

shl1819_schedule %>%
  mutate(date2 = as.Date(shl1819_schedule$date, "%m/%d/%Y")) -> shl1819_schedule
  
shl1819_schedule$date2 <- str_replace_all(shl1819_schedule$date2, '00', '20')

#make it date - I think?
shl1819_schedule$date2 <- as.Date(shl1819_schedule$date2)


#read in the 15/16 SHL schedule
#shl1516_schedule <- `1516schedule`

#shl1516_schedule %>%
  #mutate(date2 = as.Date(shl1516_schedule$date, "%m/%d/%Y")) -> shl1516_schedule

#shl1516_schedule$date2 <- as.Date(shl1516_schedule$date2)

#get list of unique names from 18/19 season
#players_names_1819 <- unique(master_player_corsi_data$name)

