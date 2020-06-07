library(tidyverse)
library(rvest)
library(ggpubr)
library(ggthemes)
library(reshape2)
library(extrafont)
library(ggplot2)
library(grid)
library(gridExtra)

font_import()

# Team Info ---------------------------------------------------------------

#vector for team names, as displayed on SHL.se
shlse_team_names <- c("AIK", "Brynäs", "Djurgården", "Frölunda", "Färjestad", "HV71", "Karlskrona",
                      "Leksand", "Linköping", "Luleå", "Malmö", "MODO", "Mora", "Oskarshamn", "Örebro",
                      "Rögle", "Skellefteå", "Södertälje", "Timrå", "Växjö")

#vector for team names, when they display as double on SHL.se
shlse_team_names_double <- c("AIKAIK", "BrynäsBIF", "DjurgårdenDIF", "FrölundaFHC", "FärjestadFBK",
                             "HV71HV71", "KarlskronaKHK", "LeksandLIF", "LinköpingLHC", "LuleåLHF",
                             "MalmöMIF", "MODOMODO", "MoraMIK", "OskarshamnIKO", "ÖrebroÖRE", 
                             "RögleRBK", "SkellefteåSKE", "xxx", "TimråTIK", "VäxjöVLH")

#vector for team ids, as displayed on SHL.se
shlse_team_ids <- c("f7c8-f7c8isEb3", "1ab8-1ab8bfj7N", "2459-2459QTs1f", "087a-087aTQv9u",
                    "752c-752c12zB7Z", "3db0-3db09jXTE", "f51e-f51eM6sbL", "9541-95418PpkP",
                    "41c4-41c4BiYZU", "1a71-1a71gTHKh", "8e6f-8e6fUXJvi", "110b-110bJcIAI",
                    "dcba-dcbaXSrRU", "259b-259bYGVIp", "82eb-82ebmgaJ8", "ee93-ee93uy4oW",
                    "50e6-50e6DYeWM", "936a-936aAY8bT", "31d1-31d1NbSlR", "fe02-fe02mf1FN")

#vector for team names, as displayed on swehockey
swehockey_team_names <- c("AIK", "Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK",
                          "HV 71", "Karlskrona HK", "Leksands IF", "Linköping HC", "Luleå HF",
                          "IF Malmö Redhawks", "MODO Hockey", "Mora IK", "IK Oskarshamn",
                          "Örebro HK", "Rögle BK", "Skellefteå AIK", "Södertälje SK", "Timrå IK", 
                          "Växjö Lakers HC")

ha_swehockey_team_names <- c('IF Björklöven', 'Timrå IK', 'Västerås IK', 'MODO Hockey', 'BIK Karlskoga',
                             'HC Vita Hästen', 'Södertälje SK', 'Tingsryds AIF', 'Västerviks IK', 'Mora IK',
                             'Karlskrona HK', 'Kristianstads IK', 'Almtuna IS', 'AIK')

#vector for team abbreviations, created by me
shl_team_abbreviations <- c("aik", "bif", "dif", "fhc", "fbk", "hv", "khk", "lif", 
                            "lhc", "lhf", "mif", "modo", "mik","iko", "ohk", "rbk", 
                            "ske", "ssk", "tik", "vax")

#lookup table for all administrative team details
shl_team_dictionary <- tibble(shlse_team_name = shlse_team_names,
                              shlse_team_name_double = shlse_team_names_double,
                              swehockey_team_name = swehockey_team_names,
                              shl_team_abbreviation = shl_team_abbreviations,
                              shlse_team_id = shlse_team_ids)

#19/20 season team dictionary
shl_team_dictionary %>%
  slice(2, 3, 4, 5, 6, 8, 9, 10, 11, 14, 15, 16, 17, 20) -> shl_team_dictionary_1920

#18/19 season team dictionary
shl_team_dictionary %>%
  slice(2, 3, 4, 5, 6, 9, 10, 11, 13, 15, 16, 17, 19, 20) -> shl_team_dictionary_1819

#14/15 season team dictionary
shl_team_dictionary %>%
  slice(2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 17, 20) -> shl_team_dictionary_1415

#15/16 season team dictionary
shl_team_dictionary %>%
  slice(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 15, 16, 17, 20) -> shl_team_dictionary_1516

#16/17 season team dictionary
shl_team_dictionary %>%
  slice(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 15, 16, 17, 20) -> shl_team_dictionary_1617

#17/18 season team dictionary
shl_team_dictionary %>%
  slice(2, 3, 4, 5, 6, 7, 9, 10, 11, 13, 15, 16, 17, 20) -> shl_team_dictionary_1718

#master team schedules
master_team_schedule <- rbind(master_team_schedule_1415, master_team_schedule_1516, master_team_schedule_1617, 
                              master_team_schedule_1718, master_team_schedule_1819, master_team_schedule_1920)


# Player Info -------------------------------------------------------------

#player lookup with swehocky names
player_lookup_1920 <- read.csv("player_lookup_1920.csv", stringsAsFactors = FALSE)

player_lookup_1920_update <- tibble(swehockey = c("Karl Påhlsson", "Leo Lööf", "Adam Arvedson"),
                                    shlse = c("Karl Påhlsson", "Leo Lööf", "Adam Arvedson"))

player_lookup_1920 <- rbind(player_lookup_1920, player_lookup_1920_update) 

# 19/20 Schedules ---------------------------------------------------------------

#read in the 19/20 schedule, from stats.swehockey.se
shl_schedule_1920 <- read_csv("swehockey_schedule_1920.csv")

#add a row number
shl_schedule_1920 %>%
  mutate(game_number = row_number()) -> shl_schedule_1920
  
#add more columns to the 19/20 schedule
merge(shl_schedule_1920, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "home_team", by.y = "swehockey_team_name") -> shl_schedule_1920 

shl_schedule_1920 %>%
  rename(home_team_id = shlse_team_id, home_team_name_double = shlse_team_name_double) -> shl_schedule_1920

#same thing but for away team
merge(shl_schedule_1920, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "away_team", by.y = "swehockey_team_name") -> shl_schedule_1920 

shl_schedule_1920 %>%
  rename(away_team_id = shlse_team_id, away_team_name_double = shlse_team_name_double) -> shl_schedule_1920

#create a combined column concatenating the two team ids
shl_schedule_1920 %>%
  mutate(combined_ids = paste(home_team_id, away_team_id, sep = "-")) -> shl_schedule_1920

shl_schedule_1920 %>%
  mutate(combined_team_names_double = paste(home_team_name_double, away_team_name_double, sep = "-")) -> shl_schedule_1920


#add a column that puts the date in a friendly format
shl_schedule_1920 %>%
  mutate(date2 = as.Date(shl_schedule_1920$date, "%m/%d/%Y")) -> shl_schedule_1920

shl_schedule_1920$date2 <- str_replace_all(shl_schedule_1920$date2, '00', '20')

#reorder the columns so I don't lose my mind
shl_schedule_1920 <- shl_schedule_1920[c(5,12,2,1,4,6,8,10,7,9,11,3)]

#order by the date
shl_schedule_1920 <- shl_schedule_1920[order(shl_schedule_1920$game_number),]

#set dates as date format
shl_schedule_1920$date2 <- as.Date(shl_schedule_1920$date2)


#list of all gamedates from the season (all teams)
shl_dates_1920 <- unique(shl_schedule_1920$date2)


# 19/20 Team Schedules ----------------------------------------------------------

#create each team's schedule for the 19/20 schedule
  
#Brynäs
bif_schedule_1920 <- subset(shl_schedule_1920, grepl("Brynäs IF", shl_schedule_1920$combined_teams))
  
bif_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "BrynäsBIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> bif_schedule_1920

#Djurgården
dif_schedule_1920 <- subset(shl_schedule_1920, grepl("Djurgårdens IF", shl_schedule_1920$combined_teams))

dif_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "DjurgårdenDIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> dif_schedule_1920

#Frölunda
fhc_schedule_1920 <- subset(shl_schedule_1920, grepl("Frölunda HC", shl_schedule_1920$combined_teams))

fhc_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FrölundaFHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fhc_schedule_1920

#Färjestad
fbk_schedule_1920 <- subset(shl_schedule_1920, grepl("Färjestad BK", shl_schedule_1920$combined_teams))

fbk_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FärjestadFBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fbk_schedule_1920

#HV71
hv_schedule_1920 <- subset(shl_schedule_1920, grepl("HV 71", shl_schedule_1920$combined_teams))

hv_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "HV71HV71") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> hv_schedule_1920

#Leksand
lif_schedule_1920 <- subset(shl_schedule_1920, grepl("Leksands IF", shl_schedule_1920$combined_teams))

lif_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LeksandLIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lif_schedule_1920

#Linköping
lhc_schedule_1920 <- subset(shl_schedule_1920, grepl("Linköping HC", shl_schedule_1920$combined_teams))

lhc_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LinköpingLHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhc_schedule_1920

#Luleå
lhf_schedule_1920 <- subset(shl_schedule_1920, grepl("Luleå HF", shl_schedule_1920$combined_teams))

lhf_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LuleåLHF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhf_schedule_1920

#Malmö
mif_schedule_1920 <- subset(shl_schedule_1920, grepl("Malmö Redhawks", shl_schedule_1920$combined_teams))

mif_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MalmöMIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> mif_schedule_1920

#Oskarshamn
iko_schedule_1920 <- subset(shl_schedule_1920, grepl("IK Oskarshamn", shl_schedule_1920$combined_teams))

iko_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "OskarshamnIKO") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> iko_schedule_1920

#Örebro
ohk_schedule_1920 <- subset(shl_schedule_1920, grepl("Örebro HK", shl_schedule_1920$combined_teams))

ohk_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "ÖrebroÖRE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ohk_schedule_1920

#Rögle
rbk_schedule_1920 <- subset(shl_schedule_1920, grepl("Rögle BK", shl_schedule_1920$combined_teams))

rbk_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "RögleRBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> rbk_schedule_1920

#Skellefteå
ske_schedule_1920 <- subset(shl_schedule_1920, grepl("Skellefteå AIK", shl_schedule_1920$combined_teams))

ske_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "SkellefteåSKE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ske_schedule_1920

#Växjö
vax_schedule_1920 <- subset(shl_schedule_1920, grepl("Växjö Lakers", shl_schedule_1920$combined_teams))

vax_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "VäxjöVLH") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> vax_schedule_1920


master_team_schedule_1920 <- rbind(bif_schedule_1920, dif_schedule_1920, fbk_schedule_1920,
                                   fhc_schedule_1920, hv_schedule_1920, iko_schedule_1920,
                                   lhc_schedule_1920, lhf_schedule_1920, lif_schedule_1920,
                                   mif_schedule_1920, ohk_schedule_1920, rbk_schedule_1920,
                                   ske_schedule_1920, vax_schedule_1920)

# #messing around with a function to do this once as opposed to 14 separate times
# create_team_schedule_1920 <- function(team) {
#   
#   #team_schedules_1920 <- list(bif_schedule_1920, dif_schedule_1920)
#   
#   table <- subset(shl_schedule_1920, grepl(team, shl_schedule_1920$combined_teams))
#   
#   table %>%
#     mutate(game_number = row_number()) %>%
#     mutate(team = team) %>%
#     mutate(team_date = paste(team, date2, sep = '')) -> table
#   
#   return(table)
#   
# }


# 19/20 Stats Scraping ----------------------------------------------------------

#player points scrape
player_points <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/point?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) %>% 
    #make the headers unique
    make.unique()
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #rename column headers
    rename(G_PP = G.1, A_PP = A.1, TP_PP = TP.1,
           G_EV = G.2, A_EV = A.2, TP_EV = TP.2,
           G_SH = G.3, A_SH = A.3, TP_SH = TP.3) %>%
    
    #make columns numeric
    mutate_at(c('Nr', 'GP', 'G', 'A', 'A1', 'A2', 'TP',
                'G_EV', 'A_EV', 'TP_EV',
                'G_PP', 'A_PP', 'TP_PP',
                'G_SH', 'A_SH', 'TP_SH'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1920, by = "team_date") %>%
  
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_points_data_1920 <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
    
    for (date1 in as.list(shl_dates_1920[1:48])) {
    
    temp <- player_points(teamID)
    
    player_points_data_1920 <- rbind(player_points_data_1920, temp)
    
    print(teamID)
    rm(temp)
    
  }
  }

player_points_oo <- function(url) {
    
    # url <- read_html(paste('https://www.shl.se/statistik/spelare/point?season=from', date1,
    #                        'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
    #                        sep=''))
    
    table <- url %>%
      html_nodes("table") %>%
      .[1] %>%
      html_table(fill = TRUE, header = TRUE) %>%
      as.data.frame(fill = TRUE)
    
    #make first row the column headers
    colnames(table) <- as.character(unlist(table[1,])) %>% 
      #make the headers unique
      make.unique()
    table <- table[-1, ] %>%
      
      #add game date
      mutate(date1) %>%
      
      #rename column headers
      rename(G_PP = G.1, A_PP = A.1, TP_PP = TP.1,
             G_EV = G.2, A_EV = A.2, TP_EV = TP.2,
             G_SH = G.3, A_SH = A.3, TP_SH = TP.3) %>%
      
      #make columns numeric
      mutate_at(c('Nr', 'GP', 'G', 'A', 'A1', 'A2', 'TP',
                  'G_EV', 'A_EV', 'TP_EV',
                  'G_PP', 'A_PP', 'TP_PP',
                  'G_SH', 'A_SH', 'TP_SH'), as.numeric) %>%
      
      #remove the last word from the Spelare string
      mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
      
      #remove the last letter from the placeholder_name string
      mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
      
      #concatenate name and date for unique identifier
      mutate(name_date = paste(name, date1, sep = "")) %>%
      
      #concatenate team and date
      mutate(team_date = paste(Lag, date1, sep = "")) %>%
      
      #join in to get game number for the team/of the season
      left_join(master_team_schedule_1920, by = "team_date") %>%
      
      #remove unneeded columns by schedule data
      select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
                home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
    
    return(table)
    
  }

#player toi scrape
player_toi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/timeonice?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1920, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #separate TOI columns
    separate(TOI.GP, sep=':', into=c('Minutes', 'Seconds')) %>%
    separate(PPTOI.GP, sep=':', into=c('PPMinutes', 'PPSeconds')) %>%
    separate(ESTOI.GP, sep=':', into=c('ESMinutes', 'ESSeconds')) %>%
    separate(SHTOI.GP, sep=':', into=c('SHMinutes', 'SHSeconds')) %>%
    
    #convert minutes and seconds into numeric
    mutate_at(c('Minutes', 'Seconds','PPMinutes', 'PPSeconds',
                'ESMinutes', 'ESSeconds', 'SHMinutes', 'SHSeconds'), as.numeric) %>%
    
    #create TOI metric that's usable
    mutate(TOI = round((Minutes * 60 + Seconds)/60,1)) %>%
    mutate(PPTOI = round((PPMinutes * 60 + PPSeconds)/60,1)) %>%
    mutate(ESTOI = round((ESMinutes * 60 + ESSeconds)/60,1)) %>%
    mutate(SHTOI = round((SHMinutes * 60 + SHSeconds)/60,1))
  
  return(table)
  
  }

#loop through all combinations to get master player toi table
player_toi_data_1920 <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:48])) {
    
    temp <- player_toi(teamID)
    
    player_toi_data_1920 <- rbind(player_toi_data_1920, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#team corsi scrape
team_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove space in "Lag" column
    #mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
  
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
  
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1920, by = "team_date") %>%
  
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
            home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
}

#loop through all combination to get master team points table
team_corsi_data_1920 <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:48])) {
    
    temp <- team_corsi(teamID)
    
    team_corsi_data_1920 <- rbind(team_corsi_data_1920, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player corsi scrape
player_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
  
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1920, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team,
              `CF%`, `CCF%`, `FF%`, `CFF%`, `OIG%`, `OISVS%`, PDO, `TOI/GP`)) %>%
    
    #join in team corsi data
    #need this to get relative points later
    left_join(team_corsi_data_1920, by = "team_date") %>%
    
    #remove duplicated columns from join
    select(-c(Rank.y, Lag.y, GP.y, `CF%`, `CCF%`, `FF%`, `CFF%`,
              `OIG%`, `OISVS%`, PDO, date1.y, Lag2, game_number.y)) %>%
    
    #remove columns that'll break the update process
    #select(-c(shlse_team_name, swehockey_team_name, shl_team_abbreviation, shlse_team_id)) %>%
    
    #rename columns
    rename(Lag = Lag.x, CF = CF.x, CA = CA.x, FF = FF.x, FA = FA.x,
           date2 = date1.x, tmCF = CF.y, tmCA = CA.y, tmFF = FF.y,
           tmFA = FA.y) %>%
    
    #add in % columns
    mutate(`CF%` = round(CF/(CF + CA),3), `FF%` = round(FF/(FF + FA),3),
           CF_off = tmCF - CF, CA_off = tmCA - CA, FF_off = tmFF - FF,
           FA_off = tmFA - FA) %>%
    
    #add in team CF% and FF%
    mutate(`CF%_off` = round(CF_off/(CF_off + CA_off),3),
           `FF%_off` = round(FF_off/(FF_off + FA_off),3))      
  
  return(table)
  
}

#loop through all combination to get master player points table
player_corsi_data_1920 <- c() 
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:48])) {
    
    temp <- player_corsi(teamID)
    
    player_corsi_data_1920 <- rbind(player_corsi_data_1920, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player ev scrape
player_ev <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/equalstrength?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1920, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
      
      #rename columns
      rename(GF = X., GA = X..1, PlusMinus = X...) %>%
      
      mutate_at(c('GF', 'GA','PlusMinus'), as.numeric)
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_ev_data_1920 <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:48])) {
    
    temp <- player_ev(teamID)
    
    player_ev_data_1920 <- rbind(player_ev_data_1920, temp)
    
    print(teamID)
    rm(temp)
    
  }
}



# 19/20 Player Card -------------------------------------------------------------

#Full player card
#This function will create the full player card, creating four graphs
#and plotting them all together
player_card <- function(player_name) {
  
  #TOI
  #create one-off table to expand y axis as needed
  #will use this later to expand the y-axis if needed
  y_toi_axis <- player_toi_data_1920 %>%
    filter(name == player_name)
  
  #pull relevant data from master
  table_toi <- player_toi_data_1920 %>%
    filter(name == player_name) %>%
    select(game_number, ESTOI, PPTOI, SHTOI) %>%
    #rename it for what I want to display in the legend
    rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
  
  #melt so can plot a stacked column (only way to create stacked column?)
  table_toi2 <- melt(table_toi, id.var="game_number") %>%
    rename(GameNumber = game_number, Situation = variable, TOI = value)
  
  #create the order of those stacked components
  #don't fully understand this one - needed to create the order I want the stacking in
  levels(table_toi2$Situation)
  table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
  
  #ggplot the toi visual
  toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),], 
                       aes(x = GameNumber, y = TOI, fill = Situation)) +
    geom_bar(stat = "identity") +
    
    #creating the title for the overall plot, plus the TOI subheader data
    labs(title = paste(player_name, " 19/20 Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
         subtitle = paste("TOI by Game Situation \n",
                          round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
                          round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
                          round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = '')) +
    theme_few() +
    #use the font I loaded in specifically for this visual
    theme(text = element_text(family = "Sweden Sans")) +
    theme(plot.title = element_text(face = "bold")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#FF7F7F", "#FFD17F", "#7F7FFF")) +
    #x axis spans entire 52 game season
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    #default y axis goes from 0 to 20 minutes
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0))
  
  #but for players with games > 20 minutes, this is where the y axis table comes in
  if(max(y_toi_axis$TOI) > 20){
    toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
                                                  expand = c(0, 0))
  }
  
  #Player point production
  #all same as above
  y_points_axis <- player_points_data_1920 %>%
    filter(name == player_name)
  
  table_points <- player_points_data_1920 %>%
    filter(name == player_name) %>%
    select(game_number, G, A1, A2)
  
  table_points_2 <- melt(table_points, id.var="game_number") %>%
    rename(GameNumber = game_number, PointType = variable, Points = value)
  
  levels(table_points_2$PointType)
  table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
  
  points_visual <- ggplot(table_points_2[order(table_points_2$PointType),], 
                          aes(x = GameNumber, y = Points, fill = PointType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Point Production \n",
                          sum(table_points$G), ' G  -  ',
                          sum(table_points$A1), ' A1  -  ',
                          sum(table_points$A2), ' A2  (',
                          
                          round((sum(y_points_axis$TP) / sum(y_toi_axis$TOI)) * 60, 2),
                          
                          ' P/60)',
                          
                          sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#CCE4CC", "#B2B2FF", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0))
  
  
  if(max(y_points_axis$TP) > 3){
    points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_points_axis$TP)),
                                                        expand = c(0, 0))
  }
  
  #corsi
  y_corsi_axis <- player_corsi_data_1920 %>%
    filter(name == player_name) %>%
    #select(gameNumber, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2) %>%
    mutate(CA_abs = abs(CA))
  
  #storing values to calculate stats for subheader (CF%, relative CF%)
  player_corsi <- round((sum(y_corsi_axis$CF) / (sum(y_corsi_axis$CF) + sum(y_corsi_axis$CA_abs)) *100), 1)
  player_off_corsi <- round((sum(y_corsi_axis$CF_off) / (sum(y_corsi_axis$CF_off) + sum(y_corsi_axis$CA_off)) *100), 1)
  CF_rel <- player_corsi - player_off_corsi
  
  table_corsi <- player_corsi_data_1920 %>%
    filter(name == player_name) %>%
    select(game_number.x, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2)
  
  table_corsi_2 <- melt(table_corsi, id.var="game_number.x") %>%
    rename(GameNumber = game_number.x, ShotType = variable, Shots = value)
  
  levels(table_corsi_2$Type)
  table_corsi_2$ShotType <- factor(table_corsi_2$ShotType, levels = rev(levels(table_corsi_2$ShotType)))
  
  corsi_visual <- ggplot(table_corsi_2[order(table_corsi_2$ShotType),], 
                         aes(x = GameNumber, y = Shots, fill = ShotType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Corsi For and Against \n",  
                          round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1), 
                          ' CF%', 
                          
                          ' (', round(CF_rel, 1),' rel)', sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    #flip the order of the legend so CF is on top and CA is on the bottom
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-20, 20), expand = c(0, 0)) 
  
  
  if(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) > 20){
    corsi_visual <- corsi_visual + scale_y_continuous(limits=c(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) * -1, max(y_corsi_axis$CF,y_corsi_axis$CA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #Goals
  #same methodology as all others
  y_goals_axis <- player_ev_data_1920 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2) %>%
    mutate(GA_abs = abs(GA))
  
  table_goals <- player_ev_data_1920 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2)
  
  table_goals_2 <- melt(table_goals, id.var="game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)
  
  levels(table_goals_2$GoalType)
  table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))
  
  goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),], 
                         aes(x = GameNumber, y = Goals, fill = GoalType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste('ES Goals For and Against \n', 
                          sum(table_goals$GF), ' GF  -  ', 
                          sum(y_goals_axis$GA_abs), ' GA ',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se', ' | ', '@zellenthal_DIF'), sep = '') +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) 
  
  
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #player_card_visual <- ggarrange(toi_visual, points_visual, points_visual, ncol = 1, nrow = 3)
  
  #return(player_card_visual)
  
  #title <- text_grob(paste(player_name, '18/19 Season', sep = ' ')) 
  
  #using the ggpubr package so that I can align all the ggplots on one visual
  #and so all the x axes align
  g2 <- ggplotGrob(toi_visual)
  g3 <- ggplotGrob(corsi_visual)
  g4 <- ggplotGrob(goals_visual)
  g5 <- ggplotGrob(points_visual)
  g <- rbind(g2, g5, g3, g4, size = "first")
  g$widths <- unit.pmax(g2$widths, g5$widths, g3$widths, g4$widths)
  grid.newpage()
  grid.draw(g)
  
  return(g)
  
}

# Updates -----------------------------------------------------------------

player_toi_data_1920 <- player_toi_data_1920[,c(1:30)]
player_points_data_1920 <- player_points_data_1920[,c(1:31)]
player_corsi_data_1920 <- player_corsi_data_1920[,c(1:32)]
player_ev_data_1920 <- player_ev_data_1920[,c(1:26)]

#points
player_points_data_1920_update <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:62])) {
    
    temp <- player_points(teamID)
    
    player_points_data_1920_update <- rbind(player_points_data_1920_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(player_points_data_1920_update, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> player_points_data_1920_update

player_points_data_1920 <- rbind(player_points_data_1920, player_points_data_1920_update)


#team corsi
team_corsi_data_1920_update <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:62])) {
    
    temp <- team_corsi(teamID)
    
    team_corsi_data_1920_update <- rbind(team_corsi_data_1920_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}



left_join(team_corsi_data_1920_update, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> team_corsi_data_1920_update

team_corsi_data_1920 <- rbind(team_corsi_data_1920, team_corsi_data_1920_update)


#player corsi
player_corsi_data_1920_update <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:62])) {
    
    temp <- player_corsi(teamID)
    
    player_corsi_data_1920_update <- rbind(player_corsi_data_1920_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

player_corsi_data_1920_update[,c(1:20,25:32)] -> player_corsi_data_1920_update

left_join(player_corsi_data_1920_update, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> player_corsi_data_1920_update

player_corsi_data_1920 <- rbind(player_corsi_data_1920, player_corsi_data_1920_update)


#ev
player_ev_data_1920_update <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:62])) {
    
    temp <- player_ev(teamID)
    
    player_ev_data_1920_update <- rbind(player_ev_data_1920_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(player_ev_data_1920_update, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> player_ev_data_1920_update

player_ev_data_1920 <- rbind(player_ev_data_1920, player_ev_data_1920_update)

#toi
player_toi_data_1920_update <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:62])) {
    
    temp <- player_toi(teamID)
    
    player_toi_data_1920_update <- rbind(player_toi_data_1920_update, temp)
    
    print(teamID)
    rm(temp)
  
  }
}

left_join(player_toi_data_1920_update, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> player_toi_data_1920_update

player_toi_data_1920 <- rbind(player_toi_data_1920, player_toi_data_1920_update)

#left join in swehockey name
player_toi_data_1920 %>%
  left_join(player_lookup_1920, by = c("name" = "shlse")) %>% 
  rename(swehockey_name = swehockey) -> player_toi_data_1920

player_points_data_1920 %>%
  left_join(player_lookup_1920, by = c("name" = "shlse")) %>% 
  rename(swehockey_name = swehockey) -> player_points_data_1920

player_corsi_data_1920 %>%
  left_join(player_lookup_1920, by = c("name" = "shlse")) %>% 
  rename(swehockey_name = swehockey) -> player_corsi_data_1920

player_ev_data_1920 %>%
  left_join(player_lookup_1920, by = c("name" = "shlse")) %>% 
  rename(swehockey_name = swehockey) -> player_ev_data_1920


#join in friendlier team names
left_join(player_points_data_1920, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> player_points_data_1920
left_join(player_ev_data_1920, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> player_ev_data_1920
left_join(player_corsi_data_1920, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> player_corsi_data_1920
left_join(player_toi_data_1920, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> player_toi_data_1920
left_join(team_corsi_data_1920, shl_team_dictionary_1920, by = c("Lag" = "shlse_team_name_double")) -> team_corsi_data_1920


# CSV Exports -------------------------------------------------------------

#Player Corsi
write.csv(player_corsi_data_1920, file = "player_corsi_data_1920.csv")

#Player EV
write.csv(player_ev_data_1920, file = "player_ev_data_1920.csv")

#Player Points
write.csv(player_points_data_1920, file = "player_points_data_1920.csv")

#Player TOI
write.csv(player_toi_data_1920, file = "player_toi_data_1920.csv")

#Unique Names
write.csv(unique(player_corsi_data_1920$swehockey_name), file = "player_names_1920.csv")






























box_score_data 














# 19/20 HA Team Schedules ------------------------------------------------------

ha_schedule_1920 <- read_csv("ha_swehockey_schedule_1920.csv")

#add a row number
ha_schedule_1920 %>%
  mutate(game_number = row_number()) -> ha_schedule_1920


ha_schedule_1920 <- ha_schedule_1920[c(5,1:4)]

#order by the date
ha_schedule_1920 <- ha_schedule_1920[order(ha_schedule_1920$game_number),]

#create each team's schedule for the 19/20 schedule

#IF Bjorkloven
ifb_schedule_1920 <- subset(ha_schedule_1920, grepl("IF Björklöven", ha_schedule_1920$combined_teams))

ifb_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "IF Björklöven") %>%
  mutate(team_date = paste(team, date, sep = '')) -> ifb_schedule_1920

#Timrå IK
tik_schedule_1920 <- subset(ha_schedule_1920, grepl("Timrå IK", ha_schedule_1920$combined_teams))

tik_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "Timrå IK") %>%
  mutate(team_date = paste(team, date, sep = '')) -> tik_schedule_1920

#Västerås IK
vik_schedule_1920 <- subset(ha_schedule_1920, grepl("Västerås IK", ha_schedule_1920$combined_teams))

vik_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "Västerås IK") %>%
  mutate(team_date = paste(team, date, sep = '')) -> vik_schedule_1920

#MODO Hockey
modo_schedule_1920 <- subset(ha_schedule_1920, grepl("MODO Hockey", ha_schedule_1920$combined_teams))

modo_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MODO Hockey") %>%
  mutate(team_date = paste(team, date, sep = '')) -> modo_schedule_1920

#BIK Karlskoga
bik_schedule_1920 <- subset(ha_schedule_1920, grepl("BIK Karlskoga", ha_schedule_1920$combined_teams))

bik_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "BIK Karlskoga") %>%
  mutate(team_date = paste(team, date, sep = '')) -> bik_schedule_1920

#HC Vita Hästen
vit_schedule_1920 <- subset(ha_schedule_1920, grepl("HC Vita Hästen", ha_schedule_1920$combined_teams))

vit_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "HC Vita Hästen") %>%
  mutate(team_date = paste(team, date, sep = '')) -> vit_schedule_1920

#Södertälje SK
ssk_schedule_1920 <- subset(ha_schedule_1920, grepl("Södertälje SK", ha_schedule_1920$combined_teams))

ssk_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "Södertälje SK") %>%
  mutate(team_date = paste(team, date, sep = '')) -> ssk_schedule_1920

#Tingsryds AIF
taif_schedule_1920 <- subset(ha_schedule_1920, grepl("Tingsryds AIF", ha_schedule_1920$combined_teams))

taif_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "Tingsryds AIF") %>%
  mutate(team_date = paste(team, date, sep = '')) -> taif_schedule_1920

#Västerviks IK
vaik_schedule_1920 <- subset(ha_schedule_1920, grepl("Västerviks IK", ha_schedule_1920$combined_teams))

vaik_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "Västerviks IK") %>%
  mutate(team_date = paste(team, date, sep = '')) -> vaik_schedule_1920

#Mora IK
mik_schedule_1920 <- subset(ha_schedule_1920, grepl("Mora IK", ha_schedule_1920$combined_teams))

mik_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "Mora IK") %>%
  mutate(team_date = paste(team, date, sep = '')) -> mik_schedule_1920

#Karlskrona HK
khk_schedule_1920 <- subset(ha_schedule_1920, grepl("Karlskrona HK", ha_schedule_1920$combined_teams))

khk_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "Karlskrona HK") %>%
  mutate(team_date = paste(team, date, sep = '')) -> khk_schedule_1920

#Kristianstads IK
kik_schedule_1920 <- subset(ha_schedule_1920, grepl("Kristianstads IK", ha_schedule_1920$combined_teams))

kik_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "Kristianstads IK") %>%
  mutate(team_date = paste(team, date, sep = '')) -> kik_schedule_1920

#Almtuna IS
ais_schedule_1920 <- subset(ha_schedule_1920, grepl("Almtuna IS", ha_schedule_1920$combined_teams))

ais_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "Almtuna IS") %>%
  mutate(team_date = paste(team, date, sep = '')) -> ais_schedule_1920

#AIK
aik_schedule_1920 <- subset(ha_schedule_1920, grepl("AIK", ha_schedule_1920$combined_teams))

aik_schedule_1920 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "AIK") %>%
  mutate(team_date = paste(team, date, sep = '')) -> aik_schedule_1920


ha_master_team_schedule_1920 <- rbind(ifb_schedule_1920, tik_schedule_1920, vik_schedule_1920,
                                   modo_schedule_1920, bik_schedule_1920, vit_schedule_1920,
                                   ssk_schedule_1920, taif_schedule_1920, vaik_schedule_1920,
                                   mik_schedule_1920, khk_schedule_1920, kik_schedule_1920,
                                   ais_schedule_1920, aik_schedule_1920)





# 18/19 Schedules ---------------------------------------------------------------

#read in the 18/19 schedule, from stats.swehockey.se
#shl_schedule_1819 <- read_csv("swehockey_schedule_1819.csv")

#add a column that puts the date in a friendly format
#shl_schedule_1819 %>%
#mutate(date2 = as.Date(shl_schedule_1819$date, "%m/%d/%Y")) -> shl_schedule_1819

#shl_schedule_1819$date2 <- str_replace_all(shl_schedule_1819$date2, '00', '20')


#read in the 19/20 schedule, from stats.swehockey.se
shl_schedule_1819 <- read_csv("swehockey_schedule_1819.csv")

#add a row number
shl_schedule_1819 %>%
  mutate(game_number = row_number()) -> shl_schedule_1819

#add more columns to the 19/20 schedule
merge(shl_schedule_1819, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "home_team", by.y = "swehockey_team_name") -> shl_schedule_1819 

shl_schedule_1819 %>%
  rename(home_team_id = shlse_team_id, home_team_name_double = shlse_team_name_double) -> shl_schedule_1819

#same thing but for away team
merge(shl_schedule_1819, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "away_team", by.y = "swehockey_team_name") -> shl_schedule_1819 

shl_schedule_1819 %>%
  rename(away_team_id = shlse_team_id, away_team_name_double = shlse_team_name_double) -> shl_schedule_1819

#create a combined column concatenating the two team ids
shl_schedule_1819 %>%
  mutate(combined_ids = paste(home_team_id, away_team_id, sep = "-")) -> shl_schedule_1819

shl_schedule_1819 %>%
  mutate(combined_team_names_double = paste(home_team_name_double, away_team_name_double, sep = "-")) -> shl_schedule_1819


#add a column that puts the date in a friendly format
shl_schedule_1819 %>%
  mutate(date2 = as.Date(shl_schedule_1819$date, "%m/%d/%Y")) -> shl_schedule_1819

shl_schedule_1819$date2 <- str_replace_all(shl_schedule_1819$date2, '00', '20')

#reorder the columns so I don't lose my mind
shl_schedule_1819 <- shl_schedule_1819[c(5,12,2,1,4,6,8,10,7,9,11,3)]

#order by the date
shl_schedule_1819 <- shl_schedule_1819[order(shl_schedule_1819$game_number),]

#set dates as date format
shl_schedule_1819$date2 <- as.Date(shl_schedule_1819$date2)

#list of all gamedates from the season (all teams)
shl_dates_1819 <- unique(shl_schedule_1819$date2)


# 18/19 Team Schedules ----------------------------------------------------------

#create each team's schedule for the 19/20 schedule

#Brynäs
bif_schedule_1819 <- subset(shl_schedule_1819, grepl("Brynäs IF", shl_schedule_1819$combined_teams))

bif_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "BrynäsBIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> bif_schedule_1819

#Djurgården
dif_schedule_1819 <- subset(shl_schedule_1819, grepl("Djurgårdens IF", shl_schedule_1819$combined_teams))

dif_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "DjurgårdenDIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> dif_schedule_1819

#Frölunda
fhc_schedule_1819 <- subset(shl_schedule_1819, grepl("Frölunda HC", shl_schedule_1819$combined_teams))

fhc_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FrölundaFHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fhc_schedule_1819

#Färjestad
fbk_schedule_1819 <- subset(shl_schedule_1819, grepl("Färjestad BK", shl_schedule_1819$combined_teams))

fbk_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FärjestadFBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fbk_schedule_1819

#HV71
hv_schedule_1819 <- subset(shl_schedule_1819, grepl("HV 71", shl_schedule_1819$combined_teams))

hv_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "HV71HV71") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> hv_schedule_1819

#Linköping
lhc_schedule_1819 <- subset(shl_schedule_1819, grepl("Linköping HC", shl_schedule_1819$combined_teams))

lhc_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LinköpingLHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhc_schedule_1819

#Luleå
lhf_schedule_1819 <- subset(shl_schedule_1819, grepl("Luleå HF", shl_schedule_1819$combined_teams))

lhf_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LuleåLHF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhf_schedule_1819

#Malmö
mif_schedule_1819 <- subset(shl_schedule_1819, grepl("Malmö Redhawks", shl_schedule_1819$combined_teams))

mif_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MalmöMIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> mif_schedule_1819

#Mora
mik_schedule_1819 <- subset(shl_schedule_1819, grepl("Mora IK", shl_schedule_1819$combined_teams))

mik_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MoraMIK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> mik_schedule_1819

#Örebro
ohk_schedule_1819 <- subset(shl_schedule_1819, grepl("Örebro HK", shl_schedule_1819$combined_teams))

ohk_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "ÖrebroÖRE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ohk_schedule_1819

#Rögle
rbk_schedule_1819 <- subset(shl_schedule_1819, grepl("Rögle BK", shl_schedule_1819$combined_teams))

rbk_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "RögleRBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> rbk_schedule_1819

#Skellefteå
ske_schedule_1819 <- subset(shl_schedule_1819, grepl("Skellefteå AIK", shl_schedule_1819$combined_teams))

ske_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "SkellefteåSKE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ske_schedule_1819

#Timrå
tik_schedule_1819 <- subset(shl_schedule_1819, grepl("Timrå IK", shl_schedule_1819$combined_teams))

tik_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "TimråTIK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> tik_schedule_1819

#Växjö
vax_schedule_1819 <- subset(shl_schedule_1819, grepl("Växjö Lakers", shl_schedule_1819$combined_teams))

vax_schedule_1819 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "VäxjöVLH") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> vax_schedule_1819


master_team_schedule_1819 <- rbind(bif_schedule_1819, dif_schedule_1819, fbk_schedule_1819,
                                   fhc_schedule_1819, hv_schedule_1819, lhc_schedule_1819, 
                                   lhf_schedule_1819, mif_schedule_1819, mik_schedule_1819, 
                                   ohk_schedule_1819, rbk_schedule_1819, ske_schedule_1819, 
                                   tik_schedule_1819, vax_schedule_1819)


# 18/19 Stats Scraping ----------------------------------------------------------

#player points scrape
player_points <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/point?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) %>% 
    #make the headers unique
    make.unique()
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #rename column headers
    rename(G_PP = G.1, A_PP = A.1, TP_PP = TP.1,
           G_EV = G.2, A_EV = A.2, TP_EV = TP.2,
           G_SH = G.3, A_SH = A.3, TP_SH = TP.3) %>%
    
    #make columns numeric
    mutate_at(c('Nr', 'GP', 'G', 'A', 'A1', 'A2', 'TP',
                'G_EV', 'A_EV', 'TP_EV',
                'G_PP', 'A_PP', 'TP_PP',
                'G_SH', 'A_SH', 'TP_SH'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1819, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_points_data_1819 <- c()
for (teamID in shl_team_dictionary_1819$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1819[1:70])) {
    
    temp <- player_points(teamID)
    
    player_points_data_1819 <- rbind(player_points_data_1819, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player toi scrape
player_toi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/timeonice?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1819, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #separate TOI columns
    separate(TOI.GP, sep=':', into=c('Minutes', 'Seconds')) %>%
    separate(PPTOI.GP, sep=':', into=c('PPMinutes', 'PPSeconds')) %>%
    separate(ESTOI.GP, sep=':', into=c('ESMinutes', 'ESSeconds')) %>%
    separate(SHTOI.GP, sep=':', into=c('SHMinutes', 'SHSeconds')) %>%
    
    #convert minutes and seconds into numeric
    mutate_at(c('Minutes', 'Seconds','PPMinutes', 'PPSeconds',
                'ESMinutes', 'ESSeconds', 'SHMinutes', 'SHSeconds'), as.numeric) %>%
    
    #create TOI metric that's usable
    mutate(TOI = round((Minutes * 60 + Seconds)/60,1)) %>%
    mutate(PPTOI = round((PPMinutes * 60 + PPSeconds)/60,1)) %>%
    mutate(ESTOI = round((ESMinutes * 60 + ESSeconds)/60,1)) %>%
    mutate(SHTOI = round((SHMinutes * 60 + SHSeconds)/60,1))
  
  return(table)
  
}

#loop through all combinations to get master player toi table
player_toi_data_1819 <- c()
for (teamID in shl_team_dictionary_1819$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1819[1:70])) {
    
    temp <- player_toi(teamID)
    
    player_toi_data_1819 <- rbind(player_toi_data_1819, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#team corsi scrape
team_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove space in "Lag" column
    #mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1819, by = "team_date") %>%
    
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
}

#loop through all combination to get master team points table
team_corsi_data_1819 <- c()
for (teamID in shl_team_dictionary_1819$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1819[1:70])) {
    
    temp <- team_corsi(teamID)
    
    team_corsi_data_1819 <- rbind(team_corsi_data_1819, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player corsi scrape
player_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1819, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team,
              `CF%`, `CCF%`, `FF%`, `CFF%`, `OIG%`, `OISVS%`, PDO, `TOI/GP`)) %>%
    
    #join in team corsi data
    #need this to get relative points later
    left_join(team_corsi_data_1819, by = "team_date") %>%
    
    #remove duplicated columns from join
    select(-c(Rank.y, Lag.y, GP.y, `CF%`, `CCF%`, `FF%`, `CFF%`,
              `OIG%`, `OISVS%`, PDO, date1.y, Lag2, game_number.y)) %>%
    
    #remove columns that'll break the update process
    #select(-c(shlse_team_name, swehockey_team_name, shl_team_abbreviation, shlse_team_id)) %>%
    
    #rename columns
    rename(Lag = Lag.x, CF = CF.x, CA = CA.x, FF = FF.x, FA = FA.x,
           date2 = date1.x, tmCF = CF.y, tmCA = CA.y, tmFF = FF.y,
           tmFA = FA.y) %>%
    
    #add in % columns
    mutate(`CF%` = round(CF/(CF + CA),3), `FF%` = round(FF/(FF + FA),3),
           CF_off = tmCF - CF, CA_off = tmCA - CA, FF_off = tmFF - FF,
           FA_off = tmFA - FA) %>%
    
    #add in team CF% and FF%
    mutate(`CF%_off` = round(CF_off/(CF_off + CA_off),3),
           `FF%_off` = round(FF_off/(FF_off + FA_off),3))      
  
  return(table)
  
}

#loop through all combination to get master player points table
player_corsi_data_1819 <- c() 
for (teamID in shl_team_dictionary_1819$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1819[1:70])) {
    
    temp <- player_corsi(teamID)
    
    player_corsi_data_1819 <- rbind(player_corsi_data_1819, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player ev scrape
player_ev <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/equalstrength?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1819, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #rename columns
    rename(GF = X., GA = X..1, PlusMinus = X...) %>%
    
    mutate_at(c('GF', 'GA','PlusMinus'), as.numeric)
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_ev_data_1819 <- c()
for (teamID in shl_team_dictionary_1819$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1819[1:70])) {
    
    temp <- player_ev(teamID)
    
    player_ev_data_1819 <- rbind(player_ev_data_1819, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#join in friendlier team names
left_join(player_points_data_1819, shl_team_dictionary_1819, by = c("Lag" = "shlse_team_name_double")) -> player_points_data_1819
left_join(player_ev_data_1819, shl_team_dictionary_1819, by = c("Lag" = "shlse_team_name_double")) -> player_ev_data_1819
left_join(player_corsi_data_1819, shl_team_dictionary_1819, by = c("Lag" = "shlse_team_name_double")) -> player_corsi_data_1819
left_join(player_toi_data_1819, shl_team_dictionary_1819, by = c("Lag" = "shlse_team_name_double")) -> player_toi_data_1819
left_join(team_corsi_data_1819, shl_team_dictionary_1819, by = c("Lag" = "shlse_team_name_double")) -> team_corsi_data_1819


# 18/19 Player Card -------------------------------------------------------------

#Full player card
#This function will create the full player card, creating four graphs
#and plotting them all together
player_card_1819 <- function(player_name) {
  
  #TOI
  #create one-off table to expand y axis as needed
  #will use this later to expand the y-axis if needed
  y_toi_axis <- player_toi_data_1819 %>%
    filter(name == player_name)
  
  #pull relevant data from master
  table_toi <- player_toi_data_1819 %>%
    filter(name == player_name) %>%
    select(game_number, ESTOI, PPTOI, SHTOI) %>%
    #rename it for what I want to display in the legend
    rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
  
  #melt so can plot a stacked column (only way to create stacked column?)
  table_toi2 <- melt(table_toi, id.var="game_number") %>%
    rename(GameNumber = game_number, Situation = variable, TOI = value)
  
  #create the order of those stacked components
  #don't fully understand this one - needed to create the order I want the stacking in
  levels(table_toi2$Situation)
  table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
  
  #ggplot the toi visual
  toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),], 
                       aes(x = GameNumber, y = TOI, fill = Situation)) +
    geom_bar(stat = "identity") +
    
    #creating the title for the overall plot, plus the TOI subheader data
    labs(title = paste(player_name, " 18/19 Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
         subtitle = paste("TOI by Game Situation \n",
                          round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
                          round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
                          round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = '')) +
    theme_few() +
    #use the font I loaded in specifically for this visual
    theme(text = element_text(family = "Sweden Sans")) +
    theme(plot.title = element_text(face = "bold")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#FF7F7F", "#FFD17F", "#7F7FFF")) +
    #x axis spans entire 52 game season
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    #default y axis goes from 0 to 20 minutes
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0))
  
  #but for players with games > 20 minutes, this is where the y axis table comes in
  if(max(y_toi_axis$TOI) > 20){
    toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
                                                  expand = c(0, 0))
  }
  
  #Player point production
  #all same as above
  y_points_axis <- player_points_data_1819 %>%
    filter(name == player_name)
  
  table_points <- player_points_data_1819 %>%
    filter(name == player_name) %>%
    select(game_number, G, A1, A2)
  
  table_points_2 <- melt(table_points, id.var="game_number") %>%
    rename(GameNumber = game_number, PointType = variable, Points = value)
  
  levels(table_points_2$PointType)
  table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
  
  points_visual <- ggplot(table_points_2[order(table_points_2$PointType),], 
                          aes(x = GameNumber, y = Points, fill = PointType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Point Production \n",
                          sum(table_points$G), ' G  -  ',
                          sum(table_points$A1), ' A1  -  ',
                          sum(table_points$A2), ' A2  (',
                          
                          round((sum(y_points_axis$TP) / sum(y_toi_axis$TOI)) * 60, 2),
                          
                          ' P/60)',
                          
                          sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#CCE4CC", "#B2B2FF", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0))
  
  
  if(max(y_points_axis$TP) > 3){
    points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_points_axis$TP)),
                                                        expand = c(0, 0))
  }
  
  #corsi
  y_corsi_axis <- player_corsi_data_1819 %>%
    filter(name == player_name) %>%
    #select(gameNumber, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2) %>%
    mutate(CA_abs = abs(CA))
  
  #storing values to calculate stats for subheader (CF%, relative CF%)
  player_corsi <- round((sum(y_corsi_axis$CF) / (sum(y_corsi_axis$CF) + sum(y_corsi_axis$CA_abs)) *100), 1)
  player_off_corsi <- round((sum(y_corsi_axis$CF_off) / (sum(y_corsi_axis$CF_off) + sum(y_corsi_axis$CA_off)) *100), 1)
  CF_rel <- player_corsi - player_off_corsi
  
  table_corsi <- player_corsi_data_1819 %>%
    filter(name == player_name) %>%
    select(game_number.x, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2)
  
  table_corsi_2 <- melt(table_corsi, id.var="game_number.x") %>%
    rename(GameNumber = game_number.x, ShotType = variable, Shots = value)
  
  levels(table_corsi_2$Type)
  table_corsi_2$ShotType <- factor(table_corsi_2$ShotType, levels = rev(levels(table_corsi_2$ShotType)))
  
  corsi_visual <- ggplot(table_corsi_2[order(table_corsi_2$ShotType),], 
                         aes(x = GameNumber, y = Shots, fill = ShotType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Corsi For and Against \n",  
                          round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1), 
                          ' CF%', 
                          
                          ' (', round(CF_rel, 1),' rel)', sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    #flip the order of the legend so CF is on top and CA is on the bottom
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-20, 20), expand = c(0, 0)) 
  
  
  if(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) > 20){
    corsi_visual <- corsi_visual + scale_y_continuous(limits=c(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) * -1, max(y_corsi_axis$CF,y_corsi_axis$CA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #Goals
  #same methodology as all others
  y_goals_axis <- player_ev_data_1819 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2) %>%
    mutate(GA_abs = abs(GA))
  
  table_goals <- player_ev_data_1819 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2)
  
  table_goals_2 <- melt(table_goals, id.var="game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)
  
  levels(table_goals_2$GoalType)
  table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))
  
  goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),], 
                         aes(x = GameNumber, y = Goals, fill = GoalType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste('ES Goals For and Against \n', 
                          sum(table_goals$GF), ' GF  -  ', 
                          sum(y_goals_axis$GA_abs), ' GA ',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se', ' | ', '@zellenthal_DIF'), sep = '') +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) 
  
  
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #player_card_visual <- ggarrange(toi_visual, points_visual, points_visual, ncol = 1, nrow = 3)
  
  #return(player_card_visual)
  
  #title <- text_grob(paste(player_name, '18/19 Season', sep = ' ')) 
  
  #using the ggpubr package so that I can align all the ggplots on one visual
  #and so all the x axes align
  g2 <- ggplotGrob(toi_visual)
  g3 <- ggplotGrob(corsi_visual)
  g4 <- ggplotGrob(goals_visual)
  g5 <- ggplotGrob(points_visual)
  g <- rbind(g2, g5, g3, g4, size = "first")
  g$widths <- unit.pmax(g2$widths, g5$widths, g3$widths, g4$widths)
  grid.newpage()
  grid.draw(g)
  
  return(g)
  
}

# CSV Exports -------------------------------------------------------------

#Player Corsi
write.csv(player_corsi_data_1819, file = "player_corsi_data_1819.csv")

#Player EV
write.csv(player_ev_data_1819, file = "player_ev_data_1819.csv")

#Player Points
write.csv(player_points_data_1819, file = "player_points_data_1819.csv")

#Player TOI
write.csv(player_toi_data_1819, file = "player_toi_data_1819.csv")

#Unique Names
write.csv(unique(player_toi_data_1819$name), file = "player_names_1819.csv")











# 14/15 Schedules ---------------------------------------------------------------

#read in the 19/20 schedule, from stats.swehockey.se
shl_schedule_1415 <- read_csv("swehockey_schedule_1415.csv")

#add a row number
shl_schedule_1415 %>%
  mutate(game_number = row_number()) -> shl_schedule_1415

#add more columns to the 19/20 schedule
merge(shl_schedule_1415, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "home_team", by.y = "swehockey_team_name") -> shl_schedule_1415 

shl_schedule_1415 %>%
  rename(home_team_id = shlse_team_id, home_team_name_double = shlse_team_name_double) -> shl_schedule_1415

#same thing but for away team
merge(shl_schedule_1415, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "away_team", by.y = "swehockey_team_name") -> shl_schedule_1415 

shl_schedule_1415 %>%
  rename(away_team_id = shlse_team_id, away_team_name_double = shlse_team_name_double) -> shl_schedule_1415

#create a combined column concatenating the two team ids
shl_schedule_1415 %>%
  mutate(combined_ids = paste(home_team_id, away_team_id, sep = "-")) -> shl_schedule_1415

shl_schedule_1415 %>%
  mutate(combined_team_names_double = paste(home_team_name_double, away_team_name_double, sep = "-")) -> shl_schedule_1415


#add a column that puts the date in a friendly format
shl_schedule_1415 %>%
  mutate(date2 = as.Date(shl_schedule_1415$date, "%m/%d/%Y")) -> shl_schedule_1415

shl_schedule_1415$date2 <- str_replace_all(shl_schedule_1415$date2, '00', '20')

#reorder the columns so I don't lose my mind
shl_schedule_1415 <- shl_schedule_1415[c(5,12,2,1,4,6,8,10,7,9,11,3)]

#order by the date
shl_schedule_1415 <- shl_schedule_1415[order(shl_schedule_1415$game_number),]

#set dates as date format
shl_schedule_1415$date2 <- as.Date(shl_schedule_1415$date2)

#list of all gamedates from the season (all teams)
shl_dates_1415 <- unique(shl_schedule_1415$date2)




# 14/15 Team Schedules ----------------------------------------------------------

#create each team's schedule for the 19/20 schedule

#Brynäs
bif_schedule_1415 <- subset(shl_schedule_1415, grepl("Brynäs IF", shl_schedule_1415$combined_teams))

bif_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "BrynäsBIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> bif_schedule_1415

#Djurgården
dif_schedule_1415 <- subset(shl_schedule_1415, grepl("Djurgårdens IF", shl_schedule_1415$combined_teams))

dif_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "DjurgårdenDIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> dif_schedule_1415

#Frölunda
fhc_schedule_1415 <- subset(shl_schedule_1415, grepl("Frölunda HC", shl_schedule_1415$combined_teams))

fhc_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FrölundaFHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fhc_schedule_1415

#Färjestad
fbk_schedule_1415 <- subset(shl_schedule_1415, grepl("Färjestad BK", shl_schedule_1415$combined_teams))

fbk_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FärjestadFBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fbk_schedule_1415

#HV71
hv_schedule_1415 <- subset(shl_schedule_1415, grepl("HV 71", shl_schedule_1415$combined_teams))

hv_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "HV71HV71") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> hv_schedule_1415

#Leksand
lif_schedule_1415 <- subset(shl_schedule_1415, grepl("Leksands IF", shl_schedule_1415$combined_teams))

lif_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LeksandLIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lif_schedule_1415

#Linköping
lhc_schedule_1415 <- subset(shl_schedule_1415, grepl("Linköping HC", shl_schedule_1415$combined_teams))

lhc_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LinköpingLHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhc_schedule_1415

#Luleå
lhf_schedule_1415 <- subset(shl_schedule_1415, grepl("Luleå HF", shl_schedule_1415$combined_teams))

lhf_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LuleåLHF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhf_schedule_1415

#MODO
modo_schedule_1415 <- subset(shl_schedule_1415, grepl("MODO Hockey", shl_schedule_1415$combined_teams))

modo_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MODOMODO") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> modo_schedule_1415

#Örebro
ohk_schedule_1415 <- subset(shl_schedule_1415, grepl("Örebro HK", shl_schedule_1415$combined_teams))

ohk_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "ÖrebroÖRE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ohk_schedule_1415

#Skellefteå
ske_schedule_1415 <- subset(shl_schedule_1415, grepl("Skellefteå AIK", shl_schedule_1415$combined_teams))

ske_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "SkellefteåSKE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ske_schedule_1415

#Växjö
vax_schedule_1415 <- subset(shl_schedule_1415, grepl("Växjö Lakers", shl_schedule_1415$combined_teams))

vax_schedule_1415 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "VäxjöVLH") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> vax_schedule_1415


master_team_schedule_1415 <- rbind(bif_schedule_1415, dif_schedule_1415, fbk_schedule_1415,
                                   fhc_schedule_1415, hv_schedule_1415, lif_schedule_1415,
                                   lhc_schedule_1415, lhf_schedule_1415, modo_schedule_1415,
                                   ohk_schedule_1415, ske_schedule_1415, vax_schedule_1415)




# 14/15 Stats Scraping ----------------------------------------------------------

#player points scrape
player_points <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/point?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) %>% 
    #make the headers unique
    make.unique()
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #rename column headers
    rename(G_PP = G.1, A_PP = A.1, TP_PP = TP.1,
           G_EV = G.2, A_EV = A.2, TP_EV = TP.2,
           G_SH = G.3, A_SH = A.3, TP_SH = TP.3) %>%
    
    #make columns numeric
    mutate_at(c('Nr', 'GP', 'G', 'A', 'A1', 'A2', 'TP',
                'G_EV', 'A_EV', 'TP_EV',
                'G_PP', 'A_PP', 'TP_PP',
                'G_SH', 'A_SH', 'TP_SH'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1415, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_points_data_1415 <- c()
for (teamID in shl_team_dictionary_1415$shlse_team_id[1:12]) {
  
  for (date1 in as.list(shl_dates_1415[1:80])) {
    
    temp <- player_points(teamID)
    
    player_points_data_1415 <- rbind(player_points_data_1415, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player toi scrape
player_toi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/timeonice?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1415, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #separate TOI columns
    separate(TOI.GP, sep=':', into=c('Minutes', 'Seconds')) %>%
    separate(PPTOI.GP, sep=':', into=c('PPMinutes', 'PPSeconds')) %>%
    separate(ESTOI.GP, sep=':', into=c('ESMinutes', 'ESSeconds')) %>%
    separate(SHTOI.GP, sep=':', into=c('SHMinutes', 'SHSeconds')) %>%
    
    #convert minutes and seconds into numeric
    mutate_at(c('Minutes', 'Seconds','PPMinutes', 'PPSeconds',
                'ESMinutes', 'ESSeconds', 'SHMinutes', 'SHSeconds'), as.numeric) %>%
    
    #create TOI metric that's usable
    mutate(TOI = round((Minutes * 60 + Seconds)/60,1)) %>%
    mutate(PPTOI = round((PPMinutes * 60 + PPSeconds)/60,1)) %>%
    mutate(ESTOI = round((ESMinutes * 60 + ESSeconds)/60,1)) %>%
    mutate(SHTOI = round((SHMinutes * 60 + SHSeconds)/60,1))
  
  return(table)
  
}

#loop through all combinations to get master player toi table
player_toi_data_1415 <- c()
for (teamID in shl_team_dictionary_1415$shlse_team_id[1:12]) {
  
  for (date1 in as.list(shl_dates_1415[1:80])) {
    
    temp <- player_toi(teamID)
    
    player_toi_data_1415 <- rbind(player_toi_data_1415, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

#player ev scrape
player_ev <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/equalstrength?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1415, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #rename columns
    rename(GF = X., GA = X..1, PlusMinus = X...) %>%
    
    mutate_at(c('GF', 'GA','PlusMinus'), as.numeric)
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_ev_data_1415 <- c()
for (teamID in shl_team_dictionary_1415$shlse_team_id[1:12]) {
  
  for (date1 in as.list(shl_dates_1415[1:80])) {
    
    temp <- player_ev(teamID)
    
    player_ev_data_1415 <- rbind(player_ev_data_1415, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#join in friendlier team names
left_join(player_points_data_1415, shl_team_dictionary_1415, by = c("Lag" = "shlse_team_name_double")) -> player_points_data_1415
left_join(player_ev_data_1415, shl_team_dictionary_1415, by = c("Lag" = "shlse_team_name_double")) -> player_ev_data_1415
left_join(player_toi_data_1415, shl_team_dictionary_1415, by = c("Lag" = "shlse_team_name_double")) -> player_toi_data_1415


# CSV Exports -------------------------------------------------------------

#Player EV
write.csv(player_ev_data_1415, file = "player_ev_data_1415.csv")

#Player Points
write.csv(player_points_data_1415, file = "player_points_data_1415csv")

#Player TOI
write.csv(player_toi_data_1415, file = "player_toi_data_1415.csv")

#Unique Names
write.csv(unique(player_toi_data_1415$name), file = "player_names_1415.csv")

# 14/15 Player Card -------------------------------------------------------------

#Full player card
#This function will create the full player card, creating four graphs
#and plotting them all together
player_card_1415 <- function(player_name) {
  
  #TOI
  #create one-off table to expand y axis as needed
  #will use this later to expand the y-axis if needed
  y_toi_axis <- player_toi_data_1415 %>%
    filter(name == player_name)
  
  #pull relevant data from master
  table_toi <- player_toi_data_1415 %>%
    filter(name == player_name) %>%
    select(game_number, ESTOI, PPTOI, SHTOI) %>%
    #rename it for what I want to display in the legend
    rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
  
  #melt so can plot a stacked column (only way to create stacked column?)
  table_toi2 <- melt(table_toi, id.var="game_number") %>%
    rename(GameNumber = game_number, Situation = variable, TOI = value)
  
  #create the order of those stacked components
  #don't fully understand this one - needed to create the order I want the stacking in
  levels(table_toi2$Situation)
  table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
  
  #ggplot the toi visual
  toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),], 
                       aes(x = GameNumber, y = TOI, fill = Situation)) +
    geom_bar(stat = "identity") +
    
    #creating the title for the overall plot, plus the TOI subheader data
    labs(title = paste(player_name, " 14/15 Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
         subtitle = paste("TOI by Game Situation \n",
                          round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
                          round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
                          round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = '')) +
    theme_few() +
    #use the font I loaded in specifically for this visual
    theme(text = element_text(family = "Sweden Sans")) +
    theme(plot.title = element_text(face = "bold")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#FF7F7F", "#FFD17F", "#7F7FFF")) +
    #x axis spans entire 52 game season
    scale_x_continuous(limits = c(0,56), expand = c(0, 0)) +
    #default y axis goes from 0 to 20 minutes
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0))
  
  #but for players with games > 20 minutes, this is where the y axis table comes in
  if(max(y_toi_axis$TOI) > 20){
    toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
                                                  expand = c(0, 0))
  }
  
  #Player point production
  #all same as above
  y_points_axis <- player_points_data_1415 %>%
    filter(name == player_name)
  
  table_points <- player_points_data_1415 %>%
    filter(name == player_name) %>%
    select(game_number, G, A1, A2)
  
  table_points_2 <- melt(table_points, id.var="game_number") %>%
    rename(GameNumber = game_number, PointType = variable, Points = value)
  
  levels(table_points_2$PointType)
  table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
  
  points_visual <- ggplot(table_points_2[order(table_points_2$PointType),], 
                          aes(x = GameNumber, y = Points, fill = PointType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Point Production \n",
                          sum(table_points$G), ' G  -  ',
                          sum(table_points$A1), ' A1  -  ',
                          sum(table_points$A2), ' A2  (',
                          
                          round((sum(y_points_axis$TP) / sum(y_toi_axis$TOI)) * 60, 2),
                          
                          ' P/60)',
                          
                          sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#CCE4CC", "#B2B2FF", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,56), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0))
  
  
  if(max(y_points_axis$TP) > 3){
    points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_points_axis$TP)),
                                                        expand = c(0, 0))
  }
  
  #Goals
  #same methodology as all others
  y_goals_axis <- player_ev_data_1415 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2) %>%
    mutate(GA_abs = abs(GA))
  
  table_goals <- player_ev_data_1415 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2)
  
  table_goals_2 <- melt(table_goals, id.var="game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)
  
  levels(table_goals_2$GoalType)
  table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))
  
  goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),], 
                         aes(x = GameNumber, y = Goals, fill = GoalType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste('ES Goals For and Against \n', 
                          sum(table_goals$GF), ' GF  -  ', 
                          sum(y_goals_axis$GA_abs), ' GA ',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se', ' | ', '@zellenthal_DIF'), sep = '') +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,56), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) 
  
  
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #player_card_visual <- ggarrange(toi_visual, points_visual, points_visual, ncol = 1, nrow = 3)
  
  #return(player_card_visual)
  
  #title <- text_grob(paste(player_name, '18/19 Season', sep = ' ')) 
  
  #using the ggpubr package so that I can align all the ggplots on one visual
  #and so all the x axes align
  g2 <- ggplotGrob(toi_visual)
  #g3 <- ggplotGrob(corsi_visual)
  g4 <- ggplotGrob(goals_visual)
  g5 <- ggplotGrob(points_visual)
  #g <- rbind(g2, g5, g3, g4, size = "first")
  g <- rbind(g2, g5, g4, size = "first")
  #g$widths <- unit.pmax(g2$widths, g5$widths, g3$widths, g4$widths)
  g$widths <- unit.pmax(g2$widths, g5$widths, g4$widths)
  grid.newpage()
  grid.draw(g)
  
  return(g)
  
}







# 15/16 Schedules ---------------------------------------------------------------

#read in the 19/20 schedule, from stats.swehockey.se
shl_schedule_1516 <- read_csv("swehockey_schedule_1516.csv")

#add a row number
shl_schedule_1516 %>%
  mutate(game_number = row_number()) -> shl_schedule_1516

#add more columns to the 19/20 schedule
merge(shl_schedule_1516, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "home_team", by.y = "swehockey_team_name") -> shl_schedule_1516 

shl_schedule_1516 %>%
  rename(home_team_id = shlse_team_id, home_team_name_double = shlse_team_name_double) -> shl_schedule_1516

#same thing but for away team
merge(shl_schedule_1516, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "away_team", by.y = "swehockey_team_name") -> shl_schedule_1516 

shl_schedule_1516 %>%
  rename(away_team_id = shlse_team_id, away_team_name_double = shlse_team_name_double) -> shl_schedule_1516

#create a combined column concatenating the two team ids
shl_schedule_1516 %>%
  mutate(combined_ids = paste(home_team_id, away_team_id, sep = "-")) -> shl_schedule_1516

shl_schedule_1516 %>%
  mutate(combined_team_names_double = paste(home_team_name_double, away_team_name_double, sep = "-")) -> shl_schedule_1516


#add a column that puts the date in a friendly format
shl_schedule_1516 %>%
  mutate(date2 = as.Date(shl_schedule_1516$date, "%m/%d/%Y")) -> shl_schedule_1516

shl_schedule_1516$date2 <- str_replace_all(shl_schedule_1516$date2, '00', '20')

#reorder the columns so I don't lose my mind
shl_schedule_1516 <- shl_schedule_1516[c(5,12,2,1,4,6,8,10,7,9,11,3)]

#order by the date
shl_schedule_1516 <- shl_schedule_1516[order(shl_schedule_1516$game_number),]

#set dates as date format
shl_schedule_1516$date2 <- as.Date(shl_schedule_1516$date2)

#list of all gamedates from the season (all teams)
shl_dates_1516 <- unique(shl_schedule_1516$date2)




# 15/16 Team Schedules ----------------------------------------------------------

#create each team's schedule for the 19/20 schedule

#Brynäs
bif_schedule_1516 <- subset(shl_schedule_1516, grepl("Brynäs IF", shl_schedule_1516$combined_teams))

bif_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "BrynäsBIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> bif_schedule_1516

#Djurgården
dif_schedule_1516 <- subset(shl_schedule_1516, grepl("Djurgårdens IF", shl_schedule_1516$combined_teams))

dif_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "DjurgårdenDIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> dif_schedule_1516

#Frölunda
fhc_schedule_1516 <- subset(shl_schedule_1516, grepl("Frölunda HC", shl_schedule_1516$combined_teams))

fhc_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FrölundaFHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fhc_schedule_1516

#Färjestad
fbk_schedule_1516 <- subset(shl_schedule_1516, grepl("Färjestad BK", shl_schedule_1516$combined_teams))

fbk_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FärjestadFBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fbk_schedule_1516

#HV71
hv_schedule_1516 <- subset(shl_schedule_1516, grepl("HV 71", shl_schedule_1516$combined_teams))

hv_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "HV71HV71") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> hv_schedule_1516

#Karlskrona
khk_schedule_1516 <- subset(shl_schedule_1516, grepl("Karlskrona HK", shl_schedule_1516$combined_teams))

khk_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "KarlskronaKHK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> khk_schedule_1516

#Linköping
lhc_schedule_1516 <- subset(shl_schedule_1516, grepl("Linköping HC", shl_schedule_1516$combined_teams))

lhc_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LinköpingLHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhc_schedule_1516

#Luleå
lhf_schedule_1516 <- subset(shl_schedule_1516, grepl("Luleå HF", shl_schedule_1516$combined_teams))

lhf_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LuleåLHF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhf_schedule_1516

#Malmö
mif_schedule_1516 <- subset(shl_schedule_1516, grepl("Malmö Redhawks", shl_schedule_1516$combined_teams))

mif_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MalmöMIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> mif_schedule_1516

#MODO
modo_schedule_1516 <- subset(shl_schedule_1516, grepl("MODO Hockey", shl_schedule_1516$combined_teams))

modo_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MODOMODO") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> modo_schedule_1516

#Örebro
ohk_schedule_1516 <- subset(shl_schedule_1516, grepl("Örebro HK", shl_schedule_1516$combined_teams))

ohk_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "ÖrebroÖRE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ohk_schedule_1516

#Rögle
rbk_schedule_1516 <- subset(shl_schedule_1516, grepl("Rögle BK", shl_schedule_1516$combined_teams))

rbk_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "RögleRBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> rbk_schedule_1516

#Skellefteå
ske_schedule_1516 <- subset(shl_schedule_1516, grepl("Skellefteå AIK", shl_schedule_1516$combined_teams))

ske_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "SkellefteåSKE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ske_schedule_1516

#Växjö
vax_schedule_1516 <- subset(shl_schedule_1516, grepl("Växjö Lakers", shl_schedule_1516$combined_teams))

vax_schedule_1516 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "VäxjöVLH") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> vax_schedule_1516


master_team_schedule_1516 <- rbind(bif_schedule_1516, dif_schedule_1516, fbk_schedule_1516,
                                   fhc_schedule_1516, hv_schedule_1516, khk_schedule_1516, lhc_schedule_1516, 
                                   lhf_schedule_1516, mif_schedule_1516, modo_schedule_1516, 
                                   ohk_schedule_1516, rbk_schedule_1516, ske_schedule_1516, 
                                   vax_schedule_1516)



# 15/16 Stats Scraping ----------------------------------------------------------

#player points scrape
player_points <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/point?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) %>% 
    #make the headers unique
    make.unique()
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #rename column headers
    rename(G_PP = G.1, A_PP = A.1, TP_PP = TP.1,
           G_EV = G.2, A_EV = A.2, TP_EV = TP.2,
           G_SH = G.3, A_SH = A.3, TP_SH = TP.3) %>%
    
    #make columns numeric
    mutate_at(c('Nr', 'GP', 'G', 'A', 'A1', 'A2', 'TP',
                'G_EV', 'A_EV', 'TP_EV',
                'G_PP', 'A_PP', 'TP_PP',
                'G_SH', 'A_SH', 'TP_SH'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1516, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_points_data_1516 <- c()
for (teamID in shl_team_dictionary_1516$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1516[1:88])) {
    
    temp <- player_points(teamID)
    
    player_points_data_1516 <- rbind(player_points_data_1516, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player toi scrape
player_toi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/timeonice?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1516, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #separate TOI columns
    separate(TOI.GP, sep=':', into=c('Minutes', 'Seconds')) %>%
    separate(PPTOI.GP, sep=':', into=c('PPMinutes', 'PPSeconds')) %>%
    separate(ESTOI.GP, sep=':', into=c('ESMinutes', 'ESSeconds')) %>%
    separate(SHTOI.GP, sep=':', into=c('SHMinutes', 'SHSeconds')) %>%
    
    #convert minutes and seconds into numeric
    mutate_at(c('Minutes', 'Seconds','PPMinutes', 'PPSeconds',
                'ESMinutes', 'ESSeconds', 'SHMinutes', 'SHSeconds'), as.numeric) %>%
    
    #create TOI metric that's usable
    mutate(TOI = round((Minutes * 60 + Seconds)/60,1)) %>%
    mutate(PPTOI = round((PPMinutes * 60 + PPSeconds)/60,1)) %>%
    mutate(ESTOI = round((ESMinutes * 60 + ESSeconds)/60,1)) %>%
    mutate(SHTOI = round((SHMinutes * 60 + SHSeconds)/60,1))
  
  return(table)
  
}

#loop through all combinations to get master player toi table
player_toi_data_1516 <- c()
for (teamID in shl_team_dictionary_1516$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1516[1:88])) {
    
    temp <- player_toi(teamID)
    
    player_toi_data_1516 <- rbind(player_toi_data_1516, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#team corsi scrape
team_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove space in "Lag" column
    #mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1516, by = "team_date") %>%
    
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
}

#loop through all combination to get master team points table
team_corsi_data_1516 <- c()
for (teamID in shl_team_dictionary_1516$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1516[1:88])) {
    
    temp <- team_corsi(teamID)
    
    team_corsi_data_1516 <- rbind(team_corsi_data_1516, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player corsi scrape
player_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1516, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team,
              `CF%`, `CCF%`, `FF%`, `CFF%`, `OIG%`, `OISVS%`, PDO, `TOI/GP`)) %>%
    
    #join in team corsi data
    #need this to get relative points later
    left_join(team_corsi_data_1516, by = "team_date") %>%
    
    #remove duplicated columns from join
    select(-c(Rank.y, Lag.y, GP.y, `CF%`, `CCF%`, `FF%`, `CFF%`,
              `OIG%`, `OISVS%`, PDO, date1.y, Lag2, game_number.y)) %>%
    
    #remove columns that'll break the update process
    #select(-c(shlse_team_name, swehockey_team_name, shl_team_abbreviation, shlse_team_id)) %>%
    
    #rename columns
    rename(Lag = Lag.x, CF = CF.x, CA = CA.x, FF = FF.x, FA = FA.x,
           date2 = date1.x, tmCF = CF.y, tmCA = CA.y, tmFF = FF.y,
           tmFA = FA.y) %>%
    
    #add in % columns
    mutate(`CF%` = round(CF/(CF + CA),3), `FF%` = round(FF/(FF + FA),3),
           CF_off = tmCF - CF, CA_off = tmCA - CA, FF_off = tmFF - FF,
           FA_off = tmFA - FA) %>%
    
    #add in team CF% and FF%
    mutate(`CF%_off` = round(CF_off/(CF_off + CA_off),3),
           `FF%_off` = round(FF_off/(FF_off + FA_off),3))      
  
  return(table)
  
}

#loop through all combination to get master player points table
player_corsi_data_1516 <- c() 
for (teamID in shl_team_dictionary_1516$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1516[1:88])) {
    
    temp <- player_corsi(teamID)
    
    player_corsi_data_1516 <- rbind(player_corsi_data_1516, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player ev scrape
player_ev <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/equalstrength?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1516, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #rename columns
    rename(GF = X., GA = X..1, PlusMinus = X...) %>%
    
    mutate_at(c('GF', 'GA','PlusMinus'), as.numeric)
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_ev_data_1516 <- c()
for (teamID in shl_team_dictionary_1516$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1516[1:88])) {
    
    temp <- player_ev(teamID)
    
    player_ev_data_1516 <- rbind(player_ev_data_1516, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#join in friendlier team names
left_join(player_points_data_1516, shl_team_dictionary_1516, by = c("Lag" = "shlse_team_name_double")) -> player_points_data_1516
left_join(player_ev_data_1516, shl_team_dictionary_1516, by = c("Lag" = "shlse_team_name_double")) -> player_ev_data_1516
left_join(player_corsi_data_1516, shl_team_dictionary_1516, by = c("Lag" = "shlse_team_name_double")) -> player_corsi_data_1516
left_join(player_toi_data_1516, shl_team_dictionary_1516, by = c("Lag" = "shlse_team_name_double")) -> player_toi_data_1516
left_join(team_corsi_data_1516, shl_team_dictionary_1516, by = c("Lag" = "shlse_team_name_double")) -> team_corsi_data_1516


# 15/16 Player Card -------------------------------------------------------------

#Full player card
#This function will create the full player card, creating four graphs
#and plotting them all together
player_card_1516 <- function(player_name) {
  
  #TOI
  #create one-off table to expand y axis as needed
  #will use this later to expand the y-axis if needed
  y_toi_axis <- player_toi_data_1516 %>%
    filter(name == player_name)
  
  #pull relevant data from master
  table_toi <- player_toi_data_1516 %>%
    filter(name == player_name) %>%
    select(game_number, ESTOI, PPTOI, SHTOI) %>%
    #rename it for what I want to display in the legend
    rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
  
  #melt so can plot a stacked column (only way to create stacked column?)
  table_toi2 <- melt(table_toi, id.var="game_number") %>%
    rename(GameNumber = game_number, Situation = variable, TOI = value)
  
  #create the order of those stacked components
  #don't fully understand this one - needed to create the order I want the stacking in
  levels(table_toi2$Situation)
  table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
  
  #ggplot the toi visual
  toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),], 
                       aes(x = GameNumber, y = TOI, fill = Situation)) +
    geom_bar(stat = "identity") +
    
    #creating the title for the overall plot, plus the TOI subheader data
    labs(title = paste(player_name, " 15/16 Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
         subtitle = paste("TOI by Game Situation \n",
                          round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
                          round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
                          round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = '')) +
    theme_few() +
    #use the font I loaded in specifically for this visual
    theme(text = element_text(family = "Sweden Sans")) +
    theme(plot.title = element_text(face = "bold")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#FF7F7F", "#FFD17F", "#7F7FFF")) +
    #x axis spans entire 52 game season
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    #default y axis goes from 0 to 20 minutes
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0))
  
  #but for players with games > 20 minutes, this is where the y axis table comes in
  if(max(y_toi_axis$TOI) > 20){
    toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
                                                  expand = c(0, 0))
  }
  
  #Player point production
  #all same as above
  y_points_axis <- player_points_data_1516 %>%
    filter(name == player_name)
  
  table_points <- player_points_data_1516 %>%
    filter(name == player_name) %>%
    select(game_number, G, A1, A2)
  
  table_points_2 <- melt(table_points, id.var="game_number") %>%
    rename(GameNumber = game_number, PointType = variable, Points = value)
  
  levels(table_points_2$PointType)
  table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
  
  points_visual <- ggplot(table_points_2[order(table_points_2$PointType),], 
                          aes(x = GameNumber, y = Points, fill = PointType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Point Production \n",
                          sum(table_points$G), ' G  -  ',
                          sum(table_points$A1), ' A1  -  ',
                          sum(table_points$A2), ' A2  (',
                          
                          round((sum(y_points_axis$TP) / sum(y_toi_axis$TOI)) * 60, 2),
                          
                          ' P/60)',
                          
                          sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#CCE4CC", "#B2B2FF", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0))
  
  
  if(max(y_points_axis$TP) > 3){
    points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_points_axis$TP)),
                                                        expand = c(0, 0))
  }
  
  #corsi
  y_corsi_axis <- player_corsi_data_1516 %>%
    filter(name == player_name) %>%
    #select(gameNumber, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2) %>%
    mutate(CA_abs = abs(CA))
  
  #storing values to calculate stats for subheader (CF%, relative CF%)
  player_corsi <- round((sum(y_corsi_axis$CF) / (sum(y_corsi_axis$CF) + sum(y_corsi_axis$CA_abs)) *100), 1)
  player_off_corsi <- round((sum(y_corsi_axis$CF_off) / (sum(y_corsi_axis$CF_off) + sum(y_corsi_axis$CA_off)) *100), 1)
  CF_rel <- player_corsi - player_off_corsi
  
  table_corsi <- player_corsi_data_1516 %>%
    filter(name == player_name) %>%
    select(game_number.x, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2)
  
  table_corsi_2 <- melt(table_corsi, id.var="game_number.x") %>%
    rename(GameNumber = game_number.x, ShotType = variable, Shots = value)
  
  levels(table_corsi_2$Type)
  table_corsi_2$ShotType <- factor(table_corsi_2$ShotType, levels = rev(levels(table_corsi_2$ShotType)))
  
  corsi_visual <- ggplot(table_corsi_2[order(table_corsi_2$ShotType),], 
                         aes(x = GameNumber, y = Shots, fill = ShotType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Corsi For and Against \n",  
                          round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1), 
                          ' CF%', 
                          
                          ' (', round(CF_rel, 1),' rel)', sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    #flip the order of the legend so CF is on top and CA is on the bottom
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-20, 20), expand = c(0, 0)) 
  
  
  if(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) > 20){
    corsi_visual <- corsi_visual + scale_y_continuous(limits=c(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) * -1, max(y_corsi_axis$CF,y_corsi_axis$CA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #Goals
  #same methodology as all others
  y_goals_axis <- player_ev_data_1516 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2) %>%
    mutate(GA_abs = abs(GA))
  
  table_goals <- player_ev_data_1516 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2)
  
  table_goals_2 <- melt(table_goals, id.var="game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)
  
  levels(table_goals_2$GoalType)
  table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))
  
  goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),], 
                         aes(x = GameNumber, y = Goals, fill = GoalType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste('ES Goals For and Against \n', 
                          sum(table_goals$GF), ' GF  -  ', 
                          sum(y_goals_axis$GA_abs), ' GA ',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se', ' | ', '@zellenthal_DIF'), sep = '') +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) 
  
  
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #player_card_visual <- ggarrange(toi_visual, points_visual, points_visual, ncol = 1, nrow = 3)
  
  #return(player_card_visual)
  
  #title <- text_grob(paste(player_name, '18/19 Season', sep = ' ')) 
  
  #using the ggpubr package so that I can align all the ggplots on one visual
  #and so all the x axes align
  g2 <- ggplotGrob(toi_visual)
  g3 <- ggplotGrob(corsi_visual)
  g4 <- ggplotGrob(goals_visual)
  g5 <- ggplotGrob(points_visual)
  g <- rbind(g2, g5, g3, g4, size = "first")
  g$widths <- unit.pmax(g2$widths, g5$widths, g3$widths, g4$widths)
  grid.newpage()
  grid.draw(g)
  
  return(g)
  
}

# CSV Exports -------------------------------------------------------------

#Player Corsi
write.csv(player_corsi_data_1516, file = "player_corsi_data_1516.csv")

#Player EV
write.csv(player_ev_data_1516, file = "player_ev_data_1516.csv")

#Player Points
write.csv(player_points_data_1516, file = "player_points_data_1516.csv")

#Player TOI
write.csv(player_toi_data_1516, file = "player_toi_data_1516.csv")

#Unique Names
write.csv(unique(player_toi_data_1516$name), file = "player_names_1516.csv")











# 16/17 Schedules ---------------------------------------------------------------

#read in the 19/20 schedule, from stats.swehockey.se
shl_schedule_1617 <- read_csv("swehockey_schedule_1617.csv")

#add a row number
shl_schedule_1617 %>%
  mutate(game_number = row_number()) -> shl_schedule_1617

#add more columns to the 19/20 schedule
merge(shl_schedule_1617, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "home_team", by.y = "swehockey_team_name") -> shl_schedule_1617 

shl_schedule_1617 %>%
  rename(home_team_id = shlse_team_id, home_team_name_double = shlse_team_name_double) -> shl_schedule_1617

#same thing but for away team
merge(shl_schedule_1617, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "away_team", by.y = "swehockey_team_name") -> shl_schedule_1617 

shl_schedule_1617 %>%
  rename(away_team_id = shlse_team_id, away_team_name_double = shlse_team_name_double) -> shl_schedule_1617

#create a combined column concatenating the two team ids
shl_schedule_1617 %>%
  mutate(combined_ids = paste(home_team_id, away_team_id, sep = "-")) -> shl_schedule_1617

shl_schedule_1617 %>%
  mutate(combined_team_names_double = paste(home_team_name_double, away_team_name_double, sep = "-")) -> shl_schedule_1617


#add a column that puts the date in a friendly format
shl_schedule_1617 %>%
  mutate(date2 = as.Date(shl_schedule_1617$date, "%m/%d/%Y")) -> shl_schedule_1617

shl_schedule_1617$date2 <- str_replace_all(shl_schedule_1617$date2, '00', '20')

#reorder the columns so I don't lose my mind
shl_schedule_1617 <- shl_schedule_1617[c(5,12,2,1,4,6,8,10,7,9,11,3)]

#order by the date
shl_schedule_1617 <- shl_schedule_1617[order(shl_schedule_1617$game_number),]

#set dates as date format
shl_schedule_1617$date2 <- as.Date(shl_schedule_1617$date2)

#list of all gamedates from the season (all teams)
shl_dates_1617 <- unique(shl_schedule_1617$date2)




# 16/17 Team Schedules ----------------------------------------------------------

#create each team's schedule for the 19/20 schedule

#Brynäs
bif_schedule_1617 <- subset(shl_schedule_1617, grepl("Brynäs IF", shl_schedule_1617$combined_teams))

bif_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "BrynäsBIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> bif_schedule_1617

#Djurgården
dif_schedule_1617 <- subset(shl_schedule_1617, grepl("Djurgårdens IF", shl_schedule_1617$combined_teams))

dif_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "DjurgårdenDIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> dif_schedule_1617

#Frölunda
fhc_schedule_1617 <- subset(shl_schedule_1617, grepl("Frölunda HC", shl_schedule_1617$combined_teams))

fhc_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FrölundaFHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fhc_schedule_1617

#Färjestad
fbk_schedule_1617 <- subset(shl_schedule_1617, grepl("Färjestad BK", shl_schedule_1617$combined_teams))

fbk_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FärjestadFBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fbk_schedule_1617

#HV71
hv_schedule_1617 <- subset(shl_schedule_1617, grepl("HV 71", shl_schedule_1617$combined_teams))

hv_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "HV71HV71") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> hv_schedule_1617

#Karlskrona
khk_schedule_1617 <- subset(shl_schedule_1617, grepl("Karlskrona HK", shl_schedule_1617$combined_teams))

khk_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "KarlskronaKHK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> khk_schedule_1617

#Leksand
lif_schedule_1617 <- subset(shl_schedule_1617, grepl("Leksands IF", shl_schedule_1617$combined_teams))

lif_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LeksandLIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lif_schedule_1617

#Linköping
lhc_schedule_1617 <- subset(shl_schedule_1617, grepl("Linköping HC", shl_schedule_1617$combined_teams))

lhc_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LinköpingLHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhc_schedule_1617

#Luleå
lhf_schedule_1617 <- subset(shl_schedule_1617, grepl("Luleå HF", shl_schedule_1617$combined_teams))

lhf_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LuleåLHF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhf_schedule_1617

#Malmö
mif_schedule_1617 <- subset(shl_schedule_1617, grepl("Malmö Redhawks", shl_schedule_1617$combined_teams))

mif_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MalmöMIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> mif_schedule_1617

#Örebro
ohk_schedule_1617 <- subset(shl_schedule_1617, grepl("Örebro HK", shl_schedule_1617$combined_teams))

ohk_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "ÖrebroÖRE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ohk_schedule_1617

#Rögle
rbk_schedule_1617 <- subset(shl_schedule_1617, grepl("Rögle BK", shl_schedule_1617$combined_teams))

rbk_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "RögleRBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> rbk_schedule_1617

#Skellefteå
ske_schedule_1617 <- subset(shl_schedule_1617, grepl("Skellefteå AIK", shl_schedule_1617$combined_teams))

ske_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "SkellefteåSKE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ske_schedule_1617

#Växjö
vax_schedule_1617 <- subset(shl_schedule_1617, grepl("Växjö Lakers", shl_schedule_1617$combined_teams))

vax_schedule_1617 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "VäxjöVLH") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> vax_schedule_1617


master_team_schedule_1617 <- rbind(bif_schedule_1617, dif_schedule_1617, fbk_schedule_1617,
                                   fhc_schedule_1617, hv_schedule_1617, khk_schedule_1617, 
                                   lif_schedule_1617, lhc_schedule_1617, 
                                   lhf_schedule_1617, mif_schedule_1617, 
                                   ohk_schedule_1617, rbk_schedule_1617, ske_schedule_1617, 
                                   vax_schedule_1617)



# 16/17 Stats Scraping ----------------------------------------------------------

#player points scrape
player_points <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/point?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) %>% 
    #make the headers unique
    make.unique()
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #rename column headers
    rename(G_PP = G.1, A_PP = A.1, TP_PP = TP.1,
           G_EV = G.2, A_EV = A.2, TP_EV = TP.2,
           G_SH = G.3, A_SH = A.3, TP_SH = TP.3) %>%
    
    #make columns numeric
    mutate_at(c('Nr', 'GP', 'G', 'A', 'A1', 'A2', 'TP',
                'G_EV', 'A_EV', 'TP_EV',
                'G_PP', 'A_PP', 'TP_PP',
                'G_SH', 'A_SH', 'TP_SH'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1617, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_points_data_1617 <- c()
for (teamID in shl_team_dictionary_1617$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1617[1:96])) {
    
    temp <- player_points(teamID)
    
    player_points_data_1617 <- rbind(player_points_data_1617, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player toi scrape
player_toi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/timeonice?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1617, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #separate TOI columns
    separate(TOI.GP, sep=':', into=c('Minutes', 'Seconds')) %>%
    separate(PPTOI.GP, sep=':', into=c('PPMinutes', 'PPSeconds')) %>%
    separate(ESTOI.GP, sep=':', into=c('ESMinutes', 'ESSeconds')) %>%
    separate(SHTOI.GP, sep=':', into=c('SHMinutes', 'SHSeconds')) %>%
    
    #convert minutes and seconds into numeric
    mutate_at(c('Minutes', 'Seconds','PPMinutes', 'PPSeconds',
                'ESMinutes', 'ESSeconds', 'SHMinutes', 'SHSeconds'), as.numeric) %>%
    
    #create TOI metric that's usable
    mutate(TOI = round((Minutes * 60 + Seconds)/60,1)) %>%
    mutate(PPTOI = round((PPMinutes * 60 + PPSeconds)/60,1)) %>%
    mutate(ESTOI = round((ESMinutes * 60 + ESSeconds)/60,1)) %>%
    mutate(SHTOI = round((SHMinutes * 60 + SHSeconds)/60,1))
  
  return(table)
  
}

#loop through all combinations to get master player toi table
player_toi_data_1617 <- c()
for (teamID in shl_team_dictionary_1617$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1617[1:96])) {
    
    temp <- player_toi(teamID)
    
    player_toi_data_1617 <- rbind(player_toi_data_1617, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#team corsi scrape
team_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove space in "Lag" column
    #mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1617, by = "team_date") %>%
    
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
}

#loop through all combination to get master team points table
team_corsi_data_1617 <- c()
for (teamID in shl_team_dictionary_1617$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1617[1:96])) {
    
    temp <- team_corsi(teamID)
    
    team_corsi_data_1617 <- rbind(team_corsi_data_1617, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player corsi scrape
player_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1617, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team,
              `CF%`, `CCF%`, `FF%`, `CFF%`, `OIG%`, `OISVS%`, PDO, `TOI/GP`)) %>%
    
    #join in team corsi data
    #need this to get relative points later
    left_join(team_corsi_data_1617, by = "team_date") %>%
    
    #remove duplicated columns from join
    select(-c(Rank.y, Lag.y, GP.y, `CF%`, `CCF%`, `FF%`, `CFF%`,
              `OIG%`, `OISVS%`, PDO, date1.y, Lag2, game_number.y)) %>%
    
    #remove columns that'll break the update process
    #select(-c(shlse_team_name, swehockey_team_name, shl_team_abbreviation, shlse_team_id)) %>%
    
    #rename columns
    rename(Lag = Lag.x, CF = CF.x, CA = CA.x, FF = FF.x, FA = FA.x,
           date2 = date1.x, tmCF = CF.y, tmCA = CA.y, tmFF = FF.y,
           tmFA = FA.y) %>%
    
    #add in % columns
    mutate(`CF%` = round(CF/(CF + CA),3), `FF%` = round(FF/(FF + FA),3),
           CF_off = tmCF - CF, CA_off = tmCA - CA, FF_off = tmFF - FF,
           FA_off = tmFA - FA) %>%
    
    #add in team CF% and FF%
    mutate(`CF%_off` = round(CF_off/(CF_off + CA_off),3),
           `FF%_off` = round(FF_off/(FF_off + FA_off),3))      
  
  return(table)
  
}

#loop through all combination to get master player points table
player_corsi_data_1617 <- c() 
for (teamID in shl_team_dictionary_1617$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1617[1:96])) {
    
    temp <- player_corsi(teamID)
    
    player_corsi_data_1617 <- rbind(player_corsi_data_1617, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player ev scrape
player_ev <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/equalstrength?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1617, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #rename columns
    rename(GF = X., GA = X..1, PlusMinus = X...) %>%
    
    mutate_at(c('GF', 'GA','PlusMinus'), as.numeric)
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_ev_data_1617 <- c()
for (teamID in shl_team_dictionary_1617$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1617[1:96])) {
    
    temp <- player_ev(teamID)
    
    player_ev_data_1617 <- rbind(player_ev_data_1617, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#join in friendlier team names
left_join(player_points_data_1617, shl_team_dictionary_1617, by = c("Lag" = "shlse_team_name_double")) -> player_points_data_1617
left_join(player_ev_data_1617, shl_team_dictionary_1617, by = c("Lag" = "shlse_team_name_double")) -> player_ev_data_1617
left_join(player_corsi_data_1617, shl_team_dictionary_1617, by = c("Lag" = "shlse_team_name_double")) -> player_corsi_data_1617
left_join(player_toi_data_1617, shl_team_dictionary_1617, by = c("Lag" = "shlse_team_name_double")) -> player_toi_data_1617
left_join(team_corsi_data_1617, shl_team_dictionary_1617, by = c("Lag" = "shlse_team_name_double")) -> team_corsi_data_1617


# 16/17 Player Card -------------------------------------------------------------

#Full player card
#This function will create the full player card, creating four graphs
#and plotting them all together
player_card_1617 <- function(player_name) {
  
  #TOI
  #create one-off table to expand y axis as needed
  #will use this later to expand the y-axis if needed
  y_toi_axis <- player_toi_data_1617 %>%
    filter(name == player_name)
  
  #pull relevant data from master
  table_toi <- player_toi_data_1617 %>%
    filter(name == player_name) %>%
    select(game_number, ESTOI, PPTOI, SHTOI) %>%
    #rename it for what I want to display in the legend
    rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
  
  #melt so can plot a stacked column (only way to create stacked column?)
  table_toi2 <- melt(table_toi, id.var="game_number") %>%
    rename(GameNumber = game_number, Situation = variable, TOI = value)
  
  #create the order of those stacked components
  #don't fully understand this one - needed to create the order I want the stacking in
  levels(table_toi2$Situation)
  table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
  
  #ggplot the toi visual
  toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),], 
                       aes(x = GameNumber, y = TOI, fill = Situation)) +
    geom_bar(stat = "identity") +
    
    #creating the title for the overall plot, plus the TOI subheader data
    labs(title = paste(player_name, " 16/17 Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
         subtitle = paste("TOI by Game Situation \n",
                          round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
                          round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
                          round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = '')) +
    theme_few() +
    #use the font I loaded in specifically for this visual
    theme(text = element_text(family = "Sweden Sans")) +
    theme(plot.title = element_text(face = "bold")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#FF7F7F", "#FFD17F", "#7F7FFF")) +
    #x axis spans entire 52 game season
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    #default y axis goes from 0 to 20 minutes
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0))
  
  #but for players with games > 20 minutes, this is where the y axis table comes in
  if(max(y_toi_axis$TOI) > 20){
    toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
                                                  expand = c(0, 0))
  }
  
  #Player point production
  #all same as above
  y_points_axis <- player_points_data_1617 %>%
    filter(name == player_name)
  
  table_points <- player_points_data_1617 %>%
    filter(name == player_name) %>%
    select(game_number, G, A1, A2)
  
  table_points_2 <- melt(table_points, id.var="game_number") %>%
    rename(GameNumber = game_number, PointType = variable, Points = value)
  
  levels(table_points_2$PointType)
  table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
  
  points_visual <- ggplot(table_points_2[order(table_points_2$PointType),], 
                          aes(x = GameNumber, y = Points, fill = PointType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Point Production \n",
                          sum(table_points$G), ' G  -  ',
                          sum(table_points$A1), ' A1  -  ',
                          sum(table_points$A2), ' A2  (',
                          
                          round((sum(y_points_axis$TP) / sum(y_toi_axis$TOI)) * 60, 2),
                          
                          ' P/60)',
                          
                          sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#CCE4CC", "#B2B2FF", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0))
  
  
  if(max(y_points_axis$TP) > 3){
    points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_points_axis$TP)),
                                                        expand = c(0, 0))
  }
  
  #corsi
  y_corsi_axis <- player_corsi_data_1617 %>%
    filter(name == player_name) %>%
    #select(gameNumber, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2) %>%
    mutate(CA_abs = abs(CA))
  
  #storing values to calculate stats for subheader (CF%, relative CF%)
  player_corsi <- round((sum(y_corsi_axis$CF) / (sum(y_corsi_axis$CF) + sum(y_corsi_axis$CA_abs)) *100), 1)
  player_off_corsi <- round((sum(y_corsi_axis$CF_off) / (sum(y_corsi_axis$CF_off) + sum(y_corsi_axis$CA_off)) *100), 1)
  CF_rel <- player_corsi - player_off_corsi
  
  table_corsi <- player_corsi_data_1617 %>%
    filter(name == player_name) %>%
    select(game_number.x, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2)
  
  table_corsi_2 <- melt(table_corsi, id.var="game_number.x") %>%
    rename(GameNumber = game_number.x, ShotType = variable, Shots = value)
  
  levels(table_corsi_2$Type)
  table_corsi_2$ShotType <- factor(table_corsi_2$ShotType, levels = rev(levels(table_corsi_2$ShotType)))
  
  corsi_visual <- ggplot(table_corsi_2[order(table_corsi_2$ShotType),], 
                         aes(x = GameNumber, y = Shots, fill = ShotType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Corsi For and Against \n",  
                          round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1), 
                          ' CF%', 
                          
                          ' (', round(CF_rel, 1),' rel)', sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    #flip the order of the legend so CF is on top and CA is on the bottom
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-20, 20), expand = c(0, 0)) 
  
  
  if(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) > 20){
    corsi_visual <- corsi_visual + scale_y_continuous(limits=c(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) * -1, max(y_corsi_axis$CF,y_corsi_axis$CA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #Goals
  #same methodology as all others
  y_goals_axis <- player_ev_data_1617 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2) %>%
    mutate(GA_abs = abs(GA))
  
  table_goals <- player_ev_data_1617 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2)
  
  table_goals_2 <- melt(table_goals, id.var="game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)
  
  levels(table_goals_2$GoalType)
  table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))
  
  goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),], 
                         aes(x = GameNumber, y = Goals, fill = GoalType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste('ES Goals For and Against \n', 
                          sum(table_goals$GF), ' GF  -  ', 
                          sum(y_goals_axis$GA_abs), ' GA ',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se', ' | ', '@zellenthal_DIF'), sep = '') +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) 
  
  
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #player_card_visual <- ggarrange(toi_visual, points_visual, points_visual, ncol = 1, nrow = 3)
  
  #return(player_card_visual)
  
  #title <- text_grob(paste(player_name, '18/19 Season', sep = ' ')) 
  
  #using the ggpubr package so that I can align all the ggplots on one visual
  #and so all the x axes align
  g2 <- ggplotGrob(toi_visual)
  g3 <- ggplotGrob(corsi_visual)
  g4 <- ggplotGrob(goals_visual)
  g5 <- ggplotGrob(points_visual)
  g <- rbind(g2, g5, g3, g4, size = "first")
  g$widths <- unit.pmax(g2$widths, g5$widths, g3$widths, g4$widths)
  grid.newpage()
  grid.draw(g)
  
  return(g)
  
}

# CSV Exports -------------------------------------------------------------

#Player Corsi
write.csv(player_corsi_data_1617, file = "player_corsi_data_1617.csv")

#Player EV
write.csv(player_ev_data_1617, file = "player_ev_data_1617.csv")

#Player Points
write.csv(player_points_data_1617, file = "player_points_data_1617.csv")

#Player TOI
write.csv(player_toi_data_1617, file = "player_toi_data_1617.csv")

#Unique Names
write.csv(unique(player_toi_data_1617$name), file = "player_names_1617.csv")











# 17/18 Schedules ---------------------------------------------------------------

#read in the 19/20 schedule, from stats.swehockey.se
shl_schedule_1718 <- read_csv("swehockey_schedule_1718.csv")

#add a row number
shl_schedule_1718 %>%
  mutate(game_number = row_number()) -> shl_schedule_1718

#add more columns to the 19/20 schedule
merge(shl_schedule_1718, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "home_team", by.y = "swehockey_team_name") -> shl_schedule_1718 

shl_schedule_1718 %>%
  rename(home_team_id = shlse_team_id, home_team_name_double = shlse_team_name_double) -> shl_schedule_1718

#same thing but for away team
merge(shl_schedule_1718, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")], by.x = "away_team", by.y = "swehockey_team_name") -> shl_schedule_1718 

shl_schedule_1718 %>%
  rename(away_team_id = shlse_team_id, away_team_name_double = shlse_team_name_double) -> shl_schedule_1718

#create a combined column concatenating the two team ids
shl_schedule_1718 %>%
  mutate(combined_ids = paste(home_team_id, away_team_id, sep = "-")) -> shl_schedule_1718

shl_schedule_1718 %>%
  mutate(combined_team_names_double = paste(home_team_name_double, away_team_name_double, sep = "-")) -> shl_schedule_1718


#add a column that puts the date in a friendly format
shl_schedule_1718 %>%
  mutate(date2 = as.Date(shl_schedule_1718$date, "%m/%d/%Y")) -> shl_schedule_1718

shl_schedule_1718$date2 <- str_replace_all(shl_schedule_1718$date2, '00', '20')

#reorder the columns so I don't lose my mind
shl_schedule_1718 <- shl_schedule_1718[c(5,12,2,1,4,6,8,10,7,9,11,3)]

#order by the date
shl_schedule_1718 <- shl_schedule_1718[order(shl_schedule_1718$game_number),]

#set dates as date format
shl_schedule_1718$date2 <- as.Date(shl_schedule_1718$date2)

#list of all gamedates from the season (all teams)
shl_dates_1718 <- unique(shl_schedule_1718$date2)





# 17/18 Team Schedules ----------------------------------------------------------

#create each team's schedule for the 19/20 schedule

#Brynäs
bif_schedule_1718 <- subset(shl_schedule_1718, grepl("Brynäs IF", shl_schedule_1718$combined_teams))

bif_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "BrynäsBIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> bif_schedule_1718

#Djurgården
dif_schedule_1718 <- subset(shl_schedule_1718, grepl("Djurgårdens IF", shl_schedule_1718$combined_teams))

dif_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "DjurgårdenDIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> dif_schedule_1718

#Frölunda
fhc_schedule_1718 <- subset(shl_schedule_1718, grepl("Frölunda HC", shl_schedule_1718$combined_teams))

fhc_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FrölundaFHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fhc_schedule_1718

#Färjestad
fbk_schedule_1718 <- subset(shl_schedule_1718, grepl("Färjestad BK", shl_schedule_1718$combined_teams))

fbk_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "FärjestadFBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> fbk_schedule_1718

#HV71
hv_schedule_1718 <- subset(shl_schedule_1718, grepl("HV 71", shl_schedule_1718$combined_teams))

hv_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "HV71HV71") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> hv_schedule_1718

#Karlskrona
khk_schedule_1718 <- subset(shl_schedule_1718, grepl("Karlskrona HK", shl_schedule_1718$combined_teams))

khk_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "KarlskronaKHK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> khk_schedule_1718

#Linköping
lhc_schedule_1718 <- subset(shl_schedule_1718, grepl("Linköping HC", shl_schedule_1718$combined_teams))

lhc_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LinköpingLHC") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhc_schedule_1718

#Luleå
lhf_schedule_1718 <- subset(shl_schedule_1718, grepl("Luleå HF", shl_schedule_1718$combined_teams))

lhf_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "LuleåLHF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> lhf_schedule_1718

#Malmö
mif_schedule_1718 <- subset(shl_schedule_1718, grepl("Malmö Redhawks", shl_schedule_1718$combined_teams))

mif_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MalmöMIF") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> mif_schedule_1718

#Mora
mik_schedule_1718 <- subset(shl_schedule_1718, grepl("Mora IK", shl_schedule_1718$combined_teams))

mik_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "MoraMIK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> mik_schedule_1718

#Örebro
ohk_schedule_1718 <- subset(shl_schedule_1718, grepl("Örebro HK", shl_schedule_1718$combined_teams))

ohk_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "ÖrebroÖRE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ohk_schedule_1718

#Rögle
rbk_schedule_1718 <- subset(shl_schedule_1718, grepl("Rögle BK", shl_schedule_1718$combined_teams))

rbk_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "RögleRBK") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> rbk_schedule_1718

#Skellefteå
ske_schedule_1718 <- subset(shl_schedule_1718, grepl("Skellefteå AIK", shl_schedule_1718$combined_teams))

ske_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "SkellefteåSKE") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> ske_schedule_1718

#Växjö
vax_schedule_1718 <- subset(shl_schedule_1718, grepl("Växjö Lakers", shl_schedule_1718$combined_teams))

vax_schedule_1718 %>%
  mutate(game_number = row_number()) %>%
  mutate(team = "VäxjöVLH") %>%
  mutate(team_date = paste(team, date2, sep = '')) -> vax_schedule_1718


master_team_schedule_1718 <- rbind(bif_schedule_1718, dif_schedule_1718, fbk_schedule_1718,
                                   fhc_schedule_1718, hv_schedule_1718, khk_schedule_1718, 
                                   lhc_schedule_1718, lhf_schedule_1718, mif_schedule_1718,
                                   mik_schedule_1718,
                                   ohk_schedule_1718, rbk_schedule_1718, ske_schedule_1718, 
                                   vax_schedule_1718)



# 17/18 Stats Scraping ----------------------------------------------------------

#player points scrape
player_points <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/point?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) %>% 
    #make the headers unique
    make.unique()
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #rename column headers
    rename(G_PP = G.1, A_PP = A.1, TP_PP = TP.1,
           G_EV = G.2, A_EV = A.2, TP_EV = TP.2,
           G_SH = G.3, A_SH = A.3, TP_SH = TP.3) %>%
    
    #make columns numeric
    mutate_at(c('Nr', 'GP', 'G', 'A', 'A1', 'A2', 'TP',
                'G_EV', 'A_EV', 'TP_EV',
                'G_PP', 'A_PP', 'TP_PP',
                'G_SH', 'A_SH', 'TP_SH'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1718, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_points_data_1718 <- c()
for (teamID in shl_team_dictionary_1718$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1718[1:96])) {
    
    temp <- player_points(teamID)
    
    player_points_data_1718 <- rbind(player_points_data_1718, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player toi scrape
player_toi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/timeonice?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1718, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #separate TOI columns
    separate(TOI.GP, sep=':', into=c('Minutes', 'Seconds')) %>%
    separate(PPTOI.GP, sep=':', into=c('PPMinutes', 'PPSeconds')) %>%
    separate(ESTOI.GP, sep=':', into=c('ESMinutes', 'ESSeconds')) %>%
    separate(SHTOI.GP, sep=':', into=c('SHMinutes', 'SHSeconds')) %>%
    
    #convert minutes and seconds into numeric
    mutate_at(c('Minutes', 'Seconds','PPMinutes', 'PPSeconds',
                'ESMinutes', 'ESSeconds', 'SHMinutes', 'SHSeconds'), as.numeric) %>%
    
    #create TOI metric that's usable
    mutate(TOI = round((Minutes * 60 + Seconds)/60,1)) %>%
    mutate(PPTOI = round((PPMinutes * 60 + PPSeconds)/60,1)) %>%
    mutate(ESTOI = round((ESMinutes * 60 + ESSeconds)/60,1)) %>%
    mutate(SHTOI = round((SHMinutes * 60 + SHSeconds)/60,1))
  
  return(table)
  
}

#loop through all combinations to get master player toi table
player_toi_data_1718 <- c()
for (teamID in shl_team_dictionary_1718$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1718[1:96])) {
    
    temp <- player_toi(teamID)
    
    player_toi_data_1718 <- rbind(player_toi_data_1718, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#team corsi scrape
team_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove space in "Lag" column
    #mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1718, by = "team_date") %>%
    
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
}

#loop through all combination to get master team points table
team_corsi_data_1718 <- c()
for (teamID in shl_team_dictionary_1718$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1718[1:96])) {
    
    temp <- team_corsi(teamID)
    
    team_corsi_data_1718 <- rbind(team_corsi_data_1718, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player corsi scrape
player_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1718, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team,
              `CF%`, `CCF%`, `FF%`, `CFF%`, `OIG%`, `OISVS%`, PDO, `TOI/GP`)) %>%
    
    #join in team corsi data
    #need this to get relative points later
    left_join(team_corsi_data_1718, by = "team_date") %>%
    
    #remove duplicated columns from join
    select(-c(Rank.y, Lag.y, GP.y, `CF%`, `CCF%`, `FF%`, `CFF%`,
              `OIG%`, `OISVS%`, PDO, date1.y, Lag2, game_number.y)) %>%
    
    #remove columns that'll break the update process
    #select(-c(shlse_team_name, swehockey_team_name, shl_team_abbreviation, shlse_team_id)) %>%
    
    #rename columns
    rename(Lag = Lag.x, CF = CF.x, CA = CA.x, FF = FF.x, FA = FA.x,
           date2 = date1.x, tmCF = CF.y, tmCA = CA.y, tmFF = FF.y,
           tmFA = FA.y) %>%
    
    #add in % columns
    mutate(`CF%` = round(CF/(CF + CA),3), `FF%` = round(FF/(FF + FA),3),
           CF_off = tmCF - CF, CA_off = tmCA - CA, FF_off = tmFF - FF,
           FA_off = tmFA - FA) %>%
    
    #add in team CF% and FF%
    mutate(`CF%_off` = round(CF_off/(CF_off + CA_off),3),
           `FF%_off` = round(FF_off/(FF_off + FA_off),3))      
  
  return(table)
  
}

#loop through all combination to get master player points table
player_corsi_data_1718 <- c() 
for (teamID in shl_team_dictionary_1718$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1718[1:96])) {
    
    temp <- player_corsi(teamID)
    
    player_corsi_data_1718 <- rbind(player_corsi_data_1718, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#player ev scrape
player_ev <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/equalstrength?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1718, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #rename columns
    rename(GF = X., GA = X..1, PlusMinus = X...) %>%
    
    mutate_at(c('GF', 'GA','PlusMinus'), as.numeric)
  
  return(table)
  
}

#loop through all combinations to get master player points table
player_ev_data_1718 <- c()
for (teamID in shl_team_dictionary_1718$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1718[1:96])) {
    
    temp <- player_ev(teamID)
    
    player_ev_data_1718 <- rbind(player_ev_data_1718, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


#join in friendlier team names
left_join(player_points_data_1718, shl_team_dictionary_1718, by = c("Lag" = "shlse_team_name_double")) -> player_points_data_1718
left_join(player_ev_data_1718, shl_team_dictionary_1718, by = c("Lag" = "shlse_team_name_double")) -> player_ev_data_1718
left_join(player_corsi_data_1718, shl_team_dictionary_1718, by = c("Lag" = "shlse_team_name_double")) -> player_corsi_data_1718
left_join(player_toi_data_1718, shl_team_dictionary_1718, by = c("Lag" = "shlse_team_name_double")) -> player_toi_data_1718
left_join(team_corsi_data_1718, shl_team_dictionary_1718, by = c("Lag" = "shlse_team_name_double")) -> team_corsi_data_1718


# 17/18 Player Card -------------------------------------------------------------

#Full player card
#This function will create the full player card, creating four graphs
#and plotting them all together
player_card_1718 <- function(player_name) {
  
  #TOI
  #create one-off table to expand y axis as needed
  #will use this later to expand the y-axis if needed
  y_toi_axis <- player_toi_data_1718 %>%
    filter(name == player_name)
  
  #pull relevant data from master
  table_toi <- player_toi_data_1718 %>%
    filter(name == player_name) %>%
    select(game_number, ESTOI, PPTOI, SHTOI) %>%
    #rename it for what I want to display in the legend
    rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
  
  #melt so can plot a stacked column (only way to create stacked column?)
  table_toi2 <- melt(table_toi, id.var="game_number") %>%
    rename(GameNumber = game_number, Situation = variable, TOI = value)
  
  #create the order of those stacked components
  #don't fully understand this one - needed to create the order I want the stacking in
  levels(table_toi2$Situation)
  table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
  
  #ggplot the toi visual
  toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),], 
                       aes(x = GameNumber, y = TOI, fill = Situation)) +
    geom_bar(stat = "identity") +
    
    #creating the title for the overall plot, plus the TOI subheader data
    labs(title = paste(player_name, " 17/18 Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
         subtitle = paste("TOI by Game Situation \n",
                          round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
                          round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
                          round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = '')) +
    theme_few() +
    #use the font I loaded in specifically for this visual
    theme(text = element_text(family = "Sweden Sans")) +
    theme(plot.title = element_text(face = "bold")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#FF7F7F", "#FFD17F", "#7F7FFF")) +
    #x axis spans entire 52 game season
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    #default y axis goes from 0 to 20 minutes
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0))
  
  #but for players with games > 20 minutes, this is where the y axis table comes in
  if(max(y_toi_axis$TOI) > 20){
    toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
                                                  expand = c(0, 0))
  }
  
  #Player point production
  #all same as above
  y_points_axis <- player_points_data_1718 %>%
    filter(name == player_name)
  
  table_points <- player_points_data_1718 %>%
    filter(name == player_name) %>%
    select(game_number, G, A1, A2)
  
  table_points_2 <- melt(table_points, id.var="game_number") %>%
    rename(GameNumber = game_number, PointType = variable, Points = value)
  
  levels(table_points_2$PointType)
  table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
  
  points_visual <- ggplot(table_points_2[order(table_points_2$PointType),], 
                          aes(x = GameNumber, y = Points, fill = PointType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Point Production \n",
                          sum(table_points$G), ' G  -  ',
                          sum(table_points$A1), ' A1  -  ',
                          sum(table_points$A2), ' A2  (',
                          
                          round((sum(y_points_axis$TP) / sum(y_toi_axis$TOI)) * 60, 2),
                          
                          ' P/60)',
                          
                          sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#CCE4CC", "#B2B2FF", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0))
  
  
  if(max(y_points_axis$TP) > 3){
    points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_points_axis$TP)),
                                                        expand = c(0, 0))
  }
  
  #corsi
  y_corsi_axis <- player_corsi_data_1718 %>%
    filter(name == player_name) %>%
    #select(gameNumber, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2) %>%
    mutate(CA_abs = abs(CA))
  
  #storing values to calculate stats for subheader (CF%, relative CF%)
  player_corsi <- round((sum(y_corsi_axis$CF) / (sum(y_corsi_axis$CF) + sum(y_corsi_axis$CA_abs)) *100), 1)
  player_off_corsi <- round((sum(y_corsi_axis$CF_off) / (sum(y_corsi_axis$CF_off) + sum(y_corsi_axis$CA_off)) *100), 1)
  CF_rel <- player_corsi - player_off_corsi
  
  table_corsi <- player_corsi_data_1718 %>%
    filter(name == player_name) %>%
    select(game_number.x, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2)
  
  table_corsi_2 <- melt(table_corsi, id.var="game_number.x") %>%
    rename(GameNumber = game_number.x, ShotType = variable, Shots = value)
  
  levels(table_corsi_2$Type)
  table_corsi_2$ShotType <- factor(table_corsi_2$ShotType, levels = rev(levels(table_corsi_2$ShotType)))
  
  corsi_visual <- ggplot(table_corsi_2[order(table_corsi_2$ShotType),], 
                         aes(x = GameNumber, y = Shots, fill = ShotType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste("Corsi For and Against \n",  
                          round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1), 
                          ' CF%', 
                          
                          ' (', round(CF_rel, 1),' rel)', sep = '')) +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    #flip the order of the legend so CF is on top and CA is on the bottom
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-20, 20), expand = c(0, 0)) 
  
  
  if(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) > 20){
    corsi_visual <- corsi_visual + scale_y_continuous(limits=c(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) * -1, max(y_corsi_axis$CF,y_corsi_axis$CA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #Goals
  #same methodology as all others
  y_goals_axis <- player_ev_data_1718 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2) %>%
    mutate(GA_abs = abs(GA))
  
  table_goals <- player_ev_data_1718 %>%
    filter(name == player_name) %>%
    select(game_number, GF, GA) %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2)
  
  table_goals_2 <- melt(table_goals, id.var="game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)
  
  levels(table_goals_2$GoalType)
  table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))
  
  goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),], 
                         aes(x = GameNumber, y = Goals, fill = GoalType)) +
    geom_bar(stat = "identity") +
    labs(subtitle = paste('ES Goals For and Against \n', 
                          sum(table_goals$GF), ' GF  -  ', 
                          sum(y_goals_axis$GA_abs), ' GA ',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se', ' | ', '@zellenthal_DIF'), sep = '') +
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) 
  
  
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
  }
  
  
  #player_card_visual <- ggarrange(toi_visual, points_visual, points_visual, ncol = 1, nrow = 3)
  
  #return(player_card_visual)
  
  #title <- text_grob(paste(player_name, '18/19 Season', sep = ' ')) 
  
  #using the ggpubr package so that I can align all the ggplots on one visual
  #and so all the x axes align
  g2 <- ggplotGrob(toi_visual)
  g3 <- ggplotGrob(corsi_visual)
  g4 <- ggplotGrob(goals_visual)
  g5 <- ggplotGrob(points_visual)
  g <- rbind(g2, g5, g3, g4, size = "first")
  g$widths <- unit.pmax(g2$widths, g5$widths, g3$widths, g4$widths)
  grid.newpage()
  grid.draw(g)
  
  return(g)
  
}

# CSV Exports -------------------------------------------------------------

#Player Corsi
write.csv(player_corsi_data_1718, file = "player_corsi_data_1718.csv")

#Player EV
write.csv(player_ev_data_1718, file = "player_ev_data_1718.csv")

#Player Points
write.csv(player_points_data_1718, file = "player_points_data_1718.csv")

#Player TOI
write.csv(player_toi_data_1718, file = "player_toi_data_1718.csv")

#Unique Names
write.csv(unique(player_toi_data_1718$name), file = "player_names_1718.csv")













# Master Database ---------------------------------------------------------

player_toi_data_1819 <- player_toi_data_1819[,c(2:31)]
player_points_data_1819 <- player_points_data_1819[,c(2:32)]
player_corsi_data_1819 <- player_corsi_data_1819[,c(2:33)]

#add swehockey names to 1819 data
player_toi_data_1819 %>%
  left_join(player_lookup_1819, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey) -> player_toi_data_1819

player_points_data_1819 %>%
  left_join(player_lookup_1819, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey) -> player_points_data_1819

player_corsi_data_1819 %>%
  left_join(player_lookup_1819, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey) -> player_corsi_data_1819

#mutate in season column to everything
#1819
player_toi_data_1819 %>%
  mutate(season = '18/19') -> player_toi_data_1819

player_points_data_1819 %>%
  mutate(season = '18/19') -> player_points_data_1819

player_corsi_data_1819 %>%
  mutate(season = '18/19') -> player_corsi_data_1819

#1920
player_toi_data_1920 %>%
  mutate(season = '19/20') -> player_toi_data_1920

player_points_data_1920 %>%
  mutate(season = '19/20') -> player_points_data_1920

player_corsi_data_1920 %>%
  mutate(season = '19/20') -> player_corsi_data_1920


player_toi_data_1718 <- player_toi_data_1718[,c(2:31)]
player_points_data_1718 <- player_points_data_1718[,c(2:32)]
player_corsi_data_1718 <- player_corsi_data_1718[,c(2:33)]

#add swehockey names to 1718 data
player_toi_data_1718 %>%
  left_join(player_lookup_1718, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey) -> player_toi_data_1718

player_points_data_1718 %>%
  left_join(player_lookup_1718, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey) -> player_points_data_1718

player_corsi_data_1718 %>%
  left_join(player_lookup_1718, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey) -> player_corsi_data_1718

#mutate in season column to everything 1718
player_toi_data_1718 %>%
  mutate(season = '17/18') -> player_toi_data_1718

player_points_data_1718 %>%
  mutate(season = '17/18') -> player_points_data_1718

player_corsi_data_1718 %>%
  mutate(season = '17/18') -> player_corsi_data_1718


#bind tables together
player_toi_data_master <- rbind(player_toi_data_1718, player_toi_data_1819, player_toi_data_1920)
player_points_data_master <- rbind(player_points_data_1718, player_points_data_1819, player_points_data_1920)
player_corsi_data_master <- rbind(player_corsi_data_1718, player_corsi_data_1819, player_corsi_data_1920)


#template for renaming columns that are misaligned
player_corsi_data_1718 <- player_corsi_data_1718 %>%
  rename(
         `CF%_off` = `CF._off`,
         `FF%_off` = `FF._off`,
         `CF%` = `CF.`,
         `FF%` = `FF.`)


# player_names_1718 <- unique(player_corsi_data_1718$name) %>%
#   as_tibble()


