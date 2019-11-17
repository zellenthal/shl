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


# Schedules ---------------------------------------------------------------

#read in the 18/19 schedule, from stats.swehockey.se
#shl_schedule_1819 <- read_csv("swehockey_schedule_1819.csv")

#add a column that puts the date in a friendly format
#shl_schedule_1819 %>%
  #mutate(date2 = as.Date(shl_schedule_1819$date, "%m/%d/%Y")) -> shl_schedule_1819

  #shl_schedule_1819$date2 <- str_replace_all(shl_schedule_1819$date2, '00', '20')


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


# Team Schedules ----------------------------------------------------------

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

#messing around with a function to do this once as opposed to 14 separate times
create_team_schedule_1920 <- function(team) {
  
  #team_schedules_1920 <- list(bif_schedule_1920, dif_schedule_1920)
  
  table <- subset(shl_schedule_1920, grepl(team, shl_schedule_1920$combined_teams))
  
  table %>%
    mutate(game_number = row_number()) %>%
    mutate(team = team) %>%
    mutate(team_date = paste(team, date2, sep = '')) -> table
  
  return(table)
  
}


# Stats Scraping ----------------------------------------------------------

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
    
    for (date1 in as.list(shl_dates_1920[1:23])) {
    
    temp <- player_points(teamID)
    
    player_points_data_1920 <- rbind(player_points_data_1920, temp)
    
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
  
  for (date1 in as.list(shl_dates_1920[1:23])) {
    
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

#loop through all combination to get master team corsi table
team_corsi_data_1920 <- c()
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:23])) {
    
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
    #need this to get relative corsi later
    left_join(team_corsi_data_1920, by = "team_date") %>%
    
    #remove duplicated columns from join
    select(-c(Rank.y, Lag.y, GP.y, `CF%`, `CCF%`, `FF%`, `CFF%`,
              `OIG%`, `OISVS%`, PDO, date1.y, Lag2, game_number.y)) %>%
    
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

#loop through all combination to get master player corsi table
player_corsi_data_1920 <- c() 
for (teamID in shl_team_dictionary_1920$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_1920[1:23])) {
    
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
  
  for (date1 in as.list(shl_dates_1920[1:23])) {
    
    temp <- player_ev(teamID)
    
    player_ev_data_1920 <- rbind(player_ev_data_1920, temp)
    
    print(teamID)
    rm(temp)
    
  }
}


# Player Card -------------------------------------------------------------

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
    labs(title = paste(player_name, " 19/20 Season | ", y_toi_axis$Lag, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
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
  
  #Corsi
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
  
  
  #player_card_visual <- ggarrange(toi_visual, corsi_visual, points_visual, ncol = 1, nrow = 3)
  
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


