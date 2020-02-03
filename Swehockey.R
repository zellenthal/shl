library(plyr)
library(dplyr)
library(tidyverse)
library(rvest)
library(rowr)


# Team Abbreviation Table -------------------------------------------------

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

#all seasons
master_team_schedule <- read.csv("master_team_schedule.csv", stringsAsFactors = FALSE)

master_team_schedule %>%
  mutate(combined_teams_date = paste(combined_teams, date2, sep = ''),
         home_team_date = paste(home_team, date2, sep = ''),
         away_team_date = paste(away_team, date2, sep = '')) -> master_team_schedule

merge(master_team_schedule, shl_team_dictionary[,c("shlse_team_name_double", "swehockey_team_name")],
      by.x = "team", by.y = "shlse_team_name_double") -> master_team_schedule

master_team_schedule %>%
  mutate(swehockey_team_date = paste(swehockey_team_name, date2, sep = '')) -> master_team_schedule

#1920
master_team_schedule_1920 <- read.csv("master_team_schedule_1920.csv", stringsAsFactors = FALSE)

master_team_schedule_1920 %>%
  mutate(combined_teams_date = paste(combined_teams, date2, sep = ''),
         home_team_date = paste(home_team, date2, sep = ''),
         away_team_date = paste(away_team, date2, sep = '')) -> master_team_schedule_1920

merge(master_team_schedule_1920, shl_team_dictionary[,c("shlse_team_name_double", "swehockey_team_name")],
      by.x = "team", by.y = "shlse_team_name_double") -> master_team_schedule_1920

master_team_schedule_1920 %>%
  mutate(swehockey_team_date = paste(swehockey_team_name, date2, sep = '')) -> master_team_schedule_1920
  

#19/20 Season
abbreviation_1920 <- c("BIF", "DIF", "FRÖ", "FBK", "HV71", "LIF", "LHC", "LHF",
                       "MIF", "IKO", "ÖHK", "RBK", "SKE", "VÄX")

full_name_1920 <- c("Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK",
                    "HV 71", "Leksands IF", "Linköping HC", "Luleå HF", "IF Malmö Redhawks",
                    "IK Oskarshamn", "Örebro HK", "Rögle BK", "Skellefteå AIK", "Växjö Lakers HC")

swehockey_teams_1920 <- tibble(abbreviation_1920, full_name_1920)

#18/19 Season
abbreviation_1819 <- c("BIF", "DIF", "FRÖ", "FBK", "HV71", "LHC", "LHF",
                       "MIF", "MIK", "ÖHK", "RBK", "SKE", "TIK", "VÄX")

full_name_1819 <- c("Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK",
                    "HV 71", "Linköping HC", "Luleå HF", "IF Malmö Redhawks", "Mora IK",
                    "Örebro HK", "Rögle BK", "Skellefteå AIK", "Timrå IK", "Växjö Lakers HC")

swehockey_teams_1819 <- tibble(abbreviation_1819, full_name_1819)

#17/18 Season
abbreviation_1718 <- c("BIF", "DIF", "FRÖ", "FBK", "HV71", "KAR", "LHC", "LHF",
                       "MIF", "MIK", "ÖHK", "RBK", "SKE", "VÄX")

full_name_1718 <- c("Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK",
                    "HV 71", "Karlskrona HK", "Linköping HC", "Luleå HF", "IF Malmö Redhawks", "Mora IK",
                    "Örebro HK", "Rögle BK", "Skellefteå AIK", "Växjö Lakers HC")

swehockey_teams_1718 <- tibble(abbreviation_1718, full_name_1718)

#16/17 Season
abbreviation_1617 <- c("BIF", "DIF", "FRÖ", "FBK", "HV71", "KAR", "LIF", "LHC", "LHF",
                       "MIF", "ÖHK", "RBK", "SAIK", "VÄX")

full_name_1617 <- c("Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK",
                    "HV 71", "Karlskrona HK", "Leksands IF", "Linköping HC", "Luleå HF", "IF Malmö Redhawks",
                    "Örebro HK", "Rögle BK", "Skellefteå AIK", "Växjö Lakers HC")

swehockey_teams_1617 <- tibble(abbreviation_1617, full_name_1617)

#15/16 Season
abbreviation_1516 <- c("BIF", "DIF", "FRÖ", "FBK", "HV71", "KAR", "LHC", "LHF",
                       "MIF", "MODO", "ÖHK", "RBK", "SAIK", "VÄX")

full_name_1516 <- c("Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK",
                    "HV 71", "Karlskrona HK", "Linköping HC", "Luleå HF", "IF Malmö Redhawks",
                    "MODO Hockey", "Örebro HK", "Rögle BK", "Skellefteå AIK", "Växjö Lakers HC")

swehockey_teams_1516 <- tibble(abbreviation_1516, full_name_1516)

#all teams
abbreviation <- c("BIF", "DIF", "FRÖ", "FBK", "HV71", "KAR", "LIF", "LHC", "LHF", "MIF",
                  "MIK", "MODO", "IKO", "ÖHK", "RBK", "SAIK", "SKE", "TIK", "VÄX")

full_name <- c("Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK", "HV 71",
               "Karlskrona HK", "Leksands IF", "Linköping HC", "Luleå HF", "IF Malmö Redhawks",
               "Mora IK", "MODO Hockey", "IK Oskarshamn", "Örebro HK", "Rögle BK", "Skellefteå AIK",
               "Skellefteå AIK", "Timrå IK", "Växjö Lakers HC")

swehockey_teams <- tibble(abbreviation, full_name)



# Player Info -------------------------------------------------------------


#player lookup
player_lookup <- read.csv("player_lookup.csv", stringsAsFactors = FALSE)

player_lookup_1920 <- read.csv("player_lookup_1920.csv", stringsAsFactors = FALSE)

player_lookup_1920_update <- tibble(swehockey = c("John Persson"),
                                    shlse = c("John Persson"))

player_lookup_1920 <- rbind(player_lookup_1920, player_lookup_1920_update) 

#goalies
goalies <- c("Niklas Rubin", "Roman Will", "Joel Lassinantti", "Jonas Gunnarsson", "Oscar Alsenfelt",
             "Gustaf Lindvall", "Christoffer Rifalk", "Johan Mattsson", "Dominik Furch", "Niklas Svedberg",
             "Viktor Andrén", "Jonas Gustavsson", "Arvid Holm", "Tex Williamsson", "Fredrik Pettersson-Wentzel",
             'Fredrik Petterson-Wentzel',
             "Viktor Fasth", "Axel Brage", "Karri Rämö", "Samuel Ersson", "Lars Volden", "Markus Svensson",
             "Joacim Eriksson", "Janne Juvonen", "David Rautio", "Hugo Alnefelt", "Jonas Arntzen", "Mantas Armalis",
             "Jacob Johansson", "Robin Jensen", "Oskar Östlund", "Erik Källgren", "Adam Åhman", "Eric Forsberg",
             "Matteus Ward", "Lucas Rheyneuclaudes", "Jesper Eliasson", "Samuel Ward", "Fredrik Dichow", "Frederik Dichow", "Alfred Hallin",
             "Anton Malmborg", "Viktor Kokman", "Daniel Rosengren", "Hannes Kollén", "Calle Clang", "Lars Johansson",
             "Justin Pogge", "Mikael Tellqvist", "Erik Hanses", "Marcus Högberg", "Oscar Dansk", "Bernhard Starkbaum",
             "Julius Hudacek", "Lars Haugen", "Felix Sandström", "Johan Holmqvist", "Lars Volden", "Christopher Nihlstorp",
             "Jonatan Bjurö", "Erik Ersberg", "Adam Reideborn", "Linus Fernström", "Stefan Stéen", "Henrrik Lundberg",
             "Daniel Larsson", "Johan Gustafsson", "Patrick Galbraith", "Johan Backlund", "Filip Gustavsson", "Oscar Alsenfelt",
             "Pontus Sjögren", "Samuel Ward", "Daniel Marmenlind", "Linus Söderström", "Johannes Jönsson", "Joni Ortio",
             "Anders Lindbäck", "Dan Bakala", "Atte Engren", "Henrik Haukeland", "Arvid Söderblöm", "Arvid Söderblom", "Jonas Johansson",
             "Johan Mattsson", "Christian Engstrand", "Ville Kolppanen", "Eero Kilpeläinen", "Mattias Pettersson",
             "Emil Kruse", "Oscar Masiello", "Isak Mantler", "Jhonas Enroth", "Adam Werner", "Tomi Karhunen", "Isak Wallin",
             "Victor Brattström", "Linus Marcko", "Jesper Myrenberg", "Stefan Steen", "Olle Eriksson Ek", "Victor Östman",
             "Isac Olsson", "Eric Forsberg", "Isak Ragnevad", "Tobias Forsgren", "Oscar Walldén", "Magnus Åkerlund",
             "Robin Christofferson", "Oliver Norgren", "Arvid Ljung", "Arvid Ljung", "Linus Gidbark", "Emil Kruse", "William Josbrant",
             "Isak Wallin", "Oscar Fröberg", "David Elmgren", "Isak Mantler", "John Vestermark", "John Westermark", "Gustav Walldén",
             "Olof Lindbom", "Dennis Hildeby", "Henrik Lundberg", "Oliver Dackell", "Joel Svensson", "Arvid Söderblom", "Anton Malmborg",
             "Joel Vännström", "Pontus Eltonius", "Gustav Bågnevik", "Jonatan Stålberg", "Jonatan Frykholm", "Viktor Andersson", "Rupert Svärd",
             "Tim Hultstrand", "Rasmus Hedström", "Joren van Pottelberghe", "John Morelius", "Emil Hedman", "Thomas Lillie", "Filip Ternbom",
             "Jonas Johansson", "Hampus Alexandersson", "Tim Sandberg", "Albin Enqvist", "Jesper Eriksson", "Jonas Fransson", "David Elmgren",
             "Isac Olsson", "Isac Ohlsson", "Alexander Sahlin")



# Season URLs -------------------------------------------------------------

#All season SHL URLs (since 15/16)
box_score_urls <- get_schedule("SHL", c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20"))
#all game IDs in the SHL since 2015/2016
gameIDs <- str_split(box_score_urls$url, "/", simplify = TRUE)[,6]
gameID_table <- tibble(id = gameIDs, season = box_score_urls$season, league = "SHL")

#All lineup URLs (since 15/16)
url1 <- "http://stats.swehockey.se/Game/LineUps/"
paste(url1, gameID_table$id, sep = '')
lineup_urls <- tibble(url = paste(url1, gameID_table$id, sep = ''), league = gameID_table$league, season = gameID_table$season)

#15/16 URLs
box_score_urls_1516 <- subset(box_score_urls, grepl("2015-16", box_score_urls$season))
lineup_urls_1516 <- subset(lineup_urls, grepl("2015-16", lineup_urls$season))
gameIDs_1516 <- str_split(box_score_urls_1516$url, "/", simplify = TRUE)[,6]
gameID_table_1516 <- tibble(id = gameIDs_1516, season = box_score_urls_1516$season, league = "SHL")

#16/17 URLs
box_score_urls_1617 <- subset(box_score_urls, grepl("2016-17", box_score_urls$season))
lineup_urls_1617 <- subset(lineup_urls, grepl("2016-17", lineup_urls$season))
gameIDs_1617 <- str_split(box_score_urls_1617$url, "/", simplify = TRUE)[,6]
gameID_table_1617 <- tibble(id = gameIDs_1617, season = box_score_urls_1617$season, league = "SHL")

#17/18 URLs
box_score_urls_1718 <- subset(box_score_urls, grepl("2017-18", box_score_urls$season))
lineup_urls_1718 <- subset(lineup_urls, grepl("2017-18", lineup_urls$season))
gameIDs_1718 <- str_split(box_score_urls_1718$url, "/", simplify = TRUE)[,6]
gameID_table_1718 <- tibble(id = gameIDs_1718, season = box_score_urls_1718$season, league = "SHL")

#18/19 URLs
box_score_urls_1819 <- subset(box_score_urls, grepl("2018-19", box_score_urls$season))
lineup_urls_1819 <- subset(lineup_urls, grepl("2018-19", lineup_urls$season))
gameIDs_1819 <- str_split(box_score_urls_1819$url, "/", simplify = TRUE)[,6]
gameID_table_1819 <- tibble(id = gameIDs_1819, season = box_score_urls_1819$season, league = "SHL")

#19/20 URLs
box_score_urls_1920 <- subset(box_score_urls, grepl("2019-20", box_score_urls$season))
lineup_urls_1920 <- subset(lineup_urls, grepl("2019-20", lineup_urls$season))
gameIDs_1920 <- str_split(box_score_urls_1920$url, "/", simplify = TRUE)[,6]
gameID_table_1920 <- tibble(id = gameIDs_1920, season = box_score_urls_1920$season, league = "SHL")

# Get Updated GameIDs ---------------------------------------------------------

#updated - run these
get_1920_urls <- get_schedule("SHL", "2019-20")
get_1920_ids <- str_split(get_1920_urls$url, "/", simplify = TRUE)[,6]
get_1920_gameID_table <- tibble(id = get_1920_ids, season = get_1920_urls$season, league = "SHL")

get_1920_box_score_urls <- tibble(url = paste("http://stats.swehockey.se/Game/Events/", get_1920_ids[1:length(get_1920_ids)], sep = ''),
                                  league = 'SHL', season = '2019-20')

get_1920_lineup_urls <- tibble(url = paste("http://stats.swehockey.se/Game/LineUps/", get_1920_ids[1:length(get_1920_ids)], sep = ''),
                               league = 'SHL', season = '2019-20')


#existing tables to add to - then make unique
box_score_urls_1920 <- rbind(box_score_urls_1920, get_1920_box_score_urls) %>%
  unique()

lineup_urls_1920 <- rbind(lineup_urls_1920, get_1920_lineup_urls) %>%
  unique()

gameIDs_1920 <- get_1920_ids

gameID_table_1920 <- tibble(id = gameIDs_1920, season = box_score_urls_1920$season, league = "SHL")

# Lineup Function ---------------------------------------------------------


#this is pretty good, but it takes a lot of manual cleaning up front in terms of player names and whatnot
#also can be more efficient in terms of lines of code
get_lineup <- function(id) {
  
  url <- read_html(paste('http://stats.swehockey.se/Game/LineUps/', id, sep=''))
  
  teams <- url %>%
    html_nodes(xpath='//h2') %>%
    html_text() %>%
    tibble::enframe(name = NULL) %>%
    .[1,]
  
  teams %>% 
    str_split(., "\\-", simplify = TRUE) -> teams
  
  home_team <- str_squish(teams[,1])
  away_team <- str_squish(teams[,2])
  
    #date data
    date <- url %>%
      html_nodes(xpath='//td/h3') %>%
      html_text() %>%
      tibble::enframe(name = NULL)
    
      date <- as.character(date[1,])
      #parsing out numbers from the date
      date_numbers <- str_extract_all(date, "[0-9]") %>% unlist() 
    
      year <- paste(date_numbers[1], date_numbers[2], date_numbers[3], date_numbers[4], sep = '')
      month <- paste(date_numbers[5], date_numbers[6], sep = '')
      day <- paste(date_numbers[7], date_numbers[8], sep = '')
    
      date <- as.Date(paste(year, month, day, sep = '-'))
  
  #lineup data
  raw_lineup <- url %>%
    html_nodes(".lineUpPlayer") %>%
    html_text() %>%
    tibble::enframe(name = NULL) %>%
    
    
    mutate(player = str_split(value, "\\(", simplify = TRUE, n = 2)[,1]) %>%
    mutate(player2 = str_squish(player)) %>%
    
    mutate(number = gsub( " .*$", "", player2)) %>%
    mutate(number2 = gsub('.{1}$', '', number)) %>%
    
    mutate(player3 = gsub('[0-9]+', '', player2)) %>%
    mutate(player4 = gsub('[.]', '', player3)) %>%
    mutate(player5 = str_squish(player4)) %>%
    
    mutate(last_name = str_split(player5, ",", simplify = TRUE)[,1]) %>%
    mutate(first_name = str_squish(str_split(player5, ",", simplify = TRUE)[,2])) %>%
    mutate(player6 = paste(first_name, last_name, sep = ' ')) %>%
    
    #goalie designation
    mutate(goalie = case_when(player6 %in% goalies ~ "GK")) %>% 
    mutate(player7 = ifelse(player6 %in% goalies, paste(player6, goalie, sep = ''), paste(player6, sep = ''))) %>%
    
    #add date
    mutate(date = date)
  
  raw_lineup <- raw_lineup[,c(5,13,14)] %>%
    rename(jersey = number2, player = player7)
  
  #assigning team designation, not great!
  lineup_home <- raw_lineup %>%
    slice(seq(0.5 * n()))
  
  lineup_home %>%
    mutate(team = home_team) -> lineup_home
  
  lineup_away <- raw_lineup %>%
    slice(-seq(0.5 * n()))
  
  lineup_away %>%
    mutate(team = away_team) -> lineup_away
  
  #final lineup
  lineup_final <- rbind(lineup_home, lineup_away)
  
  lineup_final %>%
    mutate(gameid = id) %>%
    mutate(uid = paste(jersey, team, date, sep = '')) %>% 
    mutate(team_date = paste(team, date, sep = '')) -> lineup_final
  
  lineup_final <- lineup_final[,c(5,3,4,1,2,6,7)]
  
  lineup_final <- merge(lineup_final, master_team_schedule[,c("swehockey_team_date", "game_number")],
                        by.x = "team_date", by.y = "swehockey_team_date")
  
  lineup_final <- lineup_final[,c(2:6,1,7,8)]
  
  return(lineup_final)
  
}

#for loop
lineup_data <- c()
for (id in gameID_table$id[1:1652]) {
  
  temp <- get_lineup(id)
  
  lineup_data <- rbind(lineup_data, temp)
  
  print(id)
  rm(temp)
  
}

#did A LOT of manual cleaning in Excel, loading that back in
#took lineupdata from the above for loop and cleaned it
lineup_data2 <- read.csv("lineup_data2.csv", stringsAsFactors = FALSE)

#19/20
lineup_data_1920 <- c()
for(id in gameID_table_1920$id[1:237]) {
  
  temp <- get_lineup(id)
  
  lineup_data_1920 <- rbind(lineup_data_1920, temp)
  
  print(id)
  rm(temp)
  
}


#manual edits
#lineup_data_1920v2 <- read.csv("lineup_data_1920_v2.csv")
#lineup_data_1920v2 <- read.csv("lineup_data_1920_v3.csv")
lineup_data_1920 <- read_csv("lineup_data_1920.csv")

lineup_data_1920_update <- c()
for(id in gameID_table_1920$id[255:260]) {

  temp <- get_lineup(id)

  lineup_data_1920_update <- rbind(lineup_data_1920_update, temp)

  print(id)
  rm(temp)

}

#if manual edits needed to update
write_excel_csv(lineup_data_1920_update, "lineup_data_1920_update.csv")

#and re-uploaded
lineup_data_1920_update <- read.csv("lineup_data_1920_update.csv",  stringsAsFactors = FALSE)

#join updated lineup data with existing base
lineup_data_1920 <- rbind(lineup_data_1920, lineup_data_1920_update)

ha_lineup_data_1920 <- c()
for(id in ha_gameID_table$id[1:273]) {
  
  temp <- get_lineup(id)
  
  ha_lineup_data_1920 <- rbind(ha_lineup_data_1920, temp)
  
  print(id)
  rm(temp)
  
}

#rbind.fill(lineup_data_1920v2, lineup_add) -> lineup_data_1920v2

#18/19
# lineup_data_1819 <- c()
# for(id in gameID_table_1819$id[1:364]) {
#   
#   temp <- get_lineup(id)
#   
#   lineup_data_1819 <- rbind(lineup_data_1819, temp)
#   
#   print(id)
#   rm(temp)
#   
# }
# 
# #manual edits
# lineup_data_1819v2 <- read.csv("lineup1819.csv")

# Box Score Function ------------------------------------------------------


#big function
get_box_score <- function(id) {
  
  url <- read_html(paste('http://stats.swehockey.se/Game/Events/', id, sep=''))
  
  #date data
  date <- url %>%
    html_nodes(xpath='//td/h3') %>%
    html_text() %>%
    tibble::enframe(name = NULL)
  
  #team data
  teams <- url %>%
    html_nodes(xpath='//h2') %>%
    html_text() %>%
    tibble::enframe(name = NULL) %>%
    .[1,]
  
  teams %>% 
    str_split(., "\\-", simplify = TRUE) -> teams
  
  home_team <- str_squish(teams[,1])
  away_team <- str_squish(teams[,2])
  
  date <- as.character(date[1,])
  #parsing out numbers from the date
  date_numbers <- str_extract_all(date, "[0-9]") %>% unlist() 
  
  year <- paste(date_numbers[1], date_numbers[2], date_numbers[3], date_numbers[4], sep = '')
  month <- paste(date_numbers[5], date_numbers[6], sep = '')
  day <- paste(date_numbers[7], date_numbers[8], sep = '')
  
  date <- as.Date(paste(year, month, day, sep = '-'))
  
  #initial box score data
  raw_data <- url %>%
    html_nodes("tr+ tr .tblContent .tdOdd") %>%
    html_text() %>%
    tibble::enframe(name = NULL)
  
  goal_data <- raw_data %>%
    filter(lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 0) | 
             lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 1) |
             lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 2) |
             #includes the players on for each goal
             lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 3))
  
  team <- goal_data %>%
    filter(row_number() %% 4 == 2) %>%
    set_names("team")
  
  situation <- goal_data %>%
    filter(row_number() %% 4 == 1) %>%
    set_names("situation")
  
  scorers <- goal_data %>%
    filter(row_number() %% 4 == 3) %>%
    set_names("scorers")
  
  players <- goal_data %>%
    filter(row_number() %% 4 == 0) %>%
    set_names("players")
  
  box_score_data <- bind_cols(team, situation, scorers, players) %>%
    mutate(goal_state = str_split(situation, "\\(", simplify = TRUE, n = 2)[,1]) %>%
    mutate(situation = str_split(situation, "\\(", simplify = TRUE, n = 2)[,2]) %>%
    mutate(situation = str_replace_all(situation, "\\)", "")) %>%
    mutate(points = str_replace_all(scorers, "[\r\n]", "")) %>%
    mutate(goal = str_split(scorers, "[0-9]+\\.", simplify = TRUE, n = 4)[,2]) %>%
    mutate(goal = str_split(goal, "\\(", simplify = TRUE)[,1]) %>%
    mutate(goal = str_c(str_squish(str_split(goal, ",", simplify = TRUE, n = 2)[,2]), str_squish(str_split(goal, ",", simplify = TRUE, n = 2)[,1]), sep = " ")) %>%
    mutate(goal = str_replace_all(goal, "[0-9]", "")) %>%
    mutate(primary_assist = str_split(points, "[0-9]+\\.", simplify = TRUE, n = 4)[,3]) %>%
    mutate(primary_assist = str_c(str_squish(str_split(primary_assist, ",", simplify = TRUE, n = 2)[,2]), str_squish(str_split(primary_assist, ",", simplify = TRUE, n = 2)[,1]), sep = " ")) %>%
    mutate(secondary_assist = str_split(points, "[0-9]+\\.", simplify = TRUE, n = 4)[,4]) %>%
    mutate(secondary_assist = str_c(str_squish(str_split(secondary_assist, ",", simplify = TRUE, n = 2)[,2]), str_squish(str_split(secondary_assist, ",", simplify = TRUE, n = 2)[,1]), sep = " ")) %>%
    mutate_all(str_squish) %>%
    select(-scorers, -points)
  
  #number of goals
  n <- nrow(box_score_data)
  
  #need another function to loop through each goal, pull out player numbers
  gf <- c()
  for(i in 1:n) {
    
    temp <- regexpr('(?<=Pos. Part.:)[ ,0-9]+', box_score_data$players[i], perl=TRUE)
    temp <- regmatches(box_score_data$players[i], temp)
    temp <- strsplit(temp, ',')[[1]]
    temp <- trimws(temp) %>%
      as.data.frame()
    
    #temp <- temp[,2:7]
    
    gf <- cbind.fill(gf, temp, fill = NA)
    
    rm(temp)
    
  }
  
  #remove weird first column
  gf[,2:(n+1)] -> gf
  #this will do
  as.data.frame(t(as.matrix(gf))) -> gf
  
  if(ncol(gf) == 6) {
    gf <- gf %>%
      rename(GF1 = V1, GF2 = V2, GF3 = V3, GF4 = V4, GF5 = V5, GF6 = V6)
  }
  
  if(ncol(gf) == 5) {
    gf <- gf %>%
      rename(GF1 = V1, GF2 = V2, GF3 = V3, GF4 = V4, GF5 = V5)
  }
  
  if(ncol(gf) == 4) {
    gf <- gf %>%
      rename(GF1 = V1, GF2 = V2, GF3 = V3, GF4 = V4)
  }
  
  if(ncol(gf) == 1) {
    gf <- gf %>%
      rename(GF1 = V1)
  }

  
  ga <- c()
  for(i in 1:n) {
    
    temp <- regexpr('(?<=Neg. Part.:)[ ,0-9]+', box_score_data$players[i], perl=TRUE)
    temp <- regmatches(box_score_data$players[i], temp)
    temp <- strsplit(temp, ',')[[1]]
    temp <- trimws(temp) %>%
      as.data.frame()
    
    ga <- cbind.fill(ga, temp, fill = NA)
    
    #print(temp)
    rm(temp)
    
  }
  
  #remove weird first column
  ga[,2:(n+1)] -> ga
  #this will do
  as.data.frame(t(as.matrix(ga))) -> ga
  
  if(ncol(ga) == 6) {
    ga <- ga %>%
      rename(GA1 = V1, GA2 = V2, GA3 = V3, GA4 = V4, GA5 = V5, GA6 = V6)
  }
  
  if(ncol(ga) == 5) {
    ga <- ga %>%
      rename(GA1 = V1, GA2 = V2, GA3 = V3, GA4 = V4, GA5 = V5)
  }
  
  if(ncol(ga) == 4) {
    ga <- ga %>%
      rename(GA1 = V1, GA2 = V2, GA3 = V3, GA4 = V4)
  }
  
  if(ncol(ga) == 1) {
    ga <- ga %>%
      rename(GA1 = V1)
  }
  
  box_score_data <- bind_cols(box_score_data, gf, ga) %>%
    select(-players)
  
  box_score_data %>%
    mutate(home_team = home_team, away_team = away_team) %>%
    left_join(swehockey_teams, by = c("team" = "abbreviation")) %>%
    rename(gf_team = full_name) %>%
    mutate(ga_team = ifelse(gf_team == home_team, away_team, home_team)) %>%
    mutate(date = date) %>%
    mutate(gameid = id) %>% 
    mutate(gf_team_date = paste(gf_team, date, sep = '')) %>%
    mutate(ga_team_date = paste(ga_team, date, sep = '')) -> box_score_data
  
  box_score_data <- merge(box_score_data, master_team_schedule[,c("swehockey_team_date", "game_number")],
                          by.x = "gf_team_date", by.y = "swehockey_team_date") %>%

    rename(gf_team_game_number = game_number)

  box_score_data <- merge(box_score_data, master_team_schedule[,c("swehockey_team_date", "game_number")],
                          by.x = "ga_team_date", by.y = "swehockey_team_date") %>%

    rename(ga_team_game_number = game_number)
  
  
  if(ncol(gf) == 6 & ncol(ga) == 6) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GF2_uid = paste(GF2, gf_team, date, sep = ''),
             GF3_uid = paste(GF3, gf_team, date, sep = ''),
             GF4_uid = paste(GF4, gf_team, date, sep = ''),
             GF5_uid = paste(GF5, gf_team, date, sep = ''),
             GF6_uid = paste(GF6, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''),
             GA2_uid = paste(GA2, ga_team, date, sep = ''),
             GA3_uid = paste(GA3, ga_team, date, sep = ''),
             GA4_uid = paste(GA4, ga_team, date, sep = ''),
             GA5_uid = paste(GA5, ga_team, date, sep = ''),
             GA6_uid = paste(GA6, ga_team, date, sep = ''))
    
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GF1_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GF2_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GF3_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GF4_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GF5_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF6_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GF6_name = player)
  #   
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GA1_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GA2_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GA3_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GA4_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GA5_name = player)
  #   box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA6_uid", by.y = "uid", all.x = TRUE) %>%
  #     rename(GA6_name = player)
  #   
  #   box_score_data <- box_score_data %>%
  #     mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name, sep = ''),
  #            GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name, sep = ''))
  #   
  # }
    
  }
  
  if(ncol(gf) == 6 & ncol(ga) == 5) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GF2_uid = paste(GF2, gf_team, date, sep = ''),
             GF3_uid = paste(GF3, gf_team, date, sep = ''),
             GF4_uid = paste(GF4, gf_team, date, sep = ''),
             GF5_uid = paste(GF5, gf_team, date, sep = ''),
             GF6_uid = paste(GF6, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''),
             GA2_uid = paste(GA2, ga_team, date, sep = ''),
             GA3_uid = paste(GA3, ga_team, date, sep = ''),
             GA4_uid = paste(GA4, ga_team, date, sep = ''),
             GA5_uid = paste(GA5, ga_team, date, sep = ''))
    
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF5_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF6_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF6_name = player)
    # 
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA5_name = player)
    # 
    # box_score_data <- box_score_data %>%
    #   mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name, sep = ''),
    #          GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, sep = ''))
     
  }
  
  if(ncol(gf) == 6 & ncol(ga) == 4) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GF2_uid = paste(GF2, gf_team, date, sep = ''),
             GF3_uid = paste(GF3, gf_team, date, sep = ''),
             GF4_uid = paste(GF4, gf_team, date, sep = ''),
             GF5_uid = paste(GF5, gf_team, date, sep = ''),
             GF6_uid = paste(GF6, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''),
             GA2_uid = paste(GA2, ga_team, date, sep = ''),
             GA3_uid = paste(GA3, ga_team, date, sep = ''),
             GA4_uid = paste(GA4, ga_team, date, sep = ''))
    
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF5_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF6_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF6_name = player)
    # 
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA4_name = player)
    # 
    # box_score_data <- box_score_data %>%
    #   mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name, sep = ''),
    #          GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, sep = ''))
    
  }
  
  if(ncol(gf) == 5 & ncol(ga) == 6) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GF2_uid = paste(GF2, gf_team, date, sep = ''),
             GF3_uid = paste(GF3, gf_team, date, sep = ''),
             GF4_uid = paste(GF4, gf_team, date, sep = ''),
             GF5_uid = paste(GF5, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''),
             GA2_uid = paste(GA2, ga_team, date, sep = ''),
             GA3_uid = paste(GA3, ga_team, date, sep = ''),
             GA4_uid = paste(GA4, ga_team, date, sep = ''),
             GA5_uid = paste(GA5, ga_team, date, sep = ''),
             GA6_uid = paste(GA6, ga_team, date, sep = ''))
    
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF5_name = player)
    # 
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA5_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA6_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA6_name = player)
    # 
    # box_score_data <- box_score_data %>%
    #   mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, sep = ''),
    #          GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name, sep = ''))
    
  }
  
  if(ncol(gf) == 5 & ncol(ga) == 5) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GF2_uid = paste(GF2, gf_team, date, sep = ''),
             GF3_uid = paste(GF3, gf_team, date, sep = ''),
             GF4_uid = paste(GF4, gf_team, date, sep = ''),
             GF5_uid = paste(GF5, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''),
             GA2_uid = paste(GA2, ga_team, date, sep = ''),
             GA3_uid = paste(GA3, ga_team, date, sep = ''),
             GA4_uid = paste(GA4, ga_team, date, sep = ''),
             GA5_uid = paste(GA5, ga_team, date, sep = ''))
    
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF5_name = player)
    # 
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA5_name = player)
    # 
    # box_score_data <- box_score_data %>%
    #   mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, sep = ''),
    #          GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, sep = ''))
    
  }
  
  if(ncol(gf) == 5 & ncol(ga) == 4) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GF2_uid = paste(GF2, gf_team, date, sep = ''),
             GF3_uid = paste(GF3, gf_team, date, sep = ''),
             GF4_uid = paste(GF4, gf_team, date, sep = ''),
             GF5_uid = paste(GF5, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''),
             GA2_uid = paste(GA2, ga_team, date, sep = ''),
             GA3_uid = paste(GA3, ga_team, date, sep = ''),
             GA4_uid = paste(GA4, ga_team, date, sep = ''))
    
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF5_name = player)
    # 
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA4_name = player)
    # 
    # box_score_data <- box_score_data %>%
    #   mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, sep = ''),
    #          GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, sep = ''))
    
  }
  
  if(ncol(gf) == 4 & ncol(ga) == 6) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GF2_uid = paste(GF2, gf_team, date, sep = ''),
             GF3_uid = paste(GF3, gf_team, date, sep = ''),
             GF4_uid = paste(GF4, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''),
             GA2_uid = paste(GA2, ga_team, date, sep = ''),
             GA3_uid = paste(GA3, ga_team, date, sep = ''),
             GA4_uid = paste(GA4, ga_team, date, sep = ''),
             GA5_uid = paste(GA5, ga_team, date, sep = ''),
             GA6_uid = paste(GA6, ga_team, date, sep = ''))
    
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF4_name = player)
    # 
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA5_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA6_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA6_name = player)
    # 
    # box_score_data <- box_score_data %>%
    #   mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, sep = ''),
    #          GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name, sep = ''))
    
  }
  
  if(ncol(gf) == 4 & ncol(ga) == 5) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GF2_uid = paste(GF2, gf_team, date, sep = ''),
             GF3_uid = paste(GF3, gf_team, date, sep = ''),
             GF4_uid = paste(GF4, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''),
             GA2_uid = paste(GA2, ga_team, date, sep = ''),
             GA3_uid = paste(GA3, ga_team, date, sep = ''),
             GA4_uid = paste(GA4, ga_team, date, sep = ''),
             GA5_uid = paste(GA5, ga_team, date, sep = ''))
    
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF4_name = player)
    # 
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA4_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA5_name = player)
    # 
    # box_score_data <- box_score_data %>%
    #   mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, sep = ''),
    #          GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, sep = ''))
    
  }
  
  if(ncol(gf) == 4 & ncol(ga) == 4) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GF2_uid = paste(GF2, gf_team, date, sep = ''),
             GF3_uid = paste(GF3, gf_team, date, sep = ''),
             GF4_uid = paste(GF4, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''),
             GA2_uid = paste(GA2, ga_team, date, sep = ''),
             GA3_uid = paste(GA3, ga_team, date, sep = ''),
             GA4_uid = paste(GA4, ga_team, date, sep = ''))
    
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF4_name = player)
    # 
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA1_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA2_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA3_name = player)
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA4_name = player)
    # 
    # box_score_data <- box_score_data %>%
    #   mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, sep = ''),
    #          GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, sep = ''))
  }
  
  if(ncol(gf) == 1 & ncol(ga) == 1) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''))
    
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GF1_name = player)
    # 
    # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
    #   rename(GA1_name = player)
    # 
    # box_score_data <- box_score_data %>%
    #   mutate(GF_names = paste(GF1_name, sep = ''),
    #          GA_names = paste(GA1_name, sep = ''))
  }
  
  
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GF1_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GF2_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GF3_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GF4_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GF5_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GF6_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GF6_name = player)
  # 
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GA1_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GA2_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GA3_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GA4_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GA5_name = player)
  # 
  # box_score_data <- merge(box_score_data, lineup_data_1920v2[,c("player", "uid")], by.x = "GA6_uid", by.y = "uid", all.x = TRUE) %>%
  #   rename(GA6_name = player)


  # box_score_data <- box_score_data %>%
  #   mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name, sep = ''),
  #          GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name, sep = ''))

  # box_score_data <- box_score_data %>%
  #   mutate(GF_GK = str_count(GF_names, "GK"),
  #          GF_NA = str_count(GF_names, "NA")) %>%
  #   mutate(GF_skaters = 6 - GF_GK - GF_NA) %>%
  # 
  #   mutate(GA_GK = str_count(GA_names, "GK"),
  #          GA_NA = str_count(GA_names, "NA")) %>%
  #   mutate(GA_skaters = 6 - GA_GK - GA_NA)
  # 
  # 
  # box_score_data <- box_score_data %>%
  #   mutate(game_situation = case_when(
  #     GF_skaters == 6 & GA_skaters == 6 & GA_GK == 0 ~ '6v6ENG',
  #     GF_skaters == 6 & GA_skaters == 6 ~ '6v6',
  #     GF_skaters == 6 & GA_skaters == 5 & GA_GK == 0 ~ '6v5ENG',
  #     GF_skaters == 6 & GA_skaters == 5 ~ '6v5',
  #     GF_skaters == 6 & GA_skaters == 4 & GA_GK == 0 ~ '6v4ENG',
  #     GF_skaters == 6 & GA_skaters == 4 ~ '6v4',
  #     GF_skaters == 6 & GA_skaters == 3 & GA_GK == 0 ~ '6v3ENG',
  #     GF_skaters == 6 & GA_skaters == 3 ~ '6v3',
  # 
  #     GF_skaters == 5 & GA_skaters == 6 & GA_GK == 0 ~ '5v6ENG',
  #     GF_skaters == 5 & GA_skaters == 6 ~ '5v6',
  #     GF_skaters == 5 & GA_skaters == 5 & GA_GK == 0 ~ '5v5ENG',
  #     GF_skaters == 5 & GA_skaters == 5 ~ '5v5',
  #     GF_skaters == 5 & GA_skaters == 4 & GA_GK == 0 ~ '5v4ENG',
  #     GF_skaters == 5 & GA_skaters == 4 ~ '5v4',
  #     GF_skaters == 5 & GA_skaters == 3 & GA_GK == 0 ~ '5v3ENG',
  #     GF_skaters == 5 & GA_skaters == 3 ~ '5v3',
  # 
  #     GF_skaters == 4 & GA_skaters == 6 & GA_GK == 0 ~ '4v6ENG',
  #     GF_skaters == 4 & GA_skaters == 6 ~ '4v6',
  #     GF_skaters == 4 & GA_skaters == 5 & GA_GK == 0 ~ '4v5ENG',
  #     GF_skaters == 4 & GA_skaters == 5 ~ '4v5',
  #     GF_skaters == 4 & GA_skaters == 4 & GA_GK == 0 ~ '4v4ENG',
  #     GF_skaters == 4 & GA_skaters == 4 ~ '4v4',
  #     GF_skaters == 4 & GA_skaters == 3 & GA_GK == 0 ~ '4v3ENG',
  #     GF_skaters == 4 & GA_skaters == 3 ~ '4v3',
  # 
  #     GF_skaters == 3 & GA_skaters == 6 & GA_GK == 0 ~ '3v6ENG',
  #     GF_skaters == 3 & GA_skaters == 6 ~ '3v6',
  #     GF_skaters == 3 & GA_skaters == 5 & GA_GK == 0 ~ '3v5ENG',
  #     GF_skaters == 3 & GA_skaters == 5 ~ '3v5',
  #     GF_skaters == 3 & GA_skaters == 4 & GA_GK == 0 ~ '3v4ENG',
  #     GF_skaters == 3 & GA_skaters == 4 ~ '3v4',
  #     GF_skaters == 3 & GA_skaters == 3 & GA_GK == 0 ~ '3v3ENG',
  #     GF_skaters == 3 & GA_skaters == 3 ~ '3v3',
  # 
  #     GF_skaters == 1 & GA_skaters == 0 ~ '1v0'))
  # 
  # box_score_data <- box_score_data %>%
  #   mutate(game_situation_general = case_when(
  #     game_situation == '6v6ENG' ~ 'ENG',
  #     game_situation == '6v5ENG' ~ 'ENG',
  #     game_situation == '6v4ENG' ~ 'ENG',
  #     game_situation == '6v3ENG' ~ 'ENG',
  #     game_situation == '5v6ENG' ~ 'ENG',
  #     game_situation == '5v5ENG' ~ 'ENG',
  #     game_situation == '5v4ENG' ~ 'ENG',
  #     game_situation == '5v3ENG' ~ 'ENG',
  #     game_situation == '4v6ENG' ~ 'ENG',
  #     game_situation == '4v5ENG' ~ 'ENG',
  #     game_situation == '4v4ENG' ~ 'ENG',
  #     game_situation == '4v3ENG' ~ 'ENG',
  #     game_situation == '3v6ENG' ~ 'ENG',
  #     game_situation == '3v5ENG' ~ 'ENG',
  #     game_situation == '3v4ENG' ~ 'ENG',
  #     game_situation == '3v3ENG' ~ 'ENG',
  #     game_situation == '5v5' & situation == 'EQENG' ~ 'ENG',
  #     game_situation == '4v4' & situation == 'EQENG' ~ 'ENG',
  #     game_situation == '3v3' & situation == 'EQENG' ~ 'ENG',
  # 
  #     game_situation == '6v6' ~ 'EV',
  #     game_situation == '5v5' ~ 'EV',
  #     game_situation == '4v4' ~ 'EV',
  #     game_situation == '3v3' ~ 'EV',
  # 
  #     game_situation == '6v5' & situation == 'EQ' ~ 'Goalie Pulled',
  #     game_situation == '5v4' & situation == 'EQ' ~ 'Goalie Pulled',
  #     game_situation == '4v3' & situation == 'EQ' ~ 'Goalie Pulled',
  # 
  #     game_situation == '5v4' ~ 'PP',
  #     game_situation == '5v3' ~ 'PP',
  #     game_situation == '4v3' ~ 'PP',
  #     game_situation == '6v4' ~ 'PP',
  #     game_situation == '6v3' ~ 'PP',
  # 
  #     game_situation == '4v5' ~ 'SH',
  #     game_situation == '3v5' ~ 'SH',
  #     game_situation == '3v4' ~ 'SH',
  # 
  #     game_situation == '1v0' & situation == 'GWS' ~ 'SO',
  #     game_situation == '1v0' & situation == 'PS' ~ 'PS'))

  # box_score_data <- box_score_data %>%
  #   select(gameid, date, home_team, away_team, team, goal_state, situation, goal, primary_assist, secondary_assist,
  #          gf_team, ga_team, gf_team_game_number, ga_team_game_number, GF_names, GA_names, GF_skaters, GA_skaters,
  #          game_situation, game_situation_general)
  
  # box_score_data <- box_score_data %>%
  #   select(gameid, date, home_team, away_team, team, goal_state, situation, goal, primary_assist, secondary_assist,
  #          gf_team, ga_team, gf_team_game_number, ga_team_game_number, GF_names, GA_names)
  
  # box_score_data <- box_score_data %>%
  #   select(gameid, date, home_team, away_team, team, goal_state, situation, goal, primary_assist, secondary_assist,
  #          gf_team, ga_team, gf_team_game_number, ga_team_game_number)
  
  return(box_score_data)
}


# Box Score Data ----------------------------------------------------------



box_score_data <- c()
for (id in gameID_table$id[1:1652]) {
  
  temp <- get_box_score(id)
  
  box_score_data <- rbind.fill(box_score_data, temp)
  
  print(id)
  rm(temp)
  
}

#19/20
box_score_data_1920 <- c()
for (id in gameID_table_1920$id[1:204]) {
  
  temp <- get_box_score(id)
  
  box_score_data_1920 <- rbind.fill(box_score_data_1920, temp)
  
  print(id)
  rm(temp)
  
}

box_score_data_1920_raw <- c()
for (id in gameID_table_1920$id[1:260]) {
  
  temp <- get_box_score(id)
  
  box_score_data_1920_raw <- rbind.fill(box_score_data_1920_raw, temp)
  
  print(id)
  rm(temp)
  
}


# Enrich Raw Box Score Data -----------------------------------------------

#Join in player names
box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF1_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
                                rename(GF2_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF3_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF4_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF5_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GF6_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF6_name = player)


box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA1_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA2_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA3_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA4_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA5_name = player)

box_score_data_1920_raw <- merge(box_score_data_1920_raw, lineup_data_1920[,c("player", "uid")],
                                 by.x = "GA6_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA6_name = player)


#Concatenate all GF and GA names
box_score_data_1920_raw <- box_score_data_1920_raw %>%
  mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name, sep = ''),
         GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name, sep = ''))

#Count goalies and NAs to get manpower situation
box_score_data_1920_raw <- box_score_data_1920_raw %>%
  mutate(GF_GK = str_count(box_score_data_1920_raw$GF_names, "GK"),
         GF_NA = str_count(box_score_data_1920_raw$GF_names, "NA")) %>%
  mutate(GF_skaters = 6 - GF_GK - GF_NA) %>%
  
  mutate(GA_GK = str_count(box_score_data_1920_raw$GA_names, "GK"),
         GA_NA = str_count(box_score_data_1920_raw$GA_names, "NA")) %>%
  mutate(GA_skaters = 6 - GA_GK - GA_NA)

#assess situation
box_score_data_1920_raw <- box_score_data_1920_raw %>%
  mutate(game_situation = case_when(
    GF_skaters == 6 & GA_skaters == 6 & GA_GK == 0 ~ '6v6ENG',
    GF_skaters == 6 & GA_skaters == 6 ~ '6v6',
    GF_skaters == 6 & GA_skaters == 5 & GA_GK == 0 ~ '6v5ENG',
    GF_skaters == 6 & GA_skaters == 5 ~ '6v5',
    GF_skaters == 6 & GA_skaters == 4 & GA_GK == 0 ~ '6v4ENG',
    GF_skaters == 6 & GA_skaters == 4 ~ '6v4',
    GF_skaters == 6 & GA_skaters == 3 & GA_GK == 0 ~ '6v3ENG',
    GF_skaters == 6 & GA_skaters == 3 ~ '6v3',

    GF_skaters == 5 & GA_skaters == 6 & GA_GK == 0 ~ '5v6ENG',
    GF_skaters == 5 & GA_skaters == 6 ~ '5v6',
    GF_skaters == 5 & GA_skaters == 5 & GA_GK == 0 ~ '5v5ENG',
    GF_skaters == 5 & GA_skaters == 5 ~ '5v5',
    GF_skaters == 5 & GA_skaters == 4 & GA_GK == 0 ~ '5v4ENG',
    GF_skaters == 5 & GA_skaters == 4 ~ '5v4',
    GF_skaters == 5 & GA_skaters == 3 & GA_GK == 0 ~ '5v3ENG',
    GF_skaters == 5 & GA_skaters == 3 ~ '5v3',

    GF_skaters == 4 & GA_skaters == 6 & GA_GK == 0 ~ '4v6ENG',
    GF_skaters == 4 & GA_skaters == 6 ~ '4v6',
    GF_skaters == 4 & GA_skaters == 5 & GA_GK == 0 ~ '4v5ENG',
    GF_skaters == 4 & GA_skaters == 5 ~ '4v5',
    GF_skaters == 4 & GA_skaters == 4 & GA_GK == 0 ~ '4v4ENG',
    GF_skaters == 4 & GA_skaters == 4 ~ '4v4',
    GF_skaters == 4 & GA_skaters == 3 & GA_GK == 0 ~ '4v3ENG',
    GF_skaters == 4 & GA_skaters == 3 ~ '4v3',

    GF_skaters == 3 & GA_skaters == 6 & GA_GK == 0 ~ '3v6ENG',
    GF_skaters == 3 & GA_skaters == 6 ~ '3v6',
    GF_skaters == 3 & GA_skaters == 5 & GA_GK == 0 ~ '3v5ENG',
    GF_skaters == 3 & GA_skaters == 5 ~ '3v5',
    GF_skaters == 3 & GA_skaters == 4 & GA_GK == 0 ~ '3v4ENG',
    GF_skaters == 3 & GA_skaters == 4 ~ '3v4',
    GF_skaters == 3 & GA_skaters == 3 & GA_GK == 0 ~ '3v3ENG',
    GF_skaters == 3 & GA_skaters == 3 ~ '3v3',

    GF_skaters == 1 & GA_skaters == 0 ~ '1v0'))

box_score_data_1920_raw <- box_score_data_1920_raw %>%
  mutate(game_situation_general = case_when(
        game_situation == '6v6ENG' ~ 'ENG',
        game_situation == '6v5ENG' ~ 'ENG',
        game_situation == '6v4ENG' ~ 'ENG',
        game_situation == '6v3ENG' ~ 'ENG',
        game_situation == '5v6ENG' ~ 'ENG',
        game_situation == '5v5ENG' ~ 'ENG',
        game_situation == '5v4ENG' ~ 'ENG',
        game_situation == '5v3ENG' ~ 'ENG',
        game_situation == '4v6ENG' ~ 'ENG',
        game_situation == '4v5ENG' ~ 'ENG',
        game_situation == '4v4ENG' ~ 'ENG',
        game_situation == '4v3ENG' ~ 'ENG',
        game_situation == '3v6ENG' ~ 'ENG',
        game_situation == '3v5ENG' ~ 'ENG',
        game_situation == '3v4ENG' ~ 'ENG',
        game_situation == '3v3ENG' ~ 'ENG',
        game_situation == '5v5' & situation == 'EQENG' ~ 'ENG',
        game_situation == '4v4' & situation == 'EQENG' ~ 'ENG',
        game_situation == '3v3' & situation == 'EQENG' ~ 'ENG',

        game_situation == '6v6' ~ 'EV',
        game_situation == '5v5' ~ 'EV',
        game_situation == '4v4' ~ 'EV',
        game_situation == '3v3' ~ 'EV',

        game_situation == '6v5' & situation == 'EQ' ~ 'Goalie Pulled',
        game_situation == '5v4' & situation == 'EQ' ~ 'Goalie Pulled',
        game_situation == '4v3' & situation == 'EQ' ~ 'Goalie Pulled',

        game_situation == '5v4' ~ 'PP',
        game_situation == '5v3' ~ 'PP',
        game_situation == '4v3' ~ 'PP',
        game_situation == '6v4' ~ 'PP',
        game_situation == '6v3' ~ 'PP',

        game_situation == '4v5' ~ 'SH',
        game_situation == '3v5' ~ 'SH',
        game_situation == '3v4' ~ 'SH',

        game_situation == '1v0' & situation == 'GWS' ~ 'SO',
        game_situation == '1v0' & situation == 'PS' ~ 'PS'))


box_score_data_1920_condensed <- box_score_data_1920_raw %>%
  select(gameid, date, home_team, away_team, goal, primary_assist, secondary_assist, game_situation,
         game_situation_general, gf_team, gf_team_game_number, ga_team, ga_team_game_number, GF_names, GA_names,
         GF_skaters, GA_skaters, GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name,
         GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name)





# Player Stats ------------------------------------------------------------

#Imports
player_toi_data_1920 <- read.csv("player_toi_data_1920.csv", header = TRUE)
player_corsi_data_1920 <- read.csv("player_corsi_data_1920.csv", header = TRUE)

#5v5 Data
get_5v5_data <- function(player) {
  
  GP <- player_toi_data_1920 %>%
    filter(swehockey_name == player) %>%
    nrow()
  
  fenwick_data <- player_corsi_data_1920 %>%
    filter(swehockey_name == player) %>%
    select(FF, FA)
  
  FF <- sum(fenwick_data$FF)
  FA <- sum(fenwick_data$FA)

  G <- box_score_data_1920_condensed %>%
    filter(goal == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()

  A1 <- box_score_data_1920_condensed %>%
    filter(primary_assist == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()

  A2 <- box_score_data_1920_condensed %>%
    filter(secondary_assist == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()

  A <- A1 + A2

  P1 <- G + A1

  P <- G + A

  GF1 <- box_score_data_1920_condensed %>%
    filter(GF1_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GF2 <- box_score_data_1920_condensed %>%
    filter(GF2_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GF3 <- box_score_data_1920_condensed %>%
    filter(GF3_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GF4 <- box_score_data_1920_condensed %>%
    filter(GF4_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GF5 <- box_score_data_1920_condensed %>%
    filter(GF5_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GF6 <- box_score_data_1920_condensed %>%
    filter(GF6_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_1920_condensed %>%
    filter(GA1_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GA2 <- box_score_data_1920_condensed %>%
    filter(GA2_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GA3 <- box_score_data_1920_condensed %>%
    filter(GA3_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GA4 <- box_score_data_1920_condensed %>%
    filter(GA4_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GA5 <- box_score_data_1920_condensed %>%
    filter(GA5_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  GA6 <- box_score_data_1920_condensed %>%
    filter(GA6_name == player) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6

  `GF%` <- GF / (GF + GA)
  `GF%` <- round(`GF%` * 100, 1)

  IPP <- P / GF
  IPP <- round(IPP * 100, 1)
  
  FenSh <- round(GF/FF,3)
  FenSv <- 1 - round(GA/FA,3)

  data <- tibble(player = player, situation = 'EV', manpower = '5v5', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP, FenSh = FenSh, FenSv = FenSv)

  data <- merge(data, player_toi_data_1920[,c("swehockey_name", "shlse_team_name", "Pos")],
        by.x = "player", by.y = "swehockey_name") %>% unique()
  
  data <- data %>%
    rename(Team = shlse_team_name) %>%
    .[,c(1,17,18,2:16)]

  return(data)

}

player_5v5_data <- c()
for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {

  temp <- get_5v5_data(player)

  player_5v5_data <- rbind(player_5v5_data, temp)

  print(player)

}

#PP Data
get_pp_data <- function(player) {
  
  GP <- player_toi_data_1920 %>%
    filter(swehockey_name == player) %>%
    nrow()
  
  G <- box_score_data_1920_condensed %>%
    filter(goal == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  A1 <- box_score_data_1920_condensed %>%
    filter(primary_assist == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  A2 <- box_score_data_1920_condensed %>%
    filter(secondary_assist == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_1920_condensed %>%
    filter(GF1_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GF2 <- box_score_data_1920_condensed %>%
    filter(GF2_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GF3 <- box_score_data_1920_condensed %>%
    filter(GF3_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GF4 <- box_score_data_1920_condensed %>%
    filter(GF4_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GF5 <- box_score_data_1920_condensed %>%
    filter(GF5_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GF6 <- box_score_data_1920_condensed %>%
    filter(GF6_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_1920_condensed %>%
    filter(GA1_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GA2 <- box_score_data_1920_condensed %>%
    filter(GA2_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GA3 <- box_score_data_1920_condensed %>%
    filter(GA3_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GA4 <- box_score_data_1920_condensed %>%
    filter(GA4_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GA5 <- box_score_data_1920_condensed %>%
    filter(GA5_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GA6 <- box_score_data_1920_condensed %>%
    filter(GA6_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- GF / (GF + GA)
  `GF%` <- round(`GF%` * 100, 1)
  
  IPP <- P / GF
  IPP <- round(IPP * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = 'PP', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1920[,c("swehockey_name", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "swehockey_name") %>% unique()
  
  data <- data %>%
    rename(Team = shlse_team_name) #%>%
    #.[,c(1,17,18,2:16)]
  
  return(data)
  
}

player_pp_data <- c()
for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
  
  temp <- get_pp_data(player)
  
  player_pp_data <- rbind(player_pp_data, temp)
  
  print(player)
  
}

#SH Data
get_sh_data <- function(player) {
  
  GP <- player_toi_data_1920 %>%
    filter(swehockey_name == player) %>%
    nrow()
  
  G <- box_score_data_1920_condensed %>%
    filter(goal == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  A1 <- box_score_data_1920_condensed %>%
    filter(primary_assist == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  A2 <- box_score_data_1920_condensed %>%
    filter(secondary_assist == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_1920_condensed %>%
    filter(GF1_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GF2 <- box_score_data_1920_condensed %>%
    filter(GF2_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GF3 <- box_score_data_1920_condensed %>%
    filter(GF3_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GF4 <- box_score_data_1920_condensed %>%
    filter(GF4_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GF5 <- box_score_data_1920_condensed %>%
    filter(GF5_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  GF6 <- box_score_data_1920_condensed %>%
    filter(GF6_name == player) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_1920_condensed %>%
    filter(GA1_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GA2 <- box_score_data_1920_condensed %>%
    filter(GA2_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GA3 <- box_score_data_1920_condensed %>%
    filter(GA3_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GA4 <- box_score_data_1920_condensed %>%
    filter(GA4_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GA5 <- box_score_data_1920_condensed %>%
    filter(GA5_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  GA6 <- box_score_data_1920_condensed %>%
    filter(GA6_name == player) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- GF / (GF + GA)
  `GF%` <- round(`GF%` * 100, 1)
  
  IPP <- P / GF
  IPP <- round(IPP * 100, 1)
  
  data <- tibble(player = player, situation = 'SH', manpower = 'SH', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1920[,c("swehockey_name", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "swehockey_name") %>% unique()
  
  data <- data %>%
    rename(Team = shlse_team_name) #%>%
  #.[,c(1,17,18,2:16)]
  
  return(data)
  
}

player_sh_data <- c()
for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
  
  temp <- get_sh_data(player)
  
  player_sh_data <- rbind(player_sh_data, temp)
  
  print(player)
  
}

get_1v0_data <- function(player) {
  
  #GP <- player_toi_data_1920 %>%
    #filter(swehockey_name == player) %>%
    #nrow()
  
  G <- box_score_data_1920_condensed %>%
    filter(goal == player) %>%
    filter(game_situation == '1v0') %>%
    nrow()
  
  #A1 <- box_score_data_1920_condensed %>%
    #filter(primary_assist == player) %>%
    #filter(game_situation_general == 'SH') %>%
    #nrow()
  
  #A2 <- box_score_data_1920_condensed %>%
    #filter(secondary_assist == player) %>%
    #filter(game_situation_general == 'SH') %>%
    #nrow()
  
  #A <- A1 + A2
  
  #P1 <- G + A1
  
  #P <- G + A
  
  #GF1 <- box_score_data_1920_condensed %>%
    #filter(GF1_name == player) %>%
    #filter(game_situation_general == 'SH') %>%
    #nrow()
  # GF2 <- box_score_data_1920_condensed %>%
  #   filter(GF2_name == player) %>%
  #   filter(game_situation_general == 'SH') %>%
  #   nrow()
  # GF3 <- box_score_data_1920_condensed %>%
  #   filter(GF3_name == player) %>%
  #   filter(game_situation_general == 'SH') %>%
  #   nrow()
  # GF4 <- box_score_data_1920_condensed %>%
  #   filter(GF4_name == player) %>%
  #   filter(game_situation_general == 'SH') %>%
  #   nrow()
  # GF5 <- box_score_data_1920_condensed %>%
  #   filter(GF5_name == player) %>%
  #   filter(game_situation_general == 'SH') %>%
  #   nrow()
  # GF6 <- box_score_data_1920_condensed %>%
  #   filter(GF6_name == player) %>%
  #   filter(game_situation_general == 'SH') %>%
  #   nrow()
  # 
  # GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  # 
  # GA1 <- box_score_data_1920_condensed %>%
  #   filter(GA1_name == player) %>%
  #   filter(game_situation_general == 'PP') %>%
  #   nrow()
  # GA2 <- box_score_data_1920_condensed %>%
  #   filter(GA2_name == player) %>%
  #   filter(game_situation_general == 'PP') %>%
  #   nrow()
  # GA3 <- box_score_data_1920_condensed %>%
  #   filter(GA3_name == player) %>%
  #   filter(game_situation_general == 'PP') %>%
  #   nrow()
  # GA4 <- box_score_data_1920_condensed %>%
  #   filter(GA4_name == player) %>%
  #   filter(game_situation_general == 'PP') %>%
  #   nrow()
  # GA5 <- box_score_data_1920_condensed %>%
  #   filter(GA5_name == player) %>%
  #   filter(game_situation_general == 'PP') %>%
  #   nrow()
  # GA6 <- box_score_data_1920_condensed %>%
  #   filter(GA6_name == player) %>%
  #   filter(game_situation_general == 'PP') %>%
  #   nrow()
  # 
  # GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  # 
  # `GF%` <- GF / (GF + GA)
  # `GF%` <- round(`GF%` * 100, 1)
  # 
  # IPP <- P / GF
  # IPP <- round(IPP * 100, 1)
  
  data <- tibble(player = player, situation = '1v0', manpower = '1v0', G = G)
  
  data <- merge(data, player_toi_data_1920[,c("swehockey_name", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "swehockey_name") %>% unique()
  
  data <- data %>%
    rename(Team = shlse_team_name) #%>%
  #.[,c(1,17,18,2:16)]
  
  return(data)
  
}

player_1v0_data <- c()
for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
  
  temp <- get_1v0_data(player)
  
  player_1v0_data <- rbind(player_1v0_data, temp)
  
  print(player)
  
}




# Scratch Work ------------------------------------------------------------

fenwick_data <- player_corsi_data_1920 %>%
    group_by(swehockey_name) %>%
    summarise(FF = sum(FF), FA = sum(FA))

test <- merge(master_5v5_data, fenwick_data, by.x = 'player', by.y = 'swehockey_name')


test <- test %>%
  mutate(FenSh = round(GF/FF,3)) %>%
  mutate(FenSv = 1 - round(GA/FA,3)) %>%
  mutate(FenPDO = FenSh + FenSv)


write_excel_csv(box_score_data_1920_condensed, "box_score_data_1920_condensed.csv")

write_excel_csv(lineup_data_1920, "lineup_data_1920.csv")


team_5v5_goals <- box_score_data_1920_condensed %>%
  filter(game_situation == '5v5') %>%
  group_by(gf_team) %>%
  summarise(goal = count(goal))

team_5v5_goals <- merge(team_5v5_goals, shl_team_dictionary, by.x = "gf_team", by.y = "swehockey_team_name")



team_5v5_goals <- left_join(player_5v5_data, team_5v5_goals, by = c("Team" = "shlse_team_name"))


team_5v5_goals <- team_5v5_goals %>%
  select(player, manpower, Team, Pos, GP, P, GF, goal) %>%
  rename(team_goals = goal) %>%

player_5v5_contribution <- team_5v5_goals %>%
  mutate(GF_perc_on = round(GF / team_goals,2)) %>%
  mutate(GF_point_on = round(P / team_goals,2))


FenFor <- player_corsi_data_1920 %>%
  filter(swehockey_name == 'Kodie Curran') %>%
  sum(player_corsi_data_1920$FF)

scatterplot_data <- player_5v5_data %>%
  filter(GP > 9) %>%
  select(player, FenSh, FenSv)

scatterplot_data[is.na(scatterplot_data)] <- 0

ggplot(scatterplot_data, aes(x = FenSh, y = FenSv)) +
  geom_point() 

mean(scatterplot_data$FenSh)

# Old Functions -----------------------------------------------------------


# #All Situations Data
# get_all_sits_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'All Sits.', manpower = 'All Sits.', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'All Sits.', manpower = 'All Sits.', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_all_sits_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_all_sits_data(player)
#   
#   player_all_sits_data <- rbind(player_all_sits_data, temp)
#   
#   print(player)
#   
# }
# 
# #All EV Data
# get_ev_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation_general == 'EV') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation_general == 'EV') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation_general == 'EV') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation_general == 'EV') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation_general == 'EV') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'EV', manpower = 'EV', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation_general == 'EV') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation_general == 'EV') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation_general == 'EV') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation_general == 'EV') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation_general == 'EV') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'EV', manpower = 'EV', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_ev_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_ev_data(player)
#   
#   player_ev_data <- rbind(player_ev_data, temp)
#   
#   print(player)
#   
# }
# 
# #5v5 Data
# get_5v5_data <- function(player) {
#   
#   GP <- player_toi_data_1920 %>%
#     filter(swehockey_name == player) %>%
#     nrow()
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation == '5v5') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation == '5v5') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation == '5v5') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation == '5v5') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation == '5v5') %>%
#     nrow()
#   
#   `GF%` <- GF / (GF + GA)
#   `GF%` <- round(`GF%` * 100, 1)
#   
#   IPP <- P / GF
#   IPP <- round(IPP * 100, 1)
#   
#   data <- tibble(player = player, situation = 'EV', manpower = '5v5', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     GP <- player_toi_data_1920 %>%
#       filter(swehockey_name == 'Jesper Jensen') %>%
#       nrow()
#     
#     G <- box_score_data_1920_condensed %>%
#       filter(goal == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     
#     A1 <- box_score_data_1920_condensed %>%
#       filter(primary_assist == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     
#     A2 <- box_score_data_1920_condensed %>%
#       filter(secondary_assist == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     
#     A <- A1 + A2
#     
#     P1 <- G + A1
#     
#     P <- G + A
#     
#     GF1 <- box_score_data_1920_condensed %>%
#       filter(GF1_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GF2 <- box_score_data_1920_condensed %>%
#       filter(GF2_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GF3 <- box_score_data_1920_condensed %>%
#       filter(GF3_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GF4 <- box_score_data_1920_condensed %>%
#       filter(GF4_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GF5 <- box_score_data_1920_condensed %>%
#       filter(GF5_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GF6 <- box_score_data_1920_condensed %>%
#       filter(GF6_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     
#     GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
#     
#     GA1 <- box_score_data_1920_condensed %>%
#       filter(GA1_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GA2 <- box_score_data_1920_condensed %>%
#       filter(GA2_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GA3 <- box_score_data_1920_condensed %>%
#       filter(GA3_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GA4 <- box_score_data_1920_condensed %>%
#       filter(GA4_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GA5 <- box_score_data_1920_condensed %>%
#       filter(GA5_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     GA6 <- box_score_data_1920_condensed %>%
#       filter(GA6_name == 'Jesper Jensen') %>%
#       filter(game_situation == '5v5') %>%
#       nrow()
#     
#     GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
#     
#     `GF%` <- GF / (GF + GA)
#     `GF%` <- round(`GF%` * 100, 1)
#     
#     IPP <- P / GF
#     IPP <- round(IPP * 100, 1)
#     
#     data <- tibble(player = player, situation = 'EV', manpower = '5v5', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                    IPP = IPP)
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_5v5_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_5v5_data(player)
#   
#   player_5v5_data <- rbind(player_5v5_data, temp)
#   
#   print(player)
#   
# }
# 
# #4v4 Data
# get_4v4_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation == '4v4') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation == '4v4') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation == '4v4') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation == '4v4') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation == '4v4') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'EV', manpower = '4v4', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation == '4v4') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation == '4v4') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation == '4v4') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation == '4v4') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation == '4v4') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'EV', manpower = '4v4', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_4v4_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_4v4_data(player)
#   
#   player_4v4_data <- rbind(player_4v4_data, temp)
#   
#   print(player)
#   
# }
# 
# #3v3 Data
# get_3v3_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation == '3v3') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation == '3v3') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation == '3v3') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation == '3v3') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation == '3v3') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'EV', manpower = '3v3', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation == '3v3') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation == '3v3') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation == '3v3') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation == '3v3') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation == '3v3') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'EV', manpower = '3v3', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_3v3_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_3v3_data(player)
#   
#   player_3v3_data <- rbind(player_3v3_data, temp)
#   
#   print(player)
#   
# }
# 
# #All PP Data
# get_pp_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation_general == 'PP') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation_general == 'PP') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation_general == 'PP') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation_general == 'PP') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation_general == 'SH') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'PP', manpower = 'PP', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation_general == 'PP') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation_general == 'PP') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation_general == 'PP') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation_general == 'PP') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation_general == 'SH') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'PP', manpower = 'PP', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_pp_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_pp_data(player)
#   
#   player_pp_data <- rbind(player_pp_data, temp)
#   
#   print(player)
#   
# }
# 
# #5v4 Data
# get_5v4_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation == '5v4') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation == '5v4') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation == '5v4') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation == '5v4') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation == '4v5') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'PP', manpower = '5v4', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation == '5v4') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation == '5v4') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation == '5v4') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation == '5v4') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation == '4v5') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'PP', manpower = '5v4', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_5v4_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_5v4_data(player)
#   
#   player_5v4_data <- rbind(player_5v4_data, temp)
#   
#   print(player)
#   
# }
# 
# #5v3 Data
# get_5v3_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation == '5v3') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation == '5v3') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation == '5v3') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation == '5v3') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation == '3v5') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'PP', manpower = '5v3', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation == '5v3') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation == '5v3') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation == '5v3') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation == '5v3') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation == '3v5') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'PP', manpower = '5v3', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_5v3_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_5v3_data(player)
#   
#   player_5v3_data <- rbind(player_5v3_data, temp)
#   
#   print(player)
#   
# }
# 
# #4v3 Data
# get_4v3_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation == '4v3') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation == '4v3') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation == '4v3') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation == '4v3') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation == '3v4') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'PP', manpower = '4v3', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation == '4v3') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation == '4v3') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation == '4v3') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation == '4v3') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation == '3v4') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'PP', manpower = '4v3', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_4v3_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_4v3_data(player)
#   
#   player_4v3_data <- rbind(player_4v3_data, temp)
#   
#   print(player)
#   
# }
# 
# #All SH Data
# get_sh_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation_general == 'SH') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation_general == 'SH') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation_general == 'SH') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation_general == 'SH') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation_general == 'PP') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'SH', manpower = 'SH', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation_general == 'SH') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation_general == 'SH') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation_general == 'SH') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation_general == 'SH') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation_general == 'PP') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'SH', manpower = 'SH', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_sh_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_sh_data(player)
#   
#   player_sh_data <- rbind(player_sh_data, temp)
#   
#   print(player)
#   
# }
# 
# #4v5 Data
# get_4v5_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation == '4v5') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation == '4v5') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation == '4v5') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation == '4v5') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation == '5v4') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'SH', manpower = '4v5', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation == '4v5') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation == '4v5') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation == '4v5') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation == '4v5') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation == '5v4') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'SH', manpower = '4v5', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_4v5_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_4v5_data(player)
#   
#   player_4v5_data <- rbind(player_4v5_data, temp)
#   
#   print(player)
#   
# }
# 
# #3v5 Data
# get_3v5_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation == '3v5') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation == '3v5') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation == '3v5') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation == '3v5') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation == '5v3') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'SH', manpower = '3v5', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation == '3v5') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation == '3v5') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation == '3v5') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation == '3v5') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation == '5v3') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'SH', manpower = '3v5', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_3v5_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_3v5_data(player)
#   
#   player_3v5_data <- rbind(player_3v5_data, temp)
#   
#   print(player)
#   
# }
# 
# #1v0 Data
# get_1v0_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation == '1v0') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation == '1v0') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation == '1v0') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation == '1v0') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation == '1v0') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = '1v0', manpower = '1v0', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation == '1v0') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation == '1v0') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation == '1v0') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation == '1v0') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation == '1v0') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = '1v0', manpower = '1v0', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_1v0_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_1v0_data(player)
#   
#   player_1v0_data <- rbind(player_1v0_data, temp)
#   
#   print(player)
#   
# }
# 
# #1v0 Data
# get_eng_data <- function(player) {
#   
#   G <- box_score_data_1920_condensed %>%
#     subset(grepl(player, goal)) %>%
#     filter(game_situation_general == 'ENG') %>%
#     nrow()
#   
#   A1 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, primary_assist)) %>%
#     filter(game_situation_general == 'ENG') %>%
#     nrow()
#   
#   A2 <- box_score_data_1920_condensed %>%
#     subset(grepl(player, secondary_assist)) %>%
#     filter(game_situation_general == 'ENG') %>%
#     nrow()
#   
#   A <- A1 + A2
#   
#   P1 <- G + A1
#   
#   P <- G + A
#   
#   GF <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GF_names)) %>%
#     filter(game_situation_general == 'ENG') %>%
#     nrow()
#   
#   GA <- box_score_data_1920_condensed %>%
#     subset(grepl(player, GA_names)) %>%
#     filter(game_situation_general == 'ENG') %>%
#     nrow()
#   
#   `GF%` <- round(GF / (GF + GA),2)
#   
#   IPP <- round(P/GF, 2)
#   
#   data <- tibble(player = player, situation = 'ENG', manpower = 'ENG', G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
#                  IPP = IPP)
#   
#   if(player == 'Jesper Jensen') {
#     
#     G.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', goal)) %>%
#       filter(game_situation_general == 'ENG') %>%
#       nrow()
#     
#     A1.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', primary_assist)) %>%
#       filter(game_situation_general == 'ENG') %>%
#       nrow()
#     
#     A2.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', secondary_assist)) %>%
#       filter(game_situation_general == 'ENG') %>%
#       nrow()
#     
#     A.2 <- A1.2 + A2.2
#     
#     P1.2 <- G.2 + A1.2
#     
#     P.2 <- G.2 + A.2
#     
#     GF.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GF_names)) %>%
#       filter(game_situation_general == 'ENG') %>%
#       nrow()
#     
#     GA.2 <- box_score_data_1920_condensed %>%
#       subset(grepl('Jesper Jensen Aabo', GA_names)) %>%
#       filter(game_situation_general == 'ENG') %>%
#       nrow()
#     
#     data <- tibble(player = player, situation = 'ENG', manpower = 'ENG', G = G - G.2, A = A - A.2,  A1 = A1 - A1.2, A2 = A2 - A2.2,  P = P - P.2, P1 = P1 - P1.2, 
#                    GF = GF - GF.2, GA = GA - GA.2)
#     
#     data <- data %>%
#       mutate(`GF%` = round(GF / (GF + GA),2), IPP = IPP <- round(P/GF, 2))
#     
#   }
#   
#   return(data)
#   
# }
# 
# player_eng_data <- c()
# for(player in player_lookup_1920$swehockey[1:length(player_lookup_1920$swehockey)]) {
#   
#   temp <- get_eng_data(player)
#   
#   player_eng_data <- rbind(player_eng_data, temp)
#   
#   print(player)
#   
# }
# 
# #Master Table
# master_swehockey_data <- rbind(player_all_sits_data, player_ev_data, player_5v5_data, player_3v3_data,
#                                player_pp_data, player_5v4_data, player_sh_data,
#                                player_4v5_data, player_1v0_data, player_eng_data)