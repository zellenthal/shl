library(plyr)
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



# Player Info -------------------------------------------------------------


#player lookup
player_lookup <- read.csv("player_lookup.csv", stringsAsFactors = FALSE)

player_lookup_1920 <- read.csv("player_lookup_1920.csv", stringsAsFactors = FALSE)

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
             "Anders Lindbäck", "Dan Bakala", "Atte Engren", "Henrik Haukeland", "Arvid Söderblöm", "Jonas Johansson",
             "Johan Mattsson", "Christian Engstrand", "Ville Kolppanen", "Eero Kilpeläinen", "Mattias Pettersson",
             "Emil Kruse", "Oscar Masiello", "Isak Mantler", "Jhonas Enroth", "Adam Werner", "Tomi Karhunen", "Isak Wallin",
             "Victor Brattström", "Linus Marcko", "Jesper Myrenberg", "Stefan Steen", "Olle Eriksson Ek", "Victor Östman",
             "Isac Olsson", "Eric Forsberg", "Isak Ragnevad", "Tobias Forsgren", "Oscar Walldén", "Magnus Åkerlund",
             "Robin Christofferson", "Oliver Norgren", "Arvid Ljung", "Linus Gidbark", "Emil Kruse", "William Josbrant",
             "Isak Wallin", "Oscar Fröberg", "David Elmgren", "Isak Mantler", "John Vestermark", "John Westermark", "Gustav Walldén",
             "Olof Lindbom", "Dennis Hildeby", "Henrik Lundberg", "Oliver Dackell", "Joel Svensson", "Arvid Söderblom", "Anton Malmborg",
             "Joel Vännström", "Pontus Eltonius", "Gustav Bågnevik", "Jonatan Stålberg", "Jonatan Frykholm", "Viktor Andersson", "Rupert Svärd",
             "Tim Hultstrand", "Rasmus Hedström", "Joren van Pottelberghe", "John Morelius", "Emil Hedman", "Thomas Lillie", "Filip Ternbom",
             "Jonas Johansson", "Hampus Alexandersson", "Tim Sandberg", "Albin Enqvist", "Jesper Eriksson", "Jonas Fransson")



# Season URLs -------------------------------------------------------------

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

# Functions ----------------------------------------------------------------


#this is pretty good, but it takes a lot of manual cleaning up front in terms of player names and whatnot
#also can be more efficient in terms of lines of code
get_lineup <- function(id) {
  
  url <- read_html(paste('http://stats.swehockey.se/Game/LineUps/', id, sep=''))
  
  #team data
  # teams <- url %>%
  #   html_nodes(xpath='//th/h3') %>%
  #   html_text() %>%
  #   tibble::enframe(name = NULL) 
  # 
  #   #remove the last 17 characters  
  #   home_team <- gsub('.{17}$', '', teams$value[1])
  #   away_team <- gsub('.{17}$', '', teams$value[2])
  
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
    mutate(uid = paste(jersey, team, date, sep = '')) -> lineup_final
  
  lineup_final <- lineup_final[,c(5,3,4,1,2,6)]
  
  return(lineup_final)
  
}

#tester
#get_lineup(gameIDs[1]) %>% view()

get_lineup('301796') %>% view()


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
for(id in gameID_table_1920$id[1:203]) {
  
  temp <- get_lineup(id)
  
  lineup_data_1920 <- rbind(lineup_data_1920, temp)
  
  print(id)
  rm(temp)
  
}

#manual edits
lineup_data_1920v2 <- read.csv("lineup_data_1920_v2.csv")

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
    mutate(gameid = id) -> box_score_data
  
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
  }
  
  if(ncol(gf) == 1 & ncol(ga) == 1) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''))
  }
  
  return(box_score_data)
  
}

get_box_score_1920 <- function(id) {
  
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
    left_join(swehockey_teams_1920, by = c("team" = "abbreviation_1920")) %>%
    rename(gf_team = full_name_1920) %>%
    mutate(ga_team = ifelse(gf_team == home_team, away_team, home_team)) %>%
    mutate(date = date) %>%
    mutate(gameid = id) -> box_score_data
  
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
  }
  
  if(ncol(gf) == 1 & ncol(ga) == 1) {
    box_score_data <- box_score_data %>%
      mutate(GF1_uid = paste(GF1, gf_team, date, sep = ''),
             GA1_uid = paste(GA1, ga_team, date, sep = ''))
  }
  
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
for (id in gameID_table_1920$id[1:203]) {
  
  temp <- get_box_score_1920(id)
  
  box_score_data_1920 <- rbind.fill(box_score_data_1920, temp)
  
  print(id)
  rm(temp)
  
}

#Enriching box score data

#test df
box_score_data_1920v2 <- box_score_data_1920

#Concatenation of all GF & GA UIDs
box_score_data_1920v2 %>%
  mutate(GF_string = paste(GF1_uid, GF2_uid, GF3_uid, GF4_uid, GF5_uid, GF6_uid, sep = ''),
         GA_string = paste(GA1_uid, GA2_uid, GA3_uid, GA4_uid, GA5_uid, GA6_uid, sep = '')) -> box_score_data_1920v2

#Join in Player names
#GF names
merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GF1_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GF2_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GF3_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GF4_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GF5_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GF6_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GF6_name = player) -> box_score_data_1920v2

#GA names
merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GA1_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GA2_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GA3_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GA4_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GA5_name = player) -> box_score_data_1920v2

merge(box_score_data_1920v2, lineup_data_1920v2[,c("player", "uid")], by.x = "GA6_uid", by.y = "uid", all.x = TRUE) %>%
  rename(GA6_name = player) -> box_score_data_1920v2

box_score_data_1920v2 %>%
  mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name, sep = ''),
         GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name, sep = '')) -> box_score_data_1920v2

box_score_data_1920v2 %>%
  mutate(GF_GK = str_count(box_score_data_1920v2$GF_names, "GK"),
         GF_NA = str_count(box_score_data_1920v2$GF_names, "NA")) %>%
  
  mutate(GF_skaters = 6 - GF_GK - GF_NA) %>% 
  
  mutate(GA_GK = str_count(box_score_data_1920v2$GA_names, "GK"),
         GA_NA = str_count(box_score_data_1920v2$GA_names, "NA")) %>%
  
  mutate(GA_skaters = 6 - GA_GK - GA_NA) -> box_score_data_1920v2


#assess situation
box_score_data_1920v2 %>%
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
    
    GF_skaters == 1 & GA_skaters == 0 ~ '1v0')) -> box_score_data_1920v2

box_score_data_1920v2 %>%
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
    game_situation == '1v0' & situation == 'PS' ~ 'PS')) -> box_score_data_1920v2




box_score_data_1920v2 %>%
  select(gameid, date, home_team, away_team, team, goal_state, situation, goal, primary_assist, secondary_assist,
         gf_team, ga_team, GF_string, GA_string, GF_names, GA_names, GF_skaters, GA_skaters, 
         game_situation, game_situation_general) -> box_score_data_1920v2_condensed

box_score_data_1920v2_condensed %>%
  mutate(combined_teams = paste(home_team, away_team, sep = '-')) %>% 
  mutate(combined_teams_date = paste(combined_teams, date, sep = '')) -> box_score_data_1920v2_condensed

box_score_data_1920v2_condensed %>%
  mutate(home_team_date = paste(home_team, date, sep = ''),
         away_team_date = paste(away_team, date, sep = ''),
         gf_team_date = paste(gf_team, date, sep = ''),
         ga_team_date = paste(ga_team, date, sep = '')) -> box_score_data_1920v2_condensed





merge(box_score_data_1920v2_condensed, master_team_schedule_1920[,c("swehockey_team_date", "game_number")],
      by.x = "gf_team_date", by.y = "swehockey_team_date") -> box_score_data_1920v2_condensed





get_5v5_data <- function(player) {
  
  G <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, goal)) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  
  A1 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, primary_assist)) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  
  A2 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, secondary_assist)) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GF_names)) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  
  GA <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GA_names)) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA),2)
  
  data <- tibble(player = player, G = G, A1 = A1, A2 = A2, A = A, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`)
  
  return(data)
  
}

player_5v5_data <- c()
for(player in player_lookup_1920$swehockey[1:391]) {
  
  temp <- get_5v5_data(player)
  
  player_5v5_data <- rbind(player_5v5_data, temp)
  
}


get_ev_data <- function(player) {
  
  G <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, goal)) %>%
    filter(game_situation_general == 'EV') %>%
    nrow()
  
  A1 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, primary_assist)) %>%
    filter(game_situation_general == 'EV') %>%
    nrow()
  
  A2 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, secondary_assist)) %>%
    filter(game_situation_general == 'EV') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GF_names)) %>%
    filter(game_situation_general == 'EV') %>%
    nrow()
  
  GA <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GA_names)) %>%
    filter(game_situation_general == 'EV') %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA),2)
  
  data <- tibble(player = player, G = G, A1 = A1, A2 = A2, A = A, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`)
  
  return(data)
  
}

player_ev_data <- c()
for(player in player_lookup_1920$swehockey[1:391]) {
  
  temp <- get_ev_data(player)
  
  player_ev_data <- rbind(player_ev_data, temp)
  
}

get_pp_data <- function(player) {
  
  G <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, goal)) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  A1 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, primary_assist)) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  A2 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, secondary_assist)) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GF_names)) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  GA <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GA_names)) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA),2)
  
  data <- tibble(player = player, G = G, A1 = A1, A2 = A2, A = A, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`)
  
  return(data)
  
}

player_pp_data <- c()
for(player in player_lookup_1920$swehockey[1:391]) {
  
  temp <- get_pp_data(player)
  
  player_pp_data <- rbind(player_pp_data, temp)
  
}

get_sh_data <- function(player) {
  
  G <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, goal)) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  A1 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, primary_assist)) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  A2 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, secondary_assist)) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GF_names)) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  GA <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GA_names)) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA),2)
  
  data <- tibble(player = player, G = G, A1 = A1, A2 = A2, A = A, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`)
  
  return(data)
  
}

player_sh_data <- c()
for(player in player_lookup_1920$swehockey[1:391]) {
  
  temp <- get_sh_data(player)
  
  player_sh_data <- rbind(player_sh_data, temp)
  
}

get_eng_data <- function(player) {
  
  G <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, goal)) %>%
    filter(game_situation_general == 'ENG') %>%
    nrow()
  
  A1 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, primary_assist)) %>%
    filter(game_situation_general == 'ENG') %>%
    nrow()
  
  A2 <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, secondary_assist)) %>%
    filter(game_situation_general == 'ENG') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GF_names)) %>%
    filter(game_situation_general == 'ENG') %>%
    nrow()
  
  GA <- box_score_data_1920v2_condensed %>%
    subset(grepl(player, GA_names)) %>%
    filter(game_situation_general == "ENG") %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA),2)
  
  data <- tibble(player = player, G = G, A1 = A1, A2 = A2, A = A, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`)
  
  return(data)
  
}

player_eng_data <- c()
for(player in player_lookup_1920$swehockey[1:391]) {
  
  temp <- get_eng_data(player)
  
  player_eng_data <- rbind(player_eng_data, temp)
  
}


box_score_data_1920v2_condensed %>%
  subset(grepl("Andreas Wingerli", GA_names)) %>%
  #subset(grepl("EV", game_situation_general)) %>%
  #filter(game_situation_general == 'EV') %>%
  filter(game_situation == '5v5') %>%
  nrow()

box_score_data_1920v2_condensed %>%
  subset(grepl("Mattias Guter", GA_names)) %>%
  #filter(game_situation_general == 'PP') %>%
  view()



