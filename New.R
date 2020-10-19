
# Intro -------------------------------------------------------------------

#--------------------------
# Install required packages
#--------------------------

library(httr) #for EP API
library(jsonlite) #for EP API
library(xml2) #for EP API

library(lubridate) #for working with dates

# EP API Function --------------------------------------------------------

ep_api <- function(path) {
  url <- modify_url("https://api.eliteprospects.com/v1/", path = path)
  GET(url)
}

# Team Info ---------------------------------------------------------------

#shl.se display names
shlse_team_names <- c("AIK", "Brynäs", "Djurgården", "Frölunda", "Färjestad", "HV71", "Karlskrona",
                      "Leksand", "Linköping", "Luleå", "Malmö", "MODO", "Mora", "Oskarshamn", "Örebro",
                      "Rögle", "Skellefteå", "Södertälje", "Timrå", "Växjö")

#shl.se display names (when they display in double in stats tables)
shlse_team_names_double <- c("AIKAIK", "BrynäsBIF", "DjurgårdenDIF", "FrölundaFHC", "FärjestadFBK",
                             "HV71HV71", "KarlskronaKHK", "LeksandLIF", "LinköpingLHC", "LuleåLHF",
                             "MalmöMIF", "MODOMODO", "MoraMIK", "OskarshamnIKO", "ÖrebroÖRE", 
                             "RögleRBK", "SkellefteåSKE", "xxx", "TimråTIK", "VäxjöVLH")

#shl.se team IDs
shlse_team_ids <- c("f7c8-f7c8isEb3", "1ab8-1ab8bfj7N", "2459-2459QTs1f", "087a-087aTQv9u",
                    "752c-752c12zB7Z", "3db0-3db09jXTE", "f51e-f51eM6sbL", "9541-95418PpkP",
                    "41c4-41c4BiYZU", "1a71-1a71gTHKh", "8e6f-8e6fUXJvi", "110b-110bJcIAI",
                    "dcba-dcbaXSrRU", "259b-259bYGVIp", "82eb-82ebmgaJ8", "ee93-ee93uy4oW",
                    "50e6-50e6DYeWM", "936a-936aAY8bT", "31d1-31d1NbSlR", "fe02-fe02mf1FN")

#swehockey display names
swehockey_team_names <- c("AIK", "Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK",
                          "HV 71", "Karlskrona HK", "Leksands IF", "Linköping HC", "Luleå HF",
                          "IF Malmö Redhawks", "MODO Hockey", "Mora IK", "IK Oskarshamn",
                          "Örebro HK", "Rögle BK", "Skellefteå AIK", "Södertälje SK", "Timrå IK", 
                          "Växjö Lakers HC")

#my own team abbreviations (scorebug abbreviations)
shl_team_abbreviations <- c("aik", "bif", "dif", "fhc", "fbk", "hv", "khk", "lif", 
                            "lhc", "lhf", "mif", "modo", "mik","iko", "ohk", "rbk", 
                            "ske", "ssk", "tik", "vax")

#team dictionary for all name variations (lookup table)
shl_team_dictionary <- tibble(shlse_team_name = shlse_team_names,
                              shlse_team_name_double = shlse_team_names_double,
                              swehockey_team_name = swehockey_team_names,
                              shl_team_abbreviation = shl_team_abbreviations,
                              shlse_team_id = shlse_team_ids)

abbreviations <- c("BIF", "DIF", "FRÖ", "FBK", "HV71", "KAR", "LIF", "LHC", "LHF", "MIF", "MIK", "MODO", "IKO",
                   "ÖHK", "RBK", "SKE", "SAIK", "TIK", "VÄX", "IFB", "VIK", "BIK", "VIT", "SSK", "TAIF", "VÄS",
                   "KRI", "AIS", "AIK", "VäIK")

team_names <- c("Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK", "HV 71", "Karlskrona HK", "Leksands IF",
                "Linköping HC", "Luleå HF", "IF Malmö Redhawks", "Mora IK", "MODO Hockey", "IK Oskarshamn", "Örebro HK",
                "Rögle BK", "Skellefteå AIK", "Skellefteå AIK", "Timrå IK", "Växjö Lakers HC", "IF Björklöven", "Västerås IK",
                "BIK Karlskoga", "HC Vita Hästen", "Södertälje SK", "Tingsryds AIF", "Västerviks IK", "Kristianstads IK",
                "Almtuna IS", "AIK", "Väsby IK HK")

swehockey_teams <- tibble(abbreviation = abbreviations[1:30], full_name = team_names[1:30])

nhl_full <- c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins", "Buffalo Sabres", "Calgary Flames", "Carolina Hurricanes",
              "Chicago Blackhawks", "Colorado Avalanche", "Columbus Blue Jackets", "Dallas Stars", "Detroit Red Wings",
              "Edmonton Oilers", "Florida Panthers", "Los Angeles Kings", "Minnesota Wild", "Montréal Canadiens", "Nashville Predators",
              "New Jersey Devils", "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins",
              "San Jose Sharks", "St. Louis Blues", "Tampa Bay Lightning", "Toronto Maple Leafs", "Vancouver Canucks", "Vegas Golden Knights",
              "Washington Capitals", "Winnipeg Jets")

nhl_abbrev <- c("ANA", "ARI", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL",
                "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "VGK", "WSH", "WPG")

nhl_teams <- tibble(full = nhl_full, abbrev = nhl_abbrev)


# SHL Player DB -----------------------------------------------------------

#20/21
shl_playerdb_path <- "/v1/leagues/shl/player-stats?limit=1000&sort=-regularStats.PTS&season=2020-2021&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H"

shl_playerdb_response <- ep_api(shl_playerdb_path)

shl_playerdb_jsonRespText <- content(shl_playerdb_response, as = "text", encoding = "UTF-8") 
shl_playerdb_df <- fromJSON(shl_playerdb_jsonRespText)
shl_playerdb_tibble <- as_tibble(shl_playerdb_df$data)

shl_playerdb_2021 <- tibble(ep_id = shl_playerdb_tibble$player$id,
                       ep_name = shl_playerdb_tibble$player$name,
                       dob = lubridate::as_date(shl_playerdb_tibble$player$dateOfBirth),
                       nationality = shl_playerdb_tibble$player$nationality$name,
                       pos = shl_playerdb_tibble$player$position,
                       season = shl_playerdb_tibble$season$slug,
                       team = shl_playerdb_tibble$team$name,
                       gp = shl_playerdb_tibble$regularStats$GP,
                       age = round(lubridate::time_length(lubridate::as_date("2020-09-15") - dob, "year"), 1),
                       age2 = trunc(lubridate::time_length(lubridate::as_date("2020-09-15") - dob, "year"))
                       )

shl_playerdb_master <- rbind(shl_playerdb_1718, shl_playerdb_1819, shl_playerdb_1920, shl_playerdb_2021)

#NHL Rights
path <- "/v1/nhl-rights?limit=100&sort=player&player=350702&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H"

response <- ep_api(path)

jsonRespText <- content(response, as = "text", encoding = "UTF-8") 
df <- fromJSON(jsonRespText)
tibble <- as_tibble(df$data)

shl_nhl_rights <- c()
for(id in shl_playerdb_master$ep_id[1:length(shl_playerdb_master$ep_id)]) {
  
  path <- paste0("/v1/nhl-rights?limit=100&sort=player&player=", id, "&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H")
  
  response <- ep_api(path)
  
  jsonRespText <- content(response, as = "text", encoding = "UTF-8") 
  df <- fromJSON(jsonRespText)
  tibble <- as_tibble(df$data)
  
  temp <- tibble(playerid = tibble$player$id,
                 player = tibble$player$name,
                 nhl_team = tibble$team$name)
  
  shl_nhl_rights <- rbind(shl_nhl_rights, temp)
  
  rm(temp)
  
}

shl_nhl_rights <- shl_nhl_rights %>%
  dplyr::distinct()

shl_nhl_rights <- left_join(shl_nhl_rights, nhl_teams, by = c("nhl_team" = "full"))


# Player Info -------------------------------------------------------------

#player reference data
# player_lookup <- read.csv("player_lookup.csv", stringsAsFactors = FALSE)
# 
# player_lookup_1920 <- read.csv("player_lookup_1920.csv", stringsAsFactors = FALSE)

#update each refresh with net new players
# player_lookup_1920_update <- tibble(swehockey = c("Leo Lööf", "Adam Arvedson"),
#                                     shlse = c("Leo Lööf", "Adam Arvedson"))
# 
# player_lookup_1920 <- rbind(player_lookup_1920, player_lookup_1920_update) 

#player reference data 20/21
player_lookup_2021 <- read.csv("player_lookup_2021.csv", stringsAsFactors = FALSE)

player_lookup_2021_update <- tibble(swehockey = c("Linus Johnsson"),
                                    shlse = c("Linus Johnsson"))

player_lookup_2021 <- rbind(player_lookup_2021, player_lookup_2021_update)

#order the lookup table in alphabetical order
player_lookup_2021 <- player_lookup_2021 %>%
  plyr::arrange(swehockey)


#V2 w/ EP
player_lookup_2021_v2 <- read.csv("player_lookup_2021_v2.csv", stringsAsFactors = FALSE)

#re-set the table each time, so future left joins work sine
player_lookup_2021_v2 <- player_lookup_2021_v2[,c(1:3)]

player_lookup_2021_update_v2 <- tibble(swehockey = c("Linus Johnsson"),
                                       shlse = c("Linus Johnsson"), 
                                       ep = c("Linus Johnsson"))

player_lookup_2021_v2 <- rbind(player_lookup_2021_v2, player_lookup_2021_update_v2)


player_lookup_2021_v2 <- left_join(player_lookup_2021_v2, shl_nhl_rights, by = c("ep" = "player")) %>%
    select(-c(playerid, nhl_team))

player_lookup_2021_v2[is.na(player_lookup_2021_v2)] <- "None"

#17/18
player_lookup_1718_v2 <- read.csv("player_lookup_1718_v2.csv", stringsAsFactors = FALSE)

player_lookup_1718_v2 <- left_join(player_lookup_1718_v2, shl_nhl_rights, by = c("ep" = "player")) %>%
  select(-c(playerid, nhl_team))

player_lookup_1718_v2[is.na(player_lookup_1718_v2)] <- "None"

#18/19
player_lookup_1819_v2 <- read.csv("player_lookup_1819_v2.csv", stringsAsFactors = FALSE)

player_lookup_1819_v2 <- left_join(player_lookup_1819_v2, shl_nhl_rights, by = c("ep" = "player")) %>%
  select(-c(playerid, nhl_team))

player_lookup_1819_v2[is.na(player_lookup_1819_v2)] <- "None"


#19/20
player_lookup_1920_v2 <- read.csv("player_lookup_1920_v2.csv", stringsAsFactors = FALSE)

player_lookup_1920_v2 <- left_join(player_lookup_1920_v2, shl_nhl_rights, by = c("ep" = "player")) %>%
  select(-c(playerid, nhl_team))

player_lookup_1920_v2[is.na(player_lookup_1920_v2)] <- "None"

# Goalies -----------------------------------------------------------------


#list of all goalies (keep this updated -- used in box score function)
goalies <- c("Niklas Rubin", "Roman Will", "Joel Lassinantti", "Jonas Gunnarsson", "Oscar Alsenfelt",
             "Gustaf Lindvall", "Christoffer Rifalk", "Johan Mattsson", "Dominik Furch", "Niklas Svedberg",
             "Viktor Andrén", "Jonas Gustavsson", "Arvid Holm", "Tex Williamsson", "Fredrik Pettersson-Wentzel",
             'Fredrik Petterson-Wentzel', "Viktor Fasth", "Axel Brage", "Karri Rämö", "Samuel Ersson", "Lars Volden", "Markus Svensson",
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
             "Isac Olsson", "Isac Ohlsson", "Alexander Sahlin", "Jakob Hellsten", "Richard Bachman", "Joe Cannata", "Samuel Ward", "Sam Ward",
             "Jesper Myrenberg", "Tanner Jaillet", "Tim Juel", "Mika Järvinen", "Linus Lundin", "Daniel Rosengren", "Justin Pogge", "Jesper Eliasson",
             "Isak Wallin", "Henrik Lundberg", "Andreas Ljunggren", "Viktor Kokman", "Antti Lehtonen", "Marcus Hellgren", "Olof Lindbom",
             "Emil Kruse", "Fredrik Bergvik", "Arvid Ljung", "Oscar Masiello", "Adam Vay", "August Hedlund", "Oliver Dackell", "Niklas Lundström",
             "Daniel Marmenlind", "Mattias Pettersson", "Adam Ohre", "Albin Andersson", "Patrik Bartosak", "Joel Gistedt", "Oliver Tornerefelt",
             "Robin Wallin", "Matteus Ward", "Edvin Olofsson", "Jesper Vikman", "Christian Engstrand", "Oliver Norgren", "Dan Bakala", 
             "Oliver Håkanson", "Alexander Skepp", "Kristoffer Solstad", "Jonathan Stålberg", "Hampus Nivalainen", "Emil Ranehamn", "Oscar Leijon",
             "Andreas Henriksson", "Philip Aldenrud", "Rasmus Eriksson", "Viktor Rönnqvist", "Emil Cederlund", "Jonas Eneroth", "Max Nilsson",
             "Edvard Nordlund", "Theo Rooseboom de Vries", "Isak Mantler", "Martin Svedberg", "Jesper Wallstedt", "Linus Ryttar", "Sebastian Andersson",
             "Claes Endre", "Anthony Peters", "Zackarias Skog", "Julius Pohjanoksa", "Kevin Poulin", "William Jupiter", "Hampus Hedman", "Joel Pettersson",
             "Jussi Rynnäs", "Kristoffer Ericson", "Carl Olsson", "Filip Larsson", "Hampus Vallin", "Oscar Darnell", "Isak Posch", "Hugo Marklund",
             "Emil Ranehamn", "Petter Johansson")


# Games & Schedule --------------------------------------------------------

#swehockey

#SHL - Box Scores

#all SHL box score URLs since the 2015/2016 season on
box_score_urls <- get_schedule("SHL", c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21"))

#all SHL game IDs since the 2015/2016 season (exctracted from above)
gameIDs <- str_split(box_score_urls$url, "/", simplify = TRUE)[,6]
gameID_table <- tibble(id = gameIDs, season = box_score_urls$season, league = "SHL")

#SHL - Lineups

#create lineup URLs
lineup_slug <- "https://stats.swehockey.se/Game/LineUps/"

lineup_urls <- tibble(url = paste(lineup_slug, gameID_table$id, sep = ''), 
                      league = gameID_table$league, 
                      season = gameID_table$season)

#SHL - Season URL Bundles

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

#20/21 URLs
box_score_urls_2021 <- subset(box_score_urls, grepl("2020-21", box_score_urls$season))
lineup_urls_2021 <- subset(lineup_urls, grepl("2020-21", lineup_urls$season))
gameIDs_2021 <- str_split(box_score_urls_2021$url, "/", simplify = TRUE)[,6]
gameID_table_2021 <- tibble(id = gameIDs_2021, season = box_score_urls_2021$season, league = "SHL")


# In-Season Game & Schedule Refresh ---------------------------------------

#re-run functions to get most up-to-date full schedule
#(will have to update to most recent season)
get_2021_urls <- get_schedule("SHL", "2020-21")

get_2021_ids <- str_split(get_2021_urls$url, "/", simplify = TRUE)[,6]

get_2021_gameID_table <- tibble(id = get_2021_ids, 
                                season = get_2021_urls$season, 
                                league = "SHL")

#new URL bundle for current season
get_2021_box_score_urls <- tibble(url = paste("https://stats.swehockey.se/Game/Events/", get_2021_ids[1:length(get_2021_ids)], sep = ''),
                                  league = 'SHL', season = '2020-21')

get_2021_lineup_urls <- tibble(url = paste("https://stats.swehockey.se/Game/LineUps/", get_2021_ids[1:length(get_2021_ids)], sep = ''),
                               league = 'SHL', season = '2020-21')

#refresh the existing URL values with the updated info

box_score_urls_2021 <- rbind(box_score_urls_2021, get_2021_box_score_urls) %>%
  unique()

lineup_urls_2021 <- rbind(lineup_urls_2021, get_2021_lineup_urls) %>%
  unique()

gameIDs_2021 <- get_2021_ids

#table is now fully rebuilt
gameID_table_2021 <- tibble(id = gameIDs_2021, 
                            season = box_score_urls_2021$season, 
                            league = "SHL")


# Lineup Data -------------------------------------------------------------

#refresh lineup data for most recent batch of games
lineup_data_2021_update <- c()
for(id in gameID_table_2021$id[50:56]) {
  
  temp <- get_lineup(id)
  
  lineup_data_2021_update <- rbind(lineup_data_2021_update, temp)
  
  print(id)
  rm(temp)
  
}

#manual edits will be needed since the get_lineup function is unreliable, by design
write_excel_csv(lineup_data_2021_update, "lineup_data_2021_update.csv")

#re-upload the manual edits
lineup_data_2021_update <- read.csv("lineup_data_2021_update.csv",  stringsAsFactors = FALSE)

#join updated lineup data with existing base
#this will break the first time it's run
lineup_data_2021 <- rbind(lineup_data_2021, lineup_data_2021_update)

# Swehockey Functions -----------------------------------------------------------

#function is from Evan Oppenheimer - pulls down a league schedule from an entire season from swehockey
get_schedule <- function(league, season, ..., progress = TRUE) {
  
  mydata <- tidyr::crossing(league, season)
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_schedule() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    
    pb$tick(0)
    
  }
  
  .get_schedule <- function(league, season, ...) {
    
    if (league == "SHL") {
      
      if (season == "2020-21") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/11450"}
      
      else if (season == "2019-20") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/10371"}
      else if (season == "2018-19") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9171"}
      else if (season == "2017-18") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/8121"}
      else if (season == "2016-17") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/7132"}
      else if (season == "2015-16") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/6052"}
      else if (season == "2014-15") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/5056"}
      else if (season == "2013-14") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/3905"}
      else if (season == "2012-13") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/2892"}
      else if (season == "2011-12") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/965"}
      else if (season == "2010-11") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/258"}
      else if (season == "2009-10") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9024"}
      else if (season == "2008-09") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9052"}
      else if (season == "2007-08") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9077"}
      else if (season == "2006-07") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9143"}
      else if (season == "2005-06") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9146"}
      else if (season == "2004-05") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9149"}
      else if (season == "2003-04") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9152"}
      
      else {stop("Season not available. Sorry!")}
      
    }
    
    else if (league == "Allsvenskan") {
      
      if (season == "2020-21") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/11449"}
      
      else if (season == "2019-20") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/10333"}
      else if (season == "2018-19") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9168"}
      else if (season == "2017-18") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/8122"}
      else if (season == "2016-17") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/7157"}
      else if (season == "2015-16") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/6053"}
      else if (season == "2014-15") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/5057"}
      else if (season == "2013-14") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/3906"}
      else if (season == "2012-13") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/3005"}
      else if (season == "2011-12") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/966"}
      else if (season == "2010-11") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/259"}
      else if (season == "2009-10") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9301"}
      else if (season == "2008-09") {url <- "https://stats.swehockey.se/ScheduleAndResults/Schedule/9305"}
      
      else {stop("Season not available. Sorry!")}
      
    }  
    
    else if (league == "SuperElit") {
      
      if (season == "2018-19") {
        
        url <- c(
          url_norra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/9169",
          url_sodra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/9170")
        
      }
      
      else if (season == "2017-18") {
        
        url <- c(
          url_forts = "https://stats.swehockey.se/ScheduleAndResults/Schedule/8702",
          url_top_10 = "https://stats.swehockey.se/ScheduleAndResults/Schedule/8701",
          url_norra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/8124",
          url_sodra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/8125")
        
      }
      
      else if (season == "2016-17") {
        
        url <- c(
          url_forts = "https://stats.swehockey.se/ScheduleAndResults/Schedule/7155",
          url_top_10 = "https://stats.swehockey.se/ScheduleAndResults/Schedule/7154",
          url_norra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/7152",
          url_sodra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/7153")
        
      }
      
      else if (season == "2015-16") {
        
        url <- c(
          url_forts = "https://stats.swehockey.se/ScheduleAndResults/Schedule/6695",
          url_top_10 = "https://stats.swehockey.se/ScheduleAndResults/Schedule/6694",
          url_norra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/6054",
          url_sodra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/6055")
        
      }
      
      else if (season == "2014-15") {
        
        url <- c(
          url_forts = "https://stats.swehockey.se/ScheduleAndResults/Schedule/5784",
          url_top_10 = "https://stats.swehockey.se/ScheduleAndResults/Schedule/5783",
          url_norra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/5024",
          url_sodra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/5025")
        
      }
      
      else if (season == "2013-14") {
        
        url <- c(
          url_forts = "https://stats.swehockey.se/ScheduleAndResults/Schedule/4656",
          url_top_10 = "https://stats.swehockey.se/ScheduleAndResults/Schedule/4655",
          url_norra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/3940",
          url_sodra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/3941")
        
      }
      
      else if (season == "2012-13") {
        
        url <- c(
          url_forts = "https://stats.swehockey.se/ScheduleAndResults/Schedule/3630",
          url_top_10 = "https://stats.swehockey.se/ScheduleAndResults/Schedule/3629",
          url_norra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/3002",
          url_sodra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/3003")
        
      }
      
      else if (season == "2011-12") {
        
        url <- c(
          url_forts = "https://stats.swehockey.se/ScheduleAndResults/Schedule/1651",
          url_top_10 = "https://stats.swehockey.se/ScheduleAndResults/Schedule/1650",
          url_norra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/960",
          url_sodra = "https://stats.swehockey.se/ScheduleAndResults/Schedule/961")
        
      }
      
      else {stop("Season not available. Sorry!")}
      
    }
    
    else {stop("League not available. Sorry!")}
    
    schedule_clean <- function(url, ...) {
      
      clean_schedule <- url %>%
        read_html() %>%
        html_nodes("#groupStandingResultContent a") %>%
        html_attr("href") %>%
        str_replace_all("[\r\n]", "") %>%
        str_squish() %>%
        str_extract("/Game/Events/[[:digit:]]{2,}") %>%
        str_c("https://stats.swehockey.se", .) %>%
        tibble::enframe(name = NULL) %>%
        set_names("url") %>%
        mutate(league = league) %>%
        mutate(season = season) %>%
        drop_na()
      
      return(clean_schedule)
      
    }
    
    schedule_data <- map_dfr(url, schedule_clean)
    
    if (progress) {pb$tick()}
    
    return(schedule_data)
    
  }
  
  all_schedule_data <- map2_dfr(mydata[["league"]], mydata[["season"]], .get_schedule)
  
  return(all_schedule_data)
  
}

#lineup
get_lineup <- function(id) {
  
  url <- read_html(paste('https://stats.swehockey.se/Game/LineUps/', id, sep=''))
  #url <- read_html(GET(paste('https://stats.swehockey.se/Game/LineUps/', id, sep='')))
  
  #extract the teams playing
  teams <- url %>%
    html_nodes(xpath='//h2') %>%
    html_text() %>%
    tibble::enframe(name = NULL) %>%
    .[1,]
  
  teams <- teams %>%
    str_split(., "\\-", simplify = TRUE)

  home_team <- str_squish(teams[,1])
  away_team <- str_squish(teams[,2])

  #extract the date
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

  raw_lineup <- raw_lineup[,c(5,13,14)]
  
  raw_lineup <- raw_lineup %>%
    dplyr::rename(jersey = number2, player = player7)

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

  lineup_final <- merge(lineup_final, master_team_schedule_2021_v2[,c("swehockey_team_date", "game_number")],
                        by.x = "team_date", by.y = "swehockey_team_date")

  lineup_final <- lineup_final[,c(2:6,1,7,8)]

  return(lineup_final)
  #return(raw_lineup)
}

cbind.fill<-function(...,fill=NULL)
{
  inputs<-list(...)
  inputs<-lapply(inputs,vert)
  maxlength<-max(unlist(lapply(inputs,len)))
  bufferedInputs<-lapply(inputs,buffer,length.out=maxlength,fill,preserveClass=FALSE)
  return(Reduce(cbind.data.frame,bufferedInputs))
}

vert<-function(object)
{
  #result<-as.data.frame(cbind(as.matrix(object)))
  if(is.list(object))
    object<-cbind(object)
  object<-data.frame(object)
  
  return(object)
}

len <- function(data)
{
  result<-ifelse(is.null(nrow(data)),length(data),nrow(data))
  return(result)
}

buffer<-function(x,length.out=len(x),fill=NULL,preserveClass=TRUE)
{
  xclass<-class(x)
  input<-lapply(vert(x),unlist)
  results<-as.data.frame(lapply(input,rep,length.out=length.out))
  if(length.out>len(x) && !is.null(fill))
  {
    results<-t(results)
    results[(length(unlist(x))+1):length(unlist(results))]<-fill
    results<-t(results)
  }
  if(preserveClass)
    results<-as2(results,xclass)
  return(results)   
}

#box score
get_box_score <- function(id) {
  
  url <- read_html(paste('https://stats.swehockey.se/Game/Events/', id, sep=''))
  
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
    dplyr::select(-scorers, -points)


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
      dplyr::rename(GF1 = V1, GF2 = V2, GF3 = V3, GF4 = V4, GF5 = V5, GF6 = V6)
  }

  if(ncol(gf) == 5) {
    gf <- gf %>%
      dplyr::rename(GF1 = V1, GF2 = V2, GF3 = V3, GF4 = V4, GF5 = V5)
  }

  if(ncol(gf) == 4) {
    gf <- gf %>%
      dplyr::rename(GF1 = V1, GF2 = V2, GF3 = V3, GF4 = V4)
  }

  if(ncol(gf) == 1) {
    gf <- gf %>%
      dplyr::rename(GF1 = V1)
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
      dplyr::rename(GA1 = V1, GA2 = V2, GA3 = V3, GA4 = V4, GA5 = V5, GA6 = V6)
  }

  if(ncol(ga) == 5) {
    ga <- ga %>%
      dplyr::rename(GA1 = V1, GA2 = V2, GA3 = V3, GA4 = V4, GA5 = V5)
  }

  if(ncol(ga) == 4) {
    ga <- ga %>%
      dplyr::rename(GA1 = V1, GA2 = V2, GA3 = V3, GA4 = V4)
  }

  if(ncol(ga) == 1) {
    ga <- ga %>%
      dplyr::rename(GA1 = V1)
  }

  box_score_data <- bind_cols(box_score_data, gf, ga) %>%
    dplyr::select(-players)

  box_score_data %>%
    mutate(home_team = home_team, away_team = away_team) %>%
    left_join(swehockey_teams, by = c("team" = "abbreviation")) %>%
    dplyr::rename(gf_team = full_name) %>%
    mutate(ga_team = ifelse(gf_team == home_team, away_team, home_team)) %>%
    mutate(date = date) %>%
    mutate(gameid = id) %>%
    mutate(gf_team_date = paste(gf_team, date, sep = '')) %>%
    mutate(ga_team_date = paste(ga_team, date, sep = '')) -> box_score_data

  box_score_data <- merge(box_score_data, master_team_schedule_2021_v2[,c("swehockey_team_date", "game_number")],
                          by.x = "gf_team_date", by.y = "swehockey_team_date") %>%

    dplyr::rename(gf_team_game_number = game_number)

  box_score_data <- merge(box_score_data, master_team_schedule_2021_v2[,c("swehockey_team_date", "game_number")],
                          by.x = "ga_team_date", by.y = "swehockey_team_date") %>%

    dplyr::rename(ga_team_game_number = game_number)


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


# SHL.se Functions --------------------------------------------------------

#team stats
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
    mutate(date1 = date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove space in "Lag" column
    #mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_2021, by = "team_date") %>%
    
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
}

team_goals <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/goal?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,]))
  
  table <- table[-1, ]
  
  colnames(table) <- c("Rank", "Lag", "GP", "GF", "GA", "GF_PP", "GA_PP", "GF_SH", "GA_SH", "ENG", "ENGA", "1st G")
  
  table <- table %>%
    #add game date
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'GF', 'GA', 'GF_PP', 'GA_PP', 'GF_SH', 'GA_SH', 'ENG', 'ENGA', '1st G'), as.numeric) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_2021, by = "team_date") %>%
    
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

team_pp <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/powerplay?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,]))
  
  table <- table[-1, ]
  
  table <- table %>%
    #add game date
    mutate(date1 = date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'G', 'GA', 'Opp', 'SOG'), as.numeric) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_2021, by = "team_date") %>%
    
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #separate TOI columns
    separate(TPP, sep=':', into=c('Minutes', 'Seconds')) %>%
    
    #convert minutes and seconds into numeric
    mutate_at(c('Minutes', 'Seconds'), as.numeric) %>%
    
    #create TOI metric that's usable
    mutate(TOI_PP = round((Minutes * 60 + Seconds)/60,1))
  
  return(table)
  
}

team_pk <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/penaltykilling?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,]))
  
  table <- table[-1, ]
  
  table <- table %>%
    #add game date
    mutate(date1 = date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'G', 'GA', 'Opp', 'SOGA'), as.numeric) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_2021, by = "team_date") %>%
    
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #separate TOI columns
    separate(TSH, sep=':', into=c('Minutes', 'Seconds')) %>%
    
    #convert minutes and seconds into numeric
    mutate_at(c('Minutes', 'Seconds'), as.numeric) %>%
    
    #create TOI metric that's usable
    mutate(TOI_SH = round((Minutes * 60 + Seconds)/60,1))
  
}

team_shotsfor <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/shotsforward?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,]))
  
  table <- table[-1, ]
  
  colnames(table) <- c("Rank", "Lag", "GP", "G", "SOG", "S%", "G_PP", "SOG_PP", "S%_PP", "MS", "SW", "SIB", "SF", "SS")
  
  table <- table %>%
    #add game date
    mutate(date1 = date1) %>%
    
    #make columns numeric
    mutate_at(c("GP", "G", "SOG", "G_PP", "SOG_PP", "MS", "SW", "SIB", "SF", "SS"), as.numeric) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_2021, by = "team_date") %>%
    
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

team_shotsagainst <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/lag/shotsagainst?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  #make first row the column headers
  colnames(table) <- as.character(unlist(table[1,]))
  
  table <- table[-1, ]
  
  colnames(table) <- c("Rank", "Lag", "GP", "GA", "SOGA", "SVS", "SVS%", "MSA", "SWA", "BKS", "SFA", "SSA", "HITS", "SOGA_PK")
  
  table <- table %>%
    #add game date
    mutate(date1 = date1) %>%
    
    #make columns numeric
    mutate_at(c("GP", "GA", "SOGA", "SVS", "MSA", "SWA", "BKS", "SFA", "SSA", "HITS"), as.numeric) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_2021, by = "team_date") %>%
    
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

#player stats
#old
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
    mutate(date1 = date1) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date1, sep = "")) %>%
    
    #concatenate team and date
    mutate(team_date = paste(Lag, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_2021, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
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

player_toi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/timeonice?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    mutate(
      date1 = date1,
      placeholder_name = gsub("\\s+\\S+$", "", Spelare),
      name = gsub('.{1}$', '', placeholder_name))
  
  # table <- table %>%
  #   mutate(name2 = ifelse(
  #     name == 'Linus Nässén' & Pos == 'D', 'Linus Nässén (D)', name
  #   ))
  # 
  # table <- table %>%
  #   mutate(name2 = ifelse(
  #     name == 'Fredrik Forsberg' & Lag == 'HV71HV71', 'Fredrik Forsberg (RH)', name
  #   ))
  
  table <- table %>%
    mutate(name2 = case_when(
      name == 'Linus Nässén' & Pos == 'D' ~ 'Linus Nässén (D)',
      name == 'Fredrik Forsberg' & Lag == 'HV71HV71' ~ 'Fredrik Forsberg (RH)',
      TRUE ~ name
    ))
  
  table <- table %>%
    dplyr::select(-name)
  
  table <- table %>%
    dplyr::rename(name = name2)
  
  table <- table %>%
    mutate(
      name_date = paste(name, date1, sep = ""),
      team_date = paste(Lag, date1, sep = ""),
      season = '20/21'
    ) %>%
    
    left_join(master_team_schedule_2021_v2, by = "team_date") %>%
    
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
                     home_team_name_double, away_team_name_double, combined_team_names_double, date, team, swehockey_team_name)) %>%

    left_join(shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) %>%
    
    left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
    
    dplyr::rename(swehockey_name = swehockey) %>%
    
    mutate(player_team = paste(swehockey_name, swehockey_team_name, sep = '$')) %>%

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
    mutate(SHTOI = round((SHMinutes * 60 + SHSeconds)/60,1)) %>%

    dplyr::select("Rank", "Spelare", "Nr", "Lag", "Pos", "GP", "Minutes", "Seconds", "PPMinutes", "PPSeconds",
                  "ESMinutes", "ESSeconds", "SHMinutes", "SHSeconds", "PPTOI.", "SHTOI.", "date1", "placeholder_name",
                  "name", "name_date", "team_date", "game_number", "TOI", "PPTOI", "ESTOI", "SHTOI", "shlse_team_name",
                  "swehockey_team_name", "shl_team_abbreviation", "shlse_team_id", "swehockey_name", "season", "player_team")

    #dplyr::rename(swehockey_team_name = swehockey_team_name.x)
    
    
  
  return(table)
    
}

#old
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
    mutate(date1 = date1) %>%
    
    #dplyr::rename column headers
    dplyr::rename(G_PP = G.1, A_PP = A.1, TP_PP = TP.1,
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
    left_join(master_team_schedule_2021, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

player_points <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/point?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
    
  colnames(table) <- as.character(unlist(table[1,])) %>% 
    make.unique()
  
  
  table <- table[-1, ] %>%
    mutate(
      date1 = date1,
      placeholder_name = gsub("\\s+\\S+$", "", Spelare),
      name = gsub('.{1}$', '', placeholder_name))
  
  # table <- table %>%
  #   mutate(name2 = ifelse(
  #     name == 'Linus Nässén' & Pos == 'D', 'Linus Nässén (D)', name
  #   ))
  # 
  # table <- table %>%
  #   mutate(name2 = ifelse(
  #     name == 'Fredrik Forsberg' & Lag == 'HV71HV71', 'Fredrik Forsberg (RH)', name
  #   ))
  
  table <- table %>%
    mutate(name2 = case_when(
      name == 'Linus Nässén' & Pos == 'D' ~ 'Linus Nässén (D)',
      name == 'Fredrik Forsberg' & Lag == 'HV71HV71' ~ 'Fredrik Forsberg (RH)',
      TRUE ~ name
    ))
  
  table <- table %>%
    dplyr::select(-name)
  
  table <- table %>%
    dplyr::rename(name = name2)
  
  table <- table %>%
    mutate(
      name_date = paste(name, date1, sep = ""),
      team_date = paste(Lag, date1, sep = ""),
      season = '20/21'
    ) %>%
    
    left_join(master_team_schedule_2021_v2, by = "team_date") %>%
    
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
                     home_team_name_double, away_team_name_double, combined_team_names_double, date, team, swehockey_team_name)) %>%
    
    left_join(shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) %>%
    
    left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
    
    dplyr::rename(swehockey_name = swehockey) %>%
    
    mutate(player_team = paste(swehockey_name, swehockey_team_name, sep = '$')) %>%
  
    dplyr::rename(G_PP = G.1, A_PP = A.1, TP_PP = TP.1,
                G_EV = G.2, A_EV = A.2, TP_EV = TP.2,
                G_SH = G.3, A_SH = A.3, TP_SH = TP.3) %>%
    
    #make columns numeric
    mutate_at(c('Nr', 'GP', 'G', 'A', 'A1', 'A2', 'TP',
                'G_EV', 'A_EV', 'TP_EV',
                'G_PP', 'A_PP', 'TP_PP',
                'G_SH', 'A_SH', 'TP_SH'), as.numeric) %>%
    
    dplyr::select("Rank", "Spelare", "Nr", "Lag", "Pos", "GP", "G", "A", "A1", "A2", "TP", "G_PP",
                  "A_PP", "TP_PP", "G_EV", "A_EV", "TP_EV", "G_SH", "A_SH", "TP_SH", "TOI/GP", "date1",
                  "placeholder_name", "name", "name_date", "team_date", "game_number", "shlse_team_name",
                  "swehockey_team_name", "shl_team_abbreviation", "shlse_team_id", "swehockey_name", "season")
  
  
  return(table)
  
}

#old
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
    mutate(date1 = date1) %>%
    
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
    left_join(master_team_schedule_2021, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team,
              `CF%`, `CCF%`, `FF%`, `CFF%`, `OIG%`, `OISVS%`, PDO, `TOI/GP`)) %>%
    
    #join in team corsi data
    #need this to get relative points later
    left_join(team_corsi_data_2021, by = "team_date") %>%
    
    #remove duplicated columns from join
    dplyr::select(-c(Rank.y, Lag.y, GP.y, `CF%`, `CCF%`, `FF%`, `CFF%`,
              `OIG%`, `OISVS%`, PDO, date1.y, Lag2, game_number.y)) %>%
    
    #remove columns that'll break the update process
    #dplyr::select(-c(shlse_team_name, swehockey_team_name, shl_team_abbreviation, shlse_team_id)) %>%
    
    #dplyr::rename columns
    dplyr::rename(Lag = Lag.x, CF = CF.x, CA = CA.x, FF = FF.x, FA = FA.x,
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

player_corsi <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/extendedmetrics?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  colnames(table) <- as.character(unlist(table[1,])) 
  
  table <- table[-1, ] %>%
    mutate(
      date1 = date1,
      placeholder_name = gsub("\\s+\\S+$", "", Spelare),
      name = gsub('.{1}$', '', placeholder_name))
    
    # table <- table %>%
    # mutate(name2 = ifelse(
    #   name == 'Linus Nässén' & Pos == 'D', 'Linus Nässén (D)', name
    # ))
    # 
    # table <- table %>%
    #   mutate(name2 = ifelse(
    #     name == 'Fredrik Forsberg' & Lag == 'HV71HV71', 'Fredrik Forsberg (RH)', name
    #   ))
    
    table <- table %>%
      mutate(name2 = case_when(
        name == 'Linus Nässén' & Pos == 'D' ~ 'Linus Nässén (D)',
        name == 'Fredrik Forsberg' & Lag == 'HV71HV71' ~ 'Fredrik Forsberg (RH)',
        TRUE ~ name
      )) 
  
  table <- table %>%
    dplyr::select(-name)
  
  table <- table %>%
    dplyr::rename(name = name2)
  
  table <- table %>%
    mutate(
      name_date = paste(name, date1, sep = ""),
      team_date = paste(Lag, date1, sep = ""),
      season = '20/21') %>%
    
    left_join(master_team_schedule_2021_v2, by = "team_date") %>%
    
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
                     home_team_name_double, away_team_name_double, combined_team_names_double, date, team, swehockey_team_name)) %>%
    
    left_join(shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) %>%
    
    left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
    
    dplyr::rename(swehockey_name = swehockey) %>%
    
    mutate(player_team = paste(swehockey_name, swehockey_team_name, sep = '$')) %>%
  
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    left_join(team_corsi_data_2021, by = "team_date") %>%
    
    dplyr::select(-c(Rank.y, Lag.y, GP.y, `CF%.y`, `CCF%.y`, `FF%.y`, `CFF%.y`,
                     `OIG%.y`, `OISVS%.y`, PDO.y, date1.y, Lag2, game_number.y, season.y)) %>%


    dplyr::rename(Lag = Lag.x, CF = CF.x, CA = CA.x, FF = FF.x, FA = FA.x,
                  date2 = date1.x, tmCF = CF.y, tmCA = CA.y, tmFF = FF.y,
                  tmFA = FA.y) %>%

    #add in % columns
    mutate(`CF%` = round(CF/(CF + CA),3), `FF%` = round(FF/(FF + FA),3),
           CF_off = tmCF - CF, CA_off = tmCA - CA, FF_off = tmFF - FF,
           FA_off = tmFA - FA) %>%

    #add in team CF% and FF%
    mutate(`CF%_off` = round(CF_off/(CF_off + CA_off),3),
           `FF%_off` = round(FF_off/(FF_off + FA_off),3)) %>%

    dplyr::select("Rank.x", "Spelare", "Nr", "Lag", "Pos", "GP.x", "CF", "CA", "FF", "FA", "date2", "placeholder_name", "name", "name_date",
                  "team_date", "game_number.x", "tmCF", "tmCA", "tmFF", "tmFA", "CF%", "FF%", "CF_off", "CA_off", "FF_off", "FA_off", "CF%_off",
                  "FF%_off", "shlse_team_name.x", "swehockey_team_name.x", "shl_team_abbreviation", "shlse_team_id", "swehockey_name", "season.x", "player_team") %>%

    dplyr::rename(CF. = `CF%`, FF. = `FF%`, CF._off = `CF%_off`, FF._off = `FF%_off`,
                  shlse_team_name = shlse_team_name.x, swehockey_team_name = swehockey_team_name.x, season = season.x)
    
    return(table)
  
}

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
    mutate(date1 = date1) %>%
    
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
    dplyr::select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team)) %>%
    
    #dplyr::rename columns
    dplyr::rename(GF = X., GA = X..1, PlusMinus = X...) %>%
    
    mutate_at(c('GF', 'GA','PlusMinus'), as.numeric)
  
  return(table)
  
}

player_shots <- function(teamID) {
  
  url <- read_html(paste('https://www.shl.se/statistik/spelare/shot?season=from', date1,
                         'to', date1 + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE)
  
  colnames(table) <- as.character(unlist(table[1,])) %>% 
    make.unique()
  
  table <- table[-1, ] %>%
    mutate_at(c("SOG", "SOG.1", "SOG.2", "MS"), as.numeric) %>%
    
    mutate(date1 = date1,
           SOG_EV = SOG - `SOG.1` - `SOG.2`,
           placeholder_name = gsub("\\s+\\S+$", "", Spelare),
           name = gsub('.{1}$', '', placeholder_name))
  
  # table <- table %>%
  #   mutate(name2 = case_when(
  #     name2 == 'Linus Nässén' & Pos == 'D' ~ 'Linus Nässén (D)'
  #   ))
  
  table <- table %>%
    mutate(name2 = ifelse(
      name == 'Linus Nässén' & Pos == 'D', 'Linus Nässén (D)', name
    ))
  
  table <- table %>%
    mutate(name2 = ifelse(
      name == 'Fredrik Forsberg' & Lag == 'HV71HV71', 'Fredrik Forsberg (RH)', name
    ))
  
  table <- table %>%
    dplyr::select(-name)
  
  table <- table %>%
    dplyr::rename(name = name2)
  
  table <- table %>%
    mutate(
           name_date = paste(name, date1, sep = ""),
           team_date = paste(Lag, date1, sep = ""),
           season = '20/21'
           ) %>%

    dplyr::rename(SOG_PP = SOG.1, SOG_PK = SOG.2) %>%

    left_join(master_team_schedule_2021_v2, by = "team_date") %>%

    left_join(player_lookup_2021, by = c("name" = "shlse")) %>%

    dplyr::rename(swehockey_name = swehockey) %>%

    mutate(player_team = paste(swehockey_name, swehockey_team_name, sep = '$')) %>%

    dplyr::select("Spelare", "Lag", "Pos", "SOG", "SOG_EV", "SOG_PP", "SOG_PK", "MS",
                  "date1", "name", "name_date", "swehockey_name", "season",
                  "game_number", "swehockey_team_date", "player_team")
  
  
  return(table)
  
}

# SHL.se Stats Update - Teams -----------------------------------------------------

#teams

#get rid of the joined in additional columns, so that the bind rows continues to work on the update
#(might need to add more tables here)
team_corsi_data_2021 <- team_corsi_data_2021[,c(1:18)]
#
#
#
#

#TEAM CORSI
team_corsi_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(1:14)]) {
  
  for (date1 in as.list(shl_dates_2021[12])) {
    
    temp <- team_corsi(teamID)
    
    team_corsi_data_2021_update <- rbind(team_corsi_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

#join in friendlier team names
left_join(team_corsi_data_2021_update, shl_team_dictionary_2021, by = c("Lag2" = "shlse_team_name_double")) -> team_corsi_data_2021_update

team_corsi_data_2021_update <- team_corsi_data_2021_update %>%
  mutate(season = "20/21")

team_corsi_data_2021_update <- team_corsi_data_2021_update %>%
  dplyr::select("Lag2", "team_date", "Rank", "Lag", "GP", "CF", "CA", "CF%", "CCF%", "FF", "FA", "FF%", "CFF%", "OIG%", "OISVS%", "PDO",
         "date1", "season", "game_number", "shlse_team_name", "swehockey_team_name")


#append updated rows to updated season table
team_corsi_data_2021 <- rbind(team_corsi_data_2021, team_corsi_data_2021_update)

#get rid of the first column in the master data frame (remains to be seen if this is constant)
#team_corsi_data_master_team <- team_corsi_data_master_team[,c(2:22)]

#remove the 20/21 rows first - fix this later
# team_corsi_data_master_team <- team_corsi_data_master_team %>%
#   filter(season != '20/21')

team_corsi_data_master_team <- rbind(team_corsi_data_master_team, team_corsi_data_2021_update)

#TEAM GOALS - FORGET THIS
team_goals_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(6,9)]) {
  
  for (date1 in as.list(shl_dates_2021[2])) {
    
    temp <- team_goals(teamID)
    
    team_goals_data_2021_update <- rbind(team_goals_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(team_goals_data_2021_update, shl_team_dictionary_2021, by = c("Lag2" = "shlse_team_name_double")) -> team_goals_data_2021_update

team_goals_data_2021_update <- team_goals_data_2021_update %>%
  mutate(season = "20/21")

team_goals_data_2021_update <- team_goals_data_2021_update %>%
  dplyr::select("Lag2", "team_date", "Rank", "Lag", "GP", "GF", "GA", "GF_PP", "GA_PP", "GF_SH", "GA_SH", "ENG", "ENGA", "1st G",
                "date1", "game_number", "shlse_team_name")

team_goals_data_2021_update <- team_goals_data_2021_update %>%
  dplyr::rename(X1st.G = `1st G`)

team_goals_data_2021 <- rbind(team_goals_data_2021, team_goals_data_2021_update)

#team_goals_data_master <- team_goals_data_master[,c(2:18)]
team_goals_data_master <- rbind(team_goals_data_master, team_goals_data_2021)

#TEAM PP
team_pp_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(1:14)]) {
  
  for (date1 in as.list(shl_dates_2021[12])) {
    
    temp <- team_pp(teamID)
    
    team_pp_data_2021_update <- rbind(team_pp_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(team_pp_data_2021_update, shl_team_dictionary_2021, by = c("Lag2" = "shlse_team_name_double")) -> team_pp_data_2021_update

team_pp_data_2021_update <- team_pp_data_2021_update %>%
  mutate(season = "20/21")

team_pp_data_2021_update <- team_pp_data_2021_update %>%
  dplyr::select("Lag2", "team_date", "Rank", "Lag", "GP", "G", "GA", "Opp", "PP%", "SOG", "Minutes", "Seconds", "ST%",
                "date1", "TOI_PP", "season", "game_number", "shlse_team_name", "swehockey_team_name")


team_pp_data_2021 <- rbind(team_pp_data_2021, team_pp_data_2021_update)


team_pp_data_master <- rbind(team_pp_data_master, team_pp_data_2021_update)



#TEAM PK
team_pk_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(1:14)]) {
  
  for (date1 in as.list(shl_dates_2021[12])) {
    
    temp <- team_pk(teamID)
    
    team_pk_data_2021_update <- rbind(team_pk_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(team_pk_data_2021_update, shl_team_dictionary_2021, by = c("Lag2" = "shlse_team_name_double")) -> team_pk_data_2021_update

team_pk_data_2021_update <- team_pk_data_2021_update %>%
  mutate(season = "20/21")

team_pk_data_2021_update <- team_pk_data_2021_update %>%
  dplyr::select("Lag2", "team_date", "Rank", "Lag", "GP", "G", "GA", "Opp", "PK%", "SOGA", "Minutes", "Seconds", "ST%",
                "date1", "TOI_SH", "season", "game_number", "shlse_team_name", "swehockey_team_name")

#team_pk_data_master <- team_pk_data_master[,c(2:20)]
team_pk_data_master <- rbind(team_pk_data_master, team_pk_data_2021_update)

team_pk_data_2021 <- rbind(team_pk_data_2021, team_pk_data_2021_update)

#TEAM SHOTSFOR
team_shotsfor_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(1:14)]) {
  
  for (date1 in as.list(shl_dates_2021[12])) {
    
    temp <- team_shotsfor(teamID)
    
    team_shotsfor_data_2021_update <- rbind(team_shotsfor_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(team_shotsfor_data_2021_update, shl_team_dictionary_2021, by = c("Lag2" = "shlse_team_name_double")) -> team_shotsfor_data_2021_update

team_shotsfor_data_2021_update <- team_shotsfor_data_2021_update %>%
  mutate(season = "20/21")

team_shotsfor_data_2021_update <- team_shotsfor_data_2021_update %>%
  dplyr::select("Lag2", "team_date", "Rank", "Lag", "GP", "G", "SOG", "S%", "G_PP", "SOG_PP", "S%_PP", "MS", "SW",
                "SIB", "SF", "SS", "date1", "season", "game_number", "shlse_team_name", "swehockey_team_name")


#team_shotsfor_data_master <- team_shotsfor_data_master[,c(2:22)]
team_shotsfor_data_master <- rbind(team_shotsfor_data_master, team_shotsfor_data_2021_update)

team_shotsfor_data_2021 <- rbind(team_shotsfor_data_2021, team_shotsfor_data_2021_update)

#TEAM SHOTSAGAINST
team_shotsagainst_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(1:14)]) {
  
  for (date1 in as.list(shl_dates_2021[12])) {
    
    temp <- team_shotsagainst(teamID)
    
    team_shotsagainst_data_2021_update <- rbind(team_shotsagainst_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(team_shotsagainst_data_2021_update, shl_team_dictionary_2021, by = c("Lag2" = "shlse_team_name_double")) -> team_shotsagainst_data_2021_update

team_shotsagainst_data_2021_update <- team_shotsagainst_data_2021_update %>%
  mutate(season = "20/21")

team_shotsagainst_data_2021_update <- team_shotsagainst_data_2021_update %>%
  dplyr::select("Lag2", "Rank", "Lag", "GP", "GA", "SOGA", "SVS", "SVS%", "MSA", "SWA", "BKS", "SFA", "SSA",
                "HITS", "SOGA_PK", "date1", "team_date", "game_number", "season", "shlse_team_name", "swehockey_team_name")



#team_shotsagainst_data_master <- team_shotsagainst_data_master[,c(2:22)]
team_shotsagainst_data_master <- rbind(team_shotsagainst_data_master, team_shotsagainst_data_2021_update)

team_shotsagainst_data_2021 <- rbind(team_shotsagainst_data_2021, team_shotsagainst_data_2021_update)

# team_shotsagainst_data_master <- team_shotsagainst_data_master %>%
#   mutate_at('SOGA_PK', as.numeric)


# #join in friendlier team names
# team_corsi_data_2021 <- left_join(team_corsi_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))
# team_goals_data_2021 <- left_join(team_goals_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))
# team_pp_data_2021 <- left_join(team_pp_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))
# team_pk_data_2021 <- left_join(team_pk_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))
# team_shotsfor_data_2021 <- left_join(team_shotsfor_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))


# SHL.se Stats Update - Players -------------------------------------------


#get rid of the joined in additional columns, so that the bind rows continues to work on the update
#(might need to add more tables here)
player_toi_data_2021 <- player_toi_data_2021[,c(1:30)]
player_points_data_2021 <- player_points_data_2021[,c(1:31)]
player_corsi_data_2021 <- player_corsi_data_2021[,c(1:32)]
player_ev_data_2021 <- player_ev_data_2021[,c(1:26)]

#POINTS
player_points_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(1:14)]) {
  
  for (date1 in as.list(shl_dates_2021[11])) {
    
    temp <- player_points(teamID)
    
    player_points_data_2021_update <- rbind(player_points_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

# left_join(player_points_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> player_points_data_2021_update
# 
# player_points_data_2021_update <- player_points_data_2021_update %>%
#   mutate(season = "20/21")
# 
# player_points_data_2021_update <- player_points_data_2021_update %>%
#   left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
#   dplyr::rename(swehockey_name = swehockey)
# 
# player_points_data_2021_update <- player_points_data_2021_update %>%
#   dplyr::select("Rank", "Spelare", "Nr", "Lag", "Pos", "GP", "G", "A", "A1", "A2", "TP", "G_PP", "A_PP", "TP_PP", "G_EV",
#                 "A_EV", "TP_EV", "G_SH", "A_SH", "TP_SH", "TOI/GP", "date1", "placeholder_name", "name", "name_date", "team_date",
#                 "game_number", "shlse_team_name", "swehockey_team_name", "shl_team_abbreviation", "shlse_team_id", "swehockey_name", "season")

player_points_data_2021 <- rbind(player_points_data_2021, player_points_data_2021_update)

player_points_data_master <- rbind(player_points_data_master, player_points_data_2021_update)

#CORSI
player_corsi_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(1:14)]) {
  
  for (date1 in as.list(shl_dates_2021[12])) {
    
    temp <- player_corsi(teamID)
    
    player_corsi_data_2021_update <- rbind(player_corsi_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

# left_join(player_corsi_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> player_corsi_data_2021_update
# 
# 
# player_corsi_data_2021_update <- player_corsi_data_2021_update %>%
#   mutate(season = "20/21")
# 
# player_corsi_data_2021_update <- player_corsi_data_2021_update %>%
#   left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
#   dplyr::rename(swehockey_name = swehockey)
# 
# player_corsi_data_2021_update <- player_corsi_data_2021_update %>%
#   mutate(player_team = paste(swehockey_name, swehockey_team_name.x, sep = "$"))
# 
# player_corsi_data_2021_update <- player_corsi_data_2021_update %>%
#   dplyr::select("Rank.x", "Spelare", "Nr", "Lag", "Pos", "GP.x", "CF", "CA", "FF", "FA", "date2", "placeholder_name", "name",
#                 "name_date", "team_date", "game_number.x", "tmCF", "tmCA", "tmFF", "tmFA", "CF%", "FF%", "CF_off", "CA_off",
#                 "FF_off", "FA_off", "CF%_off", "FF%_off", "shlse_team_name.x", "swehockey_team_name.x", "shl_team_abbreviation",
#                 "shlse_team_id", "swehockey_name", "season", "player_team")
# 
# player_corsi_data_2021_update <- player_corsi_data_2021_update %>%
#   dplyr::rename(`CF.` = `CF%`, `FF.` = `FF%`, `CF._off` = `CF%_off`, `FF._off` = `FF%_off`,
#                 shlse_team_name = shlse_team_name.x, swehockey_team_name = swehockey_team_name.x)

player_corsi_data_2021 <- rbind(player_corsi_data_2021, player_corsi_data_2021_update)

player_corsi_data_master_v3 <- rbind(player_corsi_data_master_v2, player_corsi_data_2021)

# #ev
# player_ev_data_2021_update <- c()
# for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
#   
#   for (date1 in as.list(shl_dates_2021[1:62])) {
#     
#     temp <- player_ev(teamID)
#     
#     player_ev_data_2021_update <- rbind(player_ev_data_2021_update, temp)
#     
#     print(teamID)
#     rm(temp)
#     
#   }
# }
# 
# left_join(player_ev_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> player_ev_data_2021_update
# 
# player_ev_data_2021 <- rbind(player_ev_data_2021, player_ev_data_2021_update)

#TOI
player_toi_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(1:14)]) {
  
  for (date1 in as.list(shl_dates_2021[12])) {
    
    temp <- player_toi(teamID)
    
    player_toi_data_2021_update <- rbind(player_toi_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

# left_join(player_toi_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> player_toi_data_2021_update
# 
# 
# player_toi_data_2021_update <- player_toi_data_2021_update %>%
#   mutate(season = "20/21")
# 
# player_toi_data_2021_update <- player_toi_data_2021_update %>%
#   left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
#   dplyr::rename(swehockey_name = swehockey)
# 
# player_toi_data_2021_update <- player_toi_data_2021_update %>%
#   mutate(player_team = paste(swehockey_name, swehockey_team_name, sep = "$"))
# 
# player_toi_data_2021_update <- player_toi_data_2021_update %>%
#   dplyr::select("Rank", "Spelare", "Nr", "Lag", "Pos", "GP", "Minutes", "Seconds", "PPMinutes", "PPSeconds", "ESMinutes", "ESSeconds",
#                 "SHMinutes", "SHSeconds", "PPTOI.", "SHTOI.", "date1", "placeholder_name", "name", "name_date", "team_date", "game_number",
#                 "TOI", "PPTOI", "ESTOI", "SHTOI", "shlse_team_name", "swehockey_team_name", "shl_team_abbreviation", "shlse_team_id",
#                 "swehockey_name", "season", "player_team")

player_toi_data_2021 <- rbind(player_toi_data_2021, player_toi_data_2021_update)

player_toi_data_master_v3 <- rbind(player_toi_data_master_v2, player_toi_data_2021)

#PLAYER SHOTS
player_shots_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(3,4,5,6,7,8,9,12,13)]) {
  
  for (date1 in as.list(shl_dates_2021[1:2])) {
    
    temp <- player_shots(teamID)
    
    player_shots_data_2021_update <- rbind(player_shots_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

test <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[c(1:14)]) {
  
  for (date1 in as.list(shl_dates_2021[3])) {
    
    temp <- player_shots(teamID)
    
    test <- rbind(test, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

# #join in swehockey names
# player_toi_data_2021 <- player_toi_data_2021 %>%
#   left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
#   rename(swehockey_name = swehockey)
# 
# player_points_data_2021 <- player_points_data_2021 %>%
#   left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
#   rename(swehockey_name = swehockey)
# 
# player_ev_data_2021 <- player_ev_data_2021 %>%
#   left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
#   rename(swehockey_name = swehockey)
# 
# player_corsi_data_2021 <- player_corsi_data_2021 %>%
#   left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
#   rename(swehockey_name = swehockey)
# 
# #join in friendlier team names
# player_toi_data_2021 <- left_join(player_toi_data_2021, shl_team_dictionary_2021, 
#                                   by = c("Lag" = "shlse_team_name_double"))
# 
# player_corsi_data_2021 <- left_join(player_corsi_data_2021, shl_team_dictionary_2021, 
#                                   by = c("Lag" = "shlse_team_name_double"))
# 
# player_ev_data_2021 <- left_join(player_ev_data_2021, shl_team_dictionary_2021, 
#                                   by = c("Lag" = "shlse_team_name_double"))
# 
# player_points_data_2021 <- left_join(player_points_data_2021, shl_team_dictionary_2021, 
#                                   by = c("Lag" = "shlse_team_name_double"))

#player names
player_names_2021 <- unique(player_corsi_data_2021$swehockey_name)

#bind with master datasets

# #team
# 
# team_corsi_data_master <- rbind(team_corsi_data_master, team_corsi_data_2021)
# team_goals_data_master <- rbind(team_goals_data_master, team_goals_data_2021)
# team_pp_data_master <- rbind(team_pp_data_master, team_pp_data_2021)
# team_pk_data_master <- rbind(team_pk_data_master, team_pk_data_2021)
# team_shotsfor_data_master <- rbind(team_shotsfor_data_master, team_shotsfor_data_2021)
# 
# #players
# 
# player_points_data_master <- rbind(player_points_data_master, player_points_data_2021)
# player_toi_data_master <- rbind(player_toi_data_master, player_toi_data_2021)
# player_corsi_data_master <- rbind(player_corsi_data_master, player_corsi_data_2021)
# player_ev_data_master <- rbind(player_ev_data_master, player_ev_data_2021)
# 
# #player names
# #(de-duped???)
# player_names_master <- rbind(player_names_master, player_names_2021)

#export CSVs of master datasets for use in Shiny

#team

#write.csv(team_corsi_data_master, file = "team_corsi_data_master.csv")
write.csv(team_corsi_data_master_team, file = "team_corsi_data_master.csv")
#write.csv(team_goals_data_master, file = "team_goals_data_master.csv")
write.csv(team_pp_data_master, file = "team_pp_data_master.csv")
write.csv(team_pk_data_master, file = "team_pk_data_master.csv")
write.csv(team_shotsfor_data_master, file = "team_shotsfor_data_master.csv")
write.csv(team_shotsagainst_data_master, file = "team_shotsagainst_data_master.csv")

#players
#write.csv(player_corsi_data_master, file = "player_corsi_data_master.csv")
write.csv(player_corsi_data_master_v3, file = "player_corsi_data_master.csv")
#write.csv(player_points_data_master, file = "player_points_data_master.csv")
#write.csv(player_ev_data_master, file = "player_ev_data_master.csv")
#write.csv(player_toi_data_master, file = "player_toi_data_master.csv")
write.csv(player_toi_data_master_v3, file = "player_toi_data_master.csv")

#player names
write.csv(player_names_master, file = "player_names_master.csv")

# Swehockey Boxscore Data -------------------------------------------------

#run box scores with updated batch of gameids for each refresh
box_score_data_2021_raw_update <- c()
for (id in gameID_table_2021$id[50:56]) {
  
  temp <- get_box_score(id)
  
  box_score_data_2021_raw_update <- rbind.fill(box_score_data_2021_raw_update, temp)
  
  print(id)
  rm(temp)
  
}

#this will break the first time, as there is now 2021_raw
#instead just assign raw to the first update refresh
#do this after running Swehockey Boxscore Data - Enriched (fix the order at some point)
box_score_data_2021_raw <- rbind(box_score_data_2021_raw, box_score_data_2021_raw_update)


# Swehockey Boxscore Data - Enriched --------------------------------------


#step 1 - join in names from the uid (team, date, jersey number)
box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GF1_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GF2_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GF3_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GF4_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GF5_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF6_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GF6_name = player)


box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GA1_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GA2_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GA3_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GA4_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GA5_name = player)

box_score_data_2021_raw_update <- merge(box_score_data_2021_raw_update, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA6_uid", by.y = "uid", all.x = TRUE) %>%
                                 dplyr::rename(GA6_name = player)

#step 2 - concatenate all GF and GA names into their own columns
box_score_data_2021_raw_update <- box_score_data_2021_raw_update %>%
  mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name, sep = ''),
         GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name, sep = ''))

#step 3 - count the number of goalies and NAs to assess manpower
box_score_data_2021_raw_update <- box_score_data_2021_raw_update %>%
  mutate(GF_GK = str_count(box_score_data_2021_raw_update$GF_names, "GK"),
         GF_NA = str_count(box_score_data_2021_raw_update$GF_names, "NA")) %>%
  
  mutate(GF_skaters = 6 - GF_GK - GF_NA) %>%
  
  mutate(GA_GK = str_count(box_score_data_2021_raw_update$GA_names, "GK"),
         GA_NA = str_count(box_score_data_2021_raw_update$GA_names, "NA")) %>%
  
  mutate(GA_skaters = 6 - GA_GK - GA_NA)

#step 4 - assign a game state
box_score_data_2021_raw_update <- box_score_data_2021_raw_update %>%
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

box_score_data_2021_raw_update <- box_score_data_2021_raw_update %>%
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


# Swehockey Box Score Data - Condensed ------------------------------------

#filtering down to only necessary columns
box_score_data_2021_condensed <- box_score_data_2021_raw %>%
  
  dplyr::select(gameid, date, home_team, away_team, goal, primary_assist, secondary_assist, game_situation,
         game_situation_general, gf_team, gf_team_game_number, ga_team, ga_team_game_number, GF_names, GA_names,
         GF_skaters, GA_skaters, GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name,
         GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name)

box_score_data_2021_condensed <- box_score_data_2021_condensed %>%
  mutate(season = "20/21")

# box_score_data_master <- rbind(box_score_data_master, box_score_data_2021_condensed)
# 
# write_excel_csv(box_score_data_master, "box_score_data_master.csv")

#adding one additional unique identifier - this is for when I go to create
#player stats for a player unique to one team (not combined & duplicated)
box_score_data_2021_condensed <- box_score_data_2021_condensed %>%
  
  mutate(goal_player_team = paste(goal, gf_team, sep = "$"),
         primary_assist_player_team = paste(primary_assist, gf_team, sep = "$"),
         secondary_assist_player_team = paste(secondary_assist, gf_team, sep = "$"),
         
         GF1_player_team = paste(GF1_name, gf_team, sep = "$"),
         GF2_player_team = paste(GF2_name, gf_team, sep = "$"),
         GF3_player_team = paste(GF3_name, gf_team, sep = "$"),
         GF4_player_team = paste(GF4_name, gf_team, sep = "$"),
         GF5_player_team = paste(GF5_name, gf_team, sep = "$"),
         GF6_player_team = paste(GF6_name, gf_team, sep = "$"),
         
         GA1_player_team = paste(GA1_name, ga_team, sep = "$"),
         GA2_player_team = paste(GA2_name, ga_team, sep = "$"),
         GA3_player_team = paste(GA3_name, ga_team, sep = "$"),
         GA4_player_team = paste(GA4_name, ga_team, sep = "$"),
         GA5_player_team = paste(GA5_name, ga_team, sep = "$"),
         GA6_player_team = paste(GA6_name, ga_team, sep = "$")
  )

#Account for duplicate names - prime example is two Linus Nässéns

#replace scoring
box_score_data_2021_condensed$goal_player_team[box_score_data_2021_condensed$goal_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$primary_assist_player_team[box_score_data_2021_condensed$primary_assist_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$secondary_assist_player_team[box_score_data_2021_condensed$secondary_assist_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$goal[box_score_data_2021_condensed$goal == 'Linus Nässén' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$primary_assist[box_score_data_2021_condensed$primary_assist == 'Linus Nässén' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$secondary_assist[box_score_data_2021_condensed$secondary_assist == 'Linus Nässén' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'

#replace on-ice GF and GA
box_score_data_2021_condensed$GF1_player_team[box_score_data_2021_condensed$GF1_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GF2_player_team[box_score_data_2021_condensed$GF2_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GF3_player_team[box_score_data_2021_condensed$GF3_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GF4_player_team[box_score_data_2021_condensed$GF4_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GF5_player_team[box_score_data_2021_condensed$GF5_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GF6_player_team[box_score_data_2021_condensed$GF6_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GF1_name[box_score_data_2021_condensed$GF1_name == 'Linus Nässén' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GF2_name[box_score_data_2021_condensed$GF2_name == 'Linus Nässén' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GF3_name[box_score_data_2021_condensed$GF3_name == 'Linus Nässén' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GF4_name[box_score_data_2021_condensed$GF4_name == 'Linus Nässén' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GF5_name[box_score_data_2021_condensed$GF5_name == 'Linus Nässén' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GF6_name[box_score_data_2021_condensed$GF6_name == 'Linus Nässén' & box_score_data_2021_condensed$gf_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'

box_score_data_2021_condensed$GA1_player_team[box_score_data_2021_condensed$GA1_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GA2_player_team[box_score_data_2021_condensed$GA2_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GA3_player_team[box_score_data_2021_condensed$GA3_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GA4_player_team[box_score_data_2021_condensed$GA4_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GA5_player_team[box_score_data_2021_condensed$GA5_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GA6_player_team[box_score_data_2021_condensed$GA6_player_team == 'Linus Nässén$Växjö Lakers HC' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)$Växjö Lakers HC'
box_score_data_2021_condensed$GA1_name[box_score_data_2021_condensed$GA1_name == 'Linus Nässén' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GA2_name[box_score_data_2021_condensed$GA2_name == 'Linus Nässén' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GA3_name[box_score_data_2021_condensed$GA3_name == 'Linus Nässén' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GA4_name[box_score_data_2021_condensed$GA4_name == 'Linus Nässén' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GA5_name[box_score_data_2021_condensed$GA5_name == 'Linus Nässén' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'
box_score_data_2021_condensed$GA6_name[box_score_data_2021_condensed$GA6_name == 'Linus Nässén' & box_score_data_2021_condensed$ga_team == 'Växjö Lakers HC'] <- 'Linus Nässén (D)'

#replace scoring
box_score_data_2021_condensed$goal_player_team[box_score_data_2021_condensed$goal_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$primary_assist_player_team[box_score_data_2021_condensed$primary_assist_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$secondary_assist_player_team[box_score_data_2021_condensed$secondary_assist_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$goal[box_score_data_2021_condensed$goal == 'Fredrik Forsberg' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)'
box_score_data_2021_condensed$primary_assist[box_score_data_2021_condensed$primary_assist == 'Fredrik Forsberg' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)'
box_score_data_2021_condensed$secondary_assist[box_score_data_2021_condensed$secondary_assist == 'Fredrik Forsberg' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)'

#replace on-ice GF and GA
box_score_data_2021_condensed$GF1_player_team[box_score_data_2021_condensed$GF1_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF2_player_team[box_score_data_2021_condensed$GF2_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF3_player_team[box_score_data_2021_condensed$GF3_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF4_player_team[box_score_data_2021_condensed$GF4_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF5_player_team[box_score_data_2021_condensed$GF5_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF6_player_team[box_score_data_2021_condensed$GF6_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF1_name[box_score_data_2021_condensed$GF1_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF2_name[box_score_data_2021_condensed$GF2_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF3_name[box_score_data_2021_condensed$GF3_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF4_name[box_score_data_2021_condensed$GF4_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF5_name[box_score_data_2021_condensed$GF5_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GF6_name[box_score_data_2021_condensed$GF6_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$gf_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'

box_score_data_2021_condensed$GA1_player_team[box_score_data_2021_condensed$GA1_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA2_player_team[box_score_data_2021_condensed$GA2_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA3_player_team[box_score_data_2021_condensed$GA3_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA4_player_team[box_score_data_2021_condensed$GA4_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA5_player_team[box_score_data_2021_condensed$GA5_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA6_player_team[box_score_data_2021_condensed$GA6_player_team == 'Fredrik Forsberg$HV 71' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA1_name[box_score_data_2021_condensed$GA1_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA2_name[box_score_data_2021_condensed$GA2_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA3_name[box_score_data_2021_condensed$GA3_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA4_name[box_score_data_2021_condensed$GA4_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA5_name[box_score_data_2021_condensed$GA5_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'
box_score_data_2021_condensed$GA6_name[box_score_data_2021_condensed$GA6_name == 'Fredrik Forsberg' & box_score_data_2021_condensed$ga_team == 'HV 71'] <- 'Fredrik Forsberg (RH)$HV 71'


#remove all rows with the 20/21 season - fix this later
box_score_data_all <- box_score_data_all %>%
  filter(season != '20/21')

box_score_data_all <- rbind(box_score_data_all, box_score_data_2021_condensed)

write_excel_csv(box_score_data_all, "box_score_data_all.csv")

# box_score_data_2021_condensed <- box_score_data_2021_condensed %>%
#   
#   mutate(goal_player_team = paste(goal, gf_team, sep = "$"),
#          primary_assist_player_team = paste(primary_assist, gf_team, sep = "$"),
#          secondary_assist_player_team = paste(secondary_assist, gf_team, sep = "$"),
#          
#          GF1_player_team = paste(GF1_name, gf_team, sep = "$"),
#          GF2_player_team = paste(GF2_name, gf_team, sep = "$"),
#          GF3_player_team = paste(GF3_name, gf_team, sep = "$"),
#          GF4_player_team = paste(GF4_name, gf_team, sep = "$"),
#          GF5_player_team = paste(GF5_name, gf_team, sep = "$"),
#          GF6_player_team = paste(GF6_name, gf_team, sep = "$"),
#          
#          GA1_player_team = paste(GA1_name, ga_team, sep = "$"),
#          GA2_player_team = paste(GA2_name, ga_team, sep = "$"),
#          GA3_player_team = paste(GA3_name, ga_team, sep = "$"),
#          GA4_player_team = paste(GA4_name, ga_team, sep = "$"),
#          GA5_player_team = paste(GA5_name, ga_team, sep = "$"),
#          GA6_player_team = paste(GA6_name, ga_team, sep = "$")
#   )

# Player Stats -----------------------------------------------------

#player_toi_data_2021$Pos[player_toi_data_2021$Spelare == 'Benjamin SmithB Smith' & player_toi_data_2021$name_date == 'Benjamin Smith2020-10-08'] <- 'F'
# player_toi_data_2021$swehockey_name[player_toi_data_2021$name == 'Linus Johnsson'] <- 'Linus Johnsson'
# player_toi_data_2021$player_team[player_toi_data_2021$name == 'Linus Johnsson'] <- 'Linus Johnsson$Färjestad BK'
# player_corsi_data_2021$swehockey_name[player_corsi_data_2021$name == 'Linus Johnsson'] <- 'Linus Johnsson'
# player_corsi_data_2021$player_team[player_corsi_data_2021$name == 'Linus Johnsson'] <- 'Linus Johnsson$Färjestad BK'



#Players from the 20/21 Season
player_team_IDs <- unique(player_toi_data_2021$player_team) %>%
  sort()

players_2021_new <- tibble(player = player_team_IDs)

get_5v5_data <- function(player) {
  
  GP <- player_toi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  fenwick_data <- player_corsi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '20/21') %>%
    dplyr::select(FF, FA, CF, CA)
  
  CF <- sum(fenwick_data$CF)
  CA <- sum(fenwick_data$CA)
  
  `CF%` <- (round(CF / (CF + CA),3)) * 100
  #`CF%` <- (signif(CF / (CF + CA),3)) * 100
  
  FF <- sum(fenwick_data$FF)
  FA <- sum(fenwick_data$FA)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  FenSh <- round((GF/FF) * 100, 1)
  FenSv <- 100 - round((GA/FA) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '5v5', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP, CF = CF, CA = CA, `CF%` = `CF%`, FenSh = FenSh, FenSv = FenSv)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  #data <- left_join(data, player_toi_data_2021, by = c("player" = "player_team")) %>% unique()

  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%

    mutate(Season = '20/21')

  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)

  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "CF", "CA", "CF%", "FenSh", "FenSv", "NHL Rights")] %>% dplyr::distinct()

  #data <- data %>% unique()

  # data <- data %>%
  #   .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
  #        "IPP", "CF", "CA", "CF%", "FenSh", "FenSv", "nhl_team")] %>% unique()
  
  return(data)
}

player_5v5_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_5v5_data(player)
  
  player_5v5_data_2021 <- rbind(player_5v5_data_2021, temp)
  
  print(player)
  
}

get_ev_data <- function(player) {
  
  GP <- player_toi_data_2021 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_2021 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$ESTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = 'All EV', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_es_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_ev_data(player)
  
  player_es_data_2021 <- rbind(player_es_data_2021, temp)
  
  print(player)
  
}

get_pp_data <- function(player) {
  
  GP <- player_toi_data_2021 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_2021 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$PPTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2)
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = 'All PP', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_pp_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_pp_data(player)
  
  player_pp_data_2021 <- rbind(player_pp_data_2021, temp)
  
  print(player)
  
}

get_5v4_data <- function(player) {
  
  GP <- player_toi_data_2021 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v4', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_5v4_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_5v4_data(player)
  
  player_5v4_data_2021 <- rbind(player_5v4_data_2021, temp)
  
  print(player)
  
}

get_5v3_data <- function(player) {
  
  GP <- player_toi_data_2021 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v3', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_5v3_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_5v3_data(player)
  
  player_5v3_data_2021 <- rbind(player_5v3_data_2021, temp)
  
  print(player)
  
}

get_sh_data <- function(player) {
  
  GP <- player_toi_data_2021 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_2021 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$SHTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2)
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'SH', manpower = 'All SH', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60,  IPP = IPP)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_sh_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_sh_data(player)
  
  player_sh_data_2021 <- rbind(player_sh_data_2021, temp)
  
  print(player)
  
}

get_3v3_data <- function(player) {
  
  GP <- player_toi_data_2021 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '3v3', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_3v3_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_3v3_data(player)
  
  player_3v3_data_2021 <- rbind(player_3v3_data_2021, temp)
  
  print(player)
  
}

get_4v4_data <- function(player) {
  
  GP <- player_toi_data_2021 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '4v4', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_4v4_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_4v4_data(player)
  
  player_4v4_data_2021 <- rbind(player_4v4_data_2021, temp)
  
  print(player)
  
}

get_1v0_data <- function(player) {
  
  GP <- player_toi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '1v0') %>%
    filter(season == '20/21') %>%
    nrow()
  
  data <- tibble(player = player, situation = 'PS/SO', manpower = '1v0', G = G, GP = GP)
  #data <- tibble(player = player, situation = 'PS/SO', manpower = '1v0', G = G)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  #data <- data %>%
    #.[,c("Player", "Season", "Team", "Pos", "GP", "G")]
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos","GP", "G", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
  
}

player_1v0_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_1v0_data(player)
  
  player_1v0_data_2021 <- rbind(player_1v0_data_2021, temp)
  
  print(player)
  
}

get_allsits_data <- function(player) {
  
  GP <- player_toi_data_2021 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_2021 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$TOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  
  data <- tibble(player = player, situation = 'All', manpower = 'All', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60, GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_allsits_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_allsits_data(player)
  
  player_allsits_data_2021 <- rbind(player_allsits_data_2021, temp)
  
  print(player)
  
}

get_eng_data <- function(player) {
  
  GP <- player_toi_data_2021 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'ENG', manpower = 'ENG', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
           Player = player,
           Situation = situation,
           Manpower = manpower) %>%
    
    mutate(Season = '20/21')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_eng_data_2021 <- c()
for(player in players_2021_new$player[1:length(players_2021_new$player)]) {
  
  temp <- get_eng_data(player)
  
  player_eng_data_2021 <- rbind(player_eng_data_2021, temp)
  
  print(player)
  
}

#Remove the Orebro N/A that doesn't make any sense
player_5v5_data_2021 <- player_5v5_data_2021 %>% filter(Player != 'NA')
player_es_data_2021 <- player_es_data_2021 %>% filter(Player != 'NA')
player_pp_data_2021 <- player_pp_data_2021 %>% filter(Player != 'NA')
player_5v4_data_2021 <- player_5v4_data_2021 %>% filter(Player != 'NA')
player_5v3_data_2021 <- player_5v3_data_2021 %>% filter(Player != 'NA')
player_sh_data_2021 <- player_sh_data_2021 %>% filter(Player != 'NA')
player_3v3_data_2021 <- player_3v3_data_2021 %>% filter(Player != 'NA')
player_4v4_data_2021 <- player_4v4_data_2021 %>% filter(Player != 'NA')
player_1v0_data_2021 <- player_1v0_data_2021 %>% filter(Player != 'NA')
player_allsits_data_2021 <- player_allsits_data_2021 %>% filter(Player != 'NA')
player_eng_data_2021 <- player_eng_data_2021 %>% filter(Player != 'NA')

#Make N/As equal 0
player_5v5_data_2021[is.na(player_5v5_data_2021)] <- 0
player_es_data_2021[is.na(player_es_data_2021)] <- 0
player_pp_data_2021[is.na(player_pp_data_2021)] <- 0
player_5v4_data_2021[is.na(player_5v4_data_2021)] <- 0
player_5v3_data_2021[is.na(player_5v3_data_2021)] <- 0
player_sh_data_2021[is.na(player_sh_data_2021)] <- 0
player_3v3_data_2021[is.na(player_3v3_data_2021)] <- 0
player_4v4_data_2021[is.na(player_4v4_data_2021)] <- 0
player_1v0_data_2021[is.na(player_1v0_data_2021)] <- 0
player_allsits_data_2021[is.na(player_allsits_data_2021)] <- 0
player_eng_data_2021[is.na(player_eng_data_2021)] <- 0

#get rid of 20/21 stats in the master frame, so I can then just bind it - prob figure out better way (update frames?)
player_5v5_data_master <- player_5v5_data_master %>% filter(Season != "20/21")
player_es_data_master <- player_es_data_master %>% filter(Season != "20/21")
player_pp_data_master <- player_pp_data_master %>% filter(Season != "20/21")
player_5v4_data_master <- player_5v4_data_master %>% filter(Season != "20/21")
player_5v3_data_master <- player_5v3_data_master %>% filter(Season != "20/21")
player_sh_data_master <- player_sh_data_master %>% filter(Season != "20/21")
player_3v3_data_master <- player_3v3_data_master %>% filter(Season != "20/21")
player_4v4_data_master <- player_4v4_data_master %>% filter(Season != "20/21")
player_1v0_data_master <- player_1v0_data_master %>% filter(Season != "20/21")
player_allsits_data_master <- player_allsits_data_master %>% filter(Season != "20/21")
player_eng_data_master <- player_eng_data_master %>% filter(Season != "20/21")

#Join in with master data frames
#come back to this the next time - can't keep binding new shit
player_5v5_data_master <- rbind(player_5v5_data_master, player_5v5_data_2021)
player_es_data_master <- rbind(player_es_data_master, player_es_data_2021)
player_pp_data_master <- rbind(player_pp_data_master, player_pp_data_2021)
player_5v4_data_master <- rbind(player_5v4_data_master, player_5v4_data_2021) 
player_5v3_data_master <- rbind(player_5v3_data_master, player_5v3_data_2021)
player_sh_data_master <- rbind(player_sh_data_master, player_sh_data_2021)
player_3v3_data_master <- rbind(player_3v3_data_master, player_3v3_data_2021)
player_4v4_data_master <- rbind(player_4v4_data_master, player_4v4_data_2021)
player_1v0_data_master <- rbind(player_1v0_data_master, player_1v0_data_2021)
player_allsits_data_master <- rbind(player_allsits_data_master, player_allsits_data_2021)
player_eng_data_master <- rbind(player_eng_data_master, player_eng_data_2021)

# #Bind them all together - one time, only when all years have had updates (new columns)
# player_5v5_data_master <- rbind(player_5v5_data_1718, player_5v5_data_1819, player_5v5_data_1920, player_5v5_data_2021)
# player_es_data_master <- rbind(player_es_data_1718, player_es_data_1819, player_es_data_1920, player_es_data_2021)
# player_pp_data_master <- rbind(player_pp_data_1718, player_pp_data_1819, player_pp_data_1920, player_pp_data_2021)
# player_5v4_data_master <- rbind(player_5v4_data_1718, player_5v4_data_1819, player_5v4_data_1920, player_5v4_data_2021)
# player_5v3_data_master <- rbind(player_5v3_data_1718, player_5v3_data_1819, player_5v3_data_1920, player_5v3_data_2021)
# player_sh_data_master <- rbind(player_sh_data_1718, player_sh_data_1819, player_sh_data_1920, player_sh_data_2021)
# player_3v3_data_master <- rbind(player_3v3_data_1718, player_3v3_data_1819, player_3v3_data_1920, player_3v3_data_2021)
# player_4v4_data_master <- rbind(player_4v4_data_1718, player_4v4_data_1819, player_4v4_data_1920, player_4v4_data_2021)
# player_1v0_data_master <- rbind(player_1v0_data_1718, player_1v0_data_1819, player_1v0_data_1920, player_1v0_data_2021)
# player_allsits_data_master <- rbind(player_allsits_data_1718, player_allsits_data_1819, player_allsits_data_1920, player_allsits_data_2021)
# player_eng_data_master <- rbind(player_eng_data_1718, player_eng_data_1819, player_eng_data_1920, player_eng_data_2021)

#Export to CSVs for Shiny file
write_excel_csv(player_5v5_data_master, "player_5v5_data_master.csv")
write_excel_csv(player_es_data_master, "player_es_data_master.csv")
write_excel_csv(player_pp_data_master, "player_pp_data_master.csv")
write_excel_csv(player_5v4_data_master, "player_5v4_data_master.csv")
write_excel_csv(player_5v3_data_master, "player_5v3_data_master.csv")
write_excel_csv(player_sh_data_master, "player_sh_data_master.csv")
write_excel_csv(player_3v3_data_master, "player_3v3_data_master.csv")
write_excel_csv(player_4v4_data_master, "player_4v4_data_master.csv")
write_excel_csv(player_1v0_data_master, "player_1v0_data_master.csv")
write_excel_csv(player_allsits_data_master, "player_allsits_data_master.csv")
write_excel_csv(player_eng_data_master, "player_eng_data_master.csv")

# Team Stats --------------------------------------------------------------

get_team_5v5 <- function(team, season_year) {
  
  GP <- team_corsi_data_master_team %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    nrow()
  
  corsi <- team_corsi_data_master_team %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    summarise(CF = sum(CF), CA = sum(CA),
              FF = sum(FF), FA = sum(FA))
  
  CF <- corsi$CF
  CA <- corsi$CA
  FF <- corsi$FF
  FA <- corsi$FA
  
  `CF%` <- round(CF / (CF + CA), 3) * 100
  `FF%` <- round(FF / (FF + FA), 3) * 100
  
  GF <- box_score_data_all %>%
    filter(gf_team == team) %>%
    filter(season == season_year) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  
  GA <- box_score_data_all %>%
    filter(ga_team == team) %>%
    filter(season == season_year) %>%
    filter(game_situation == '5v5') %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA), 3) * 100
  
  FenSh <- round(GF / FF, 3) * 100
  FenSv <- 100 - (round(GA / FA, 3) * 100)
  
  data <- tibble(team = team, Season = season_year, CF = CF, CA = CA, `CF%` = `CF%`, FF = FF, FA = FA, `FF%` = `FF%`,  
                 GF = GF, GA = GA, `GF%` = `GF%`, FenSh = FenSh, FenSv = FenSv, GP = GP)
  
  data <- merge(data, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_name")],
                by.x = "team", by.y = "swehockey_team_name")
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name) %>%
    .[,c("Team", "Season", "GP", "GF", "GA", "GF%", "CF", "CA", "CF%", "FF", "FA", "FF%", "FenSh", "FenSv")]
  
  return(data)
  
}

team_5v5_data <- c()
for(season_year in c('17/18', '18/19', '19/20', '20/21')) {
  
  for (team in shl_team_dictionary$swehockey_team_name[1:20]) {
    
    temp <- get_team_5v5(team, season_year)
    
    team_5v5_data <- rbind(team_5v5_data, temp)
    
    team_5v5_data <- subset(team_5v5_data, CF != 0)
    
    print(team)
    rm(temp)
    
  }
  
}

get_team_ev <- function(team, season_year) {
  
  GP <- team_corsi_data_master_team %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    nrow()
  
  GF <- box_score_data_all %>%
    filter(gf_team == team) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'EV') %>%
    nrow()
  
  GA <- box_score_data_all %>%
    filter(ga_team == team) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'EV') %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA), 3) * 100
  
  pp_toi <- team_pp_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    summarise(PPTOI = sum(TOI_PP))
  
  pk_toi <- team_pk_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    summarise(SHTOI = sum(TOI_SH))
  
  TOI <- (GP * 60) - pp_toi$PPTOI - pk_toi$SHTOI
  
  GF60 <- round((GF/TOI) * 60, 2)
  GA60 <- round((GA/TOI) * 60, 2)
  
  data <- tibble(team = team, Season = season_year, TOI = TOI, GF = GF, GA = GA, `GF%` = `GF%`, GF60 = GF60, GA60 = GA60,
                 GP = GP)
  
  data <- merge(data, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_name")],
                by.x = "team", by.y = "swehockey_team_name")
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name) %>%
    .[,c("Team", "Season", "GP", "TOI", "GF", "GA", "GF%", "GF60", "GA60")]
  
  return(data)
  
}

team_ev_data <- c()
for(season_year in c('17/18', '18/19', '19/20', '20/21')) {
  
  for (team in shl_team_dictionary$swehockey_team_name[1:20]) {
    
    temp <- get_team_ev(team, season_year)
    
    team_ev_data <- rbind(team_ev_data, temp)
    
    team_ev_data <- subset(team_ev_data, GP != 0)
    
    print(team)
    rm(temp)
    
  }
  
}

get_team_pp <- function(team, season_year) {
  
  GP <- team_corsi_data_master_team %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    nrow()
  
  GF <- box_score_data_all %>%
    filter(gf_team == team) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  GA <- box_score_data_all %>%
    filter(ga_team == team) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA), 3) * 100
  
  pp_toi <- team_pp_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    summarise(PPTOI = sum(TOI_PP))
  
  TOI <- pp_toi$PPTOI
  
  GF60 <- round((GF/TOI) * 60, 2)
  GA60 <- round((GA/TOI) * 60, 2)
  
  shots <- team_shotsfor_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    summarise(SOG = sum(SOG_PP))
  
  SOG <- shots$SOG
  
  SOG60 <- round((SOG/TOI) * 60, 1)
  
  `Sh%` <- round(GF / SOG, 3) * 100
  
  data <- tibble(team = team, Season = season_year, TOI = TOI, GF = GF, GA = GA, `GF%` = `GF%`, GF60 = GF60, GA60 = GA60,
                 SOG = SOG, `Sh%` = `Sh%`, SOG60 = SOG60, GP = GP)
  
  data <- merge(data, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_name")],
                by.x = "team", by.y = "swehockey_team_name")
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name) %>%
    .[,c("Team", "Season", "GP", "TOI", "GF", "GA", "GF%", "GF60", "GA60", "SOG", "SOG60", "Sh%")]
  
  return(data)
  
}

team_pp_data <- c()
for(season_year in c('17/18', '18/19', '19/20', '20/21')) {
  
  for (team in shl_team_dictionary$swehockey_team_name[1:20]) {
    
    temp <- get_team_pp(team, season_year)
    
    team_pp_data <- rbind(team_pp_data, temp)
    
    team_pp_data <- subset(team_pp_data, GP != 0)
    
    print(team)
    rm(temp)
    
  }
  
}

team_shotsagainst_data_master <- team_shotsagainst_data_master %>%
  mutate_at('SOGA_PK', as.numeric)

get_team_pk <- function(team, season_year) {
  
  GP <- team_corsi_data_master_team %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    nrow()
  
  GF <- box_score_data_all %>%
    filter(gf_team == team) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SH') %>%
    nrow()
  
  GA <- box_score_data_all %>%
    filter(ga_team == team) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PP') %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA), 3) * 100
  
  pk_toi <- team_pk_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    summarise(SHTOI = sum(TOI_SH))
  
  TOI <- pk_toi$SHTOI
  
  GF60 <- round((GF/TOI) * 60, 2)
  GA60 <- round((GA/TOI) * 60, 2)
  
  shots <- team_shotsagainst_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    summarise(SOGA = sum(SOGA_PK))
  
  SOGA <- shots$SOGA
  
  SOGA60 <- round((SOGA/TOI) * 60, 1)
  
  `Sv%` <- 100 - (round(GA / SOGA, 3) * 100)
  
  data <- tibble(team = team, Season = season_year, TOI = TOI, GF = GF, GA = GA, `GF%` = `GF%`, GF60 = GF60, GA60 = GA60,
                 SOGA = SOGA, `Sv%` = `Sv%`, SOGA60 = SOGA60, GP = GP)
  
  data <- merge(data, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_name")],
                by.x = "team", by.y = "swehockey_team_name")
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name) %>%
    .[,c("Team", "Season", "GP", "TOI", "GF", "GA", "GF%", "GF60", "GA60", "SOGA", "SOGA60", "Sv%")]
  
  return(data)
  
}

team_pk_data <- c()
for(season_year in c('17/18', '18/19', '19/20', '20/21')) {
  
  for (team in shl_team_dictionary$swehockey_team_name[1:20]) {
    
    temp <- get_team_pk(team, season_year)
    
    team_pk_data <- rbind(team_pk_data, temp)
    
    team_pk_data <- subset(team_pk_data, GP != 0)
    
    print(team)
    rm(temp)
    
  }
  
}

get_team_all <- function(team, season_year) {
  
  GP <- team_corsi_data_master_team %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    nrow()
  
  GF <- box_score_data_all %>%
    filter(gf_team == team) %>%
    filter(season == season_year) %>%
    #filter(game_situation_general == 'EV') %>%
    nrow()
  
  GA <- box_score_data_all %>%
    filter(ga_team == team) %>%
    filter(season == season_year) %>%
    #filter(game_situation_general == 'EV') %>%
    nrow()
  
  `GF%` <- round(GF / (GF + GA), 3) * 100
  
  TOI <- GP * 60
  
  GF60 <- round((GF/TOI) * 60, 2)
  GA60 <- round((GA/TOI) * 60, 2)
  
  shots <- team_shotsfor_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    
    
    data <- tibble(team = team, Season = season_year, TOI = TOI, GF = GF, GA = GA, `GF%` = `GF%`, GF60 = GF60, GA60 = GA60,
                   GP = GP)
  
  data <- merge(data, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_name")],
                by.x = "team", by.y = "swehockey_team_name")
  
  data <- data %>%
    rename(Team = shlse_team_name) %>%
    .[,c("Team", "Season", "GP", "TOI", "GF", "GA", "GF%", "GF60", "GA60")]
  
  return(data)
  
}

write_excel_csv(team_5v5_data, "team_5v5_data.csv")
write_excel_csv(team_pp_data, "team_pp_data.csv")
write_excel_csv(team_pk_data, "team_pk_data.csv")
write_excel_csv(team_ev_data, "team_ev_data.csv")

# Run Once Per Season -----------------------------------------------------

#SHL 20/21 Team Dictionary

shl_team_dictionary_2021 <- shl_team_dictionary %>%
  slice(2, 3, 4, 5, 6, 8, 9, 10, 11, 14, 15, 16, 17, 20)


#SHL 20/21 Schedule

#base schedule from stats.swehockey
shl_schedule_2021 <- read_csv("shl_schedule_2021.csv")

#add a game number
shl_schedule_2021 <- shl_schedule_2021 %>%
  dplyr::mutate(game_number = row_number())

#join in other versions of team names
shl_schedule_2021 <- merge(shl_schedule_2021, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")],
                           by.x = "home_team", by.y = "swehockey_team_name")

shl_schedule_2021 <- shl_schedule_2021 %>%
  dplyr::rename(home_team_id = shlse_team_id, home_team_name_double = shlse_team_name_double)

shl_schedule_2021 <- merge(shl_schedule_2021, shl_team_dictionary[,c("swehockey_team_name", "shlse_team_id", "shlse_team_name_double")],
                           by.x = "away_team", by.y = "swehockey_team_name")

shl_schedule_2021 <- shl_schedule_2021 %>%
  dplyr::rename(away_team_id = shlse_team_id, away_team_name_double = shlse_team_name_double)

#combined team IDs
shl_schedule_2021 <- shl_schedule_2021 %>%
  dplyr::mutate(combined_ids = paste(home_team_id, away_team_id, sep = "-"))

shl_schedule_2021 <- shl_schedule_2021 %>%
  dplyr::mutate(combined_team_names_double = paste(home_team_name_double, away_team_name_double, sep = "-"))

#date format
shl_schedule_2021 <- shl_schedule_2021 %>%
  dplyr::mutate(date2 = as.Date(shl_schedule_2021$date, "%m/%d/%Y"))

#reorder columns
shl_schedule_2021 <- shl_schedule_2021[c("game_number", "date2", "home_team", "away_team", "combined_teams", "home_team_id",
                                         "away_team_id", "combined_ids", "home_team_name_double", "away_team_name_double",
                                         "combined_team_names_double", "date")]

#sort by game number
shl_schedule_2021 <- shl_schedule_2021[order(shl_schedule_2021$game_number),]

#all game dates from the season
shl_dates_2021 <- unique(shl_schedule_2021$date2)


#SHL 20/21 Schedule (Master Schedule)

#Brynäs
bif_schedule_2021 <- subset(shl_schedule_2021, grepl("Brynäs IF", shl_schedule_2021$combined_teams))

bif_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "BrynäsBIF") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> bif_schedule_2021

#Djurgården
dif_schedule_2021 <- subset(shl_schedule_2021, grepl("Djurgårdens IF", shl_schedule_2021$combined_teams))

dif_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "DjurgårdenDIF") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> dif_schedule_2021

#Frölunda
fhc_schedule_2021 <- subset(shl_schedule_2021, grepl("Frölunda HC", shl_schedule_2021$combined_teams))

fhc_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "FrölundaFHC") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> fhc_schedule_2021

#Färjestad
fbk_schedule_2021 <- subset(shl_schedule_2021, grepl("Färjestad BK", shl_schedule_2021$combined_teams))

fbk_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "FärjestadFBK") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> fbk_schedule_2021

#HV71
hv_schedule_2021 <- subset(shl_schedule_2021, grepl("HV 71", shl_schedule_2021$combined_teams))

hv_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "HV71HV71") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> hv_schedule_2021

#Leksand
lif_schedule_2021 <- subset(shl_schedule_2021, grepl("Leksands IF", shl_schedule_2021$combined_teams))

lif_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "LeksandLIF") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> lif_schedule_2021

#Linköping
lhc_schedule_2021 <- subset(shl_schedule_2021, grepl("Linköping HC", shl_schedule_2021$combined_teams))

lhc_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "LinköpingLHC") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> lhc_schedule_2021

#Luleå
lhf_schedule_2021 <- subset(shl_schedule_2021, grepl("Luleå HF", shl_schedule_2021$combined_teams))

lhf_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "LuleåLHF") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> lhf_schedule_2021

#Malmö
mif_schedule_2021 <- subset(shl_schedule_2021, grepl("Malmö Redhawks", shl_schedule_2021$combined_teams))

mif_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "MalmöMIF") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> mif_schedule_2021

#Oskarshamn
iko_schedule_2021 <- subset(shl_schedule_2021, grepl("IK Oskarshamn", shl_schedule_2021$combined_teams))

iko_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "OskarshamnIKO") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> iko_schedule_2021

#Örebro
ohk_schedule_2021 <- subset(shl_schedule_2021, grepl("Örebro HK", shl_schedule_2021$combined_teams))

ohk_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "ÖrebroÖRE") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> ohk_schedule_2021

#Rögle
rbk_schedule_2021 <- subset(shl_schedule_2021, grepl("Rögle BK", shl_schedule_2021$combined_teams))

rbk_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "RögleRBK") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> rbk_schedule_2021

#Skellefteå
ske_schedule_2021 <- subset(shl_schedule_2021, grepl("Skellefteå AIK", shl_schedule_2021$combined_teams))

ske_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "SkellefteåSKE") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> ske_schedule_2021

#Växjö
vax_schedule_2021 <- subset(shl_schedule_2021, grepl("Växjö Lakers", shl_schedule_2021$combined_teams))

vax_schedule_2021 %>%
  dplyr::mutate(game_number = row_number()) %>%
  dplyr::mutate(team = "VäxjöVLH") %>%
  dplyr::mutate(team_date = paste(team, date2, sep = '')) -> vax_schedule_2021


shl_schedule_2021_master <- rbind(bif_schedule_2021, dif_schedule_2021, fbk_schedule_2021,
                                   fhc_schedule_2021, hv_schedule_2021, iko_schedule_2021,
                                   lhc_schedule_2021, lhf_schedule_2021, lif_schedule_2021,
                                   mif_schedule_2021, ohk_schedule_2021, rbk_schedule_2021,
                                   ske_schedule_2021, vax_schedule_2021)

master_team_schedule_2021 <- rbind(bif_schedule_2021, dif_schedule_2021, fbk_schedule_2021,
                                  fhc_schedule_2021, hv_schedule_2021, iko_schedule_2021,
                                  lhc_schedule_2021, lhf_schedule_2021, lif_schedule_2021,
                                  mif_schedule_2021, ohk_schedule_2021, rbk_schedule_2021,
                                  ske_schedule_2021, vax_schedule_2021)

#at some point it could be worth going in to add whatever columns are necessary to master_team_schedule_2021 
#to bind it to master_team_schedule

# #add additional columns that are necessary here for future functions
# master_team_schedule_2021 <- master_team_schedule_2021 %>%
#   mutate(combined_teams_date = paste(combined_teams, date2, sep = ''),
#          home_team_date = paste(home_team, date2, sep = ''),
#          away_team_date = paste(away_team, date2, sep = ''))

# 
# shl_schedule_2021 <- merge(shl_schedule_2021, shl_team_dictionary[,c("shlse_team_name_double", "swehockey_team_name")],
#                         by.x = "team", by.y = "shlse_team_name_double")
# 
# merge(master_team_schedule_1920, shl_team_dictionary[,c("shlse_team_name_double", "swehockey_team_name")],
#       by.x = "team", by.y = "shlse_team_name_double") -> master_team_schedule_1920
# 
# master_team_schedule_1920 %>%
#   mutate(swehockey_team_date = paste(swehockey_team_name, date2, sep = '')) -> master_team_schedule_1920

#J20 Schedules

#import from Excel
j20_sodra_schedule_2021 <- read_csv("j20_sodra_schedule_2021.csv")
j20_norra_schedule_2021 <- read_csv("j20_norra_schedule_2021.csv")

#one combined J20 base view
j20_schedule_2021 <- rbind(j20_sodra_schedule_2021, j20_norra_schedule_2021)

#add a game number
j20_schedule_2021 <- j20_schedule_2021 %>%
  mutate(game_number = row_number())

#friendly date format
j20_schedule_2021 <- j20_schedule_2021 %>%
  mutate(date2 = as.Date(j20_schedule_2021$Date, "%m/%d/%Y"))

#re-order columns
j20_schedule_2021 <- j20_schedule_2021[c(6, 5, 2:4)]

#set date format again
j20_schedule_2021$date2 <- as.Date(j20_schedule_2021$date2)



master_team_schedule_2021_v2 <- master_team_schedule_2021

master_team_schedule_2021_v2 <- left_join(master_team_schedule_2021_v2, shl_team_dictionary_2021[,c("shlse_team_name_double", "swehockey_team_name")],
                                          by = c("team" = "shlse_team_name_double"))

master_team_schedule_2021_v2 <- master_team_schedule_2021_v2 %>%
  mutate(swehockey_team_date = paste0(swehockey_team_name, date))


swehockey_league_roster <- function(url) {
  
  rosters <- read_html(url)
  
  table <- rosters %>%
    html_nodes("strong") %>%
    html_text() %>%
    tibble::enframe(name = NULL) %>%
    #html_table(fill = TRUE, header = TRUE) #%>%
    #as.data.frame(fill = TRUE) 
  
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
    mutate(player7 = ifelse(player6 %in% goalies, paste(player6, goalie, sep = ''), paste(player6, sep = '')))
  
  #position
  positions <- rosters %>%
    html_nodes("strong") %>%
    html_text() %>%
    tibble::enframe(name = NULL)
    
  return(table)
  
}


# HA - Team Info ----------------------------------------------------------

ha_swehockey_team_names <- c("AIK", "Almtuna IS", "BIK Karlskoga", "HC Vita Hästen", "IF Björklöven",
                             "Kristianstads IK", "MODO Hockey", "Mora IK", "Södertälje SK", "Timrå IK",
                             "Tingsryds AIF", "Väsby IK HK", "Västerviks IK", "Västerås IK")

ha_team_names <- c("AIK", "Almtuna", "Karlskoga", "Vita Hästen", "Björklöven",
                             "Kristianstad", "MODO", "Mora", "Södertälje", "Timrå",
                             "Tingsryd", "Väsby", "Västervik", "Västerås")

ha_ep_team_names <- c("AIK", "Almtuna IS", "BIK Karlskoga", "HC Vita Hästen", "IF Björklöven",
                      "Kristianstads IK", "MODO Hockey", "Mora IK", "Södertälje SK", "Timrå IK",
                      "Tingsryds AIF", "Väsby IK", "Västerviks IK", "Västerås IK")

ha_team_dictionary <- tibble(swehockey_team_name = ha_swehockey_team_names,
                             team_name = ha_team_names,
                             ep_team_name = ha_ep_team_names)


# HA - Player Info --------------------------------------------------------

ha_player_path <- "/v1/player-stats?limit=1000&sort=-season&league=hockeyallsvenskan&season=2020-2021&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H"
#ha_player_path <- "/v1/game-logs?limit=1000&sort=-game.dateTime&player=20065&game.league=hockeyallsvenskan&game.season=2020-2021&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H"

response <- ep_api(ha_player_path)

jsonRespText <- content(response, as = "text", encoding = "UTF-8") 
df <- fromJSON(jsonRespText)
tibble <- as_tibble(df$data)

ha_players <- tibble(ep_id = tibble$player$id,
                     ep_name = tibble$player$name,
                     position = tibble$player$position,
                     team = tibble$team$name,
                     dob = tibble$player$dateOfBirth) %>%
  
  dplyr::filter(position != 'G')

ha_player_lookup_2021 <- read.csv("ha_player_lookup_2021.csv", stringsAsFactors = FALSE)

ha_player_lookup_2021_update <- tibble(swehockey = c("Erik Ullman"),
                                    ep = c("Erik Ullman"))

ha_player_lookup_2021 <- rbind(ha_player_lookup_2021, ha_player_lookup_2021_update)

#order the lookup table in alphabetical order
ha_player_lookup_2021 <- ha_player_lookup_2021 %>%
  plyr::arrange(swehockey)


#create a player DB
ha_player_db <- left_join(ha_players, ha_player_lookup_2021, by = c("ep_name" = "ep"))

ha_player_db <- left_join(ha_player_db, ha_team_dictionary, by = c("team" = "ep_team_name"))

ha_player_db <- ha_player_db %>%
  mutate(player_team = paste(swehockey, swehockey_team_name, sep = '$'))

write_excel_csv(ha_player_db, "ha_player_db.csv")

#NHL Rights
ha_nhl_rights <- c()
for(id in ha_player_db$ep_id[1:length(ha_player_db$ep_id)]) {
  
  path <- paste0("/v1/nhl-rights?limit=100&sort=player&player=", id, "&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H")
  
  response <- ep_api(path)
  
  jsonRespText <- content(response, as = "text", encoding = "UTF-8") 
  df <- fromJSON(jsonRespText)
  tibble <- as_tibble(df$data)
  
  temp <- tibble(playerid = tibble$player$id,
                 player = tibble$player$name,
                 nhl_team = tibble$team$name)
  
  ha_nhl_rights <- rbind(ha_nhl_rights, temp)
  
  rm(temp)
  
}

ha_nhl_rights <- left_join(ha_nhl_rights, nhl_teams, by = c("nhl_team" = "full"))

ha_nhl_rights <- left_join(ha_nhl_rights, ha_player_lookup_2021, by = c("player" = "ep"))

#lookup v2 for the NHL Rights
ha_player_lookup_2021_v2 <- left_join(ha_player_lookup_2021, ha_nhl_rights, by = c("ep" = "player")) %>%
  dplyr::select(-c(playerid, nhl_team, swehockey.y)) %>%
  dplyr::rename(swehockey = swehockey.x)

ha_player_lookup_2021_v2[is.na(ha_player_lookup_2021_v2)] <- "None"

# HA - Games & Schedule ---------------------------------------------------
ha_box_score_urls <- get_schedule("Allsvenskan", c("2020-21"))

ha_gameIDs <- str_split(ha_box_score_urls$url, "/", simplify = TRUE)[,6]
ha_gameID_table <- tibble(id = ha_gameIDs, season = ha_box_score_urls$season, league = "SHL")

# HA - Swehockey Functions ------------------------------------------------
ha_get_lineup <- function(id) {
  
  url <- read_html(paste('https://stats.swehockey.se/Game/LineUps/', id, sep=''))
  teams <- url %>%
    html_nodes(xpath='//h2') %>%
    html_text() %>%
    tibble::enframe(name = NULL) %>%
    .[1,]
  
  teams <- teams %>%
    str_split(., "\\-", simplify = TRUE)
  
  home_team <- str_squish(teams[,1])
  away_team <- str_squish(teams[,2])
  
  #extract the date
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
  
  raw_lineup <- raw_lineup[,c(5,13,14)]
  
  raw_lineup <- raw_lineup %>%
    dplyr::rename(jersey = number2, player = player7)
  
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
  
  lineup_final <- lineup_final[,c("gameid", "date", "team", "jersey", "player", "uid", "team_date")]
  
  lineup_final <- merge(lineup_final, ha_master_team_schedule_2021[,c("team_date", "game_number")],
                        by.x = "team_date", by.y = "team_date")
  
  lineup_final <- lineup_final[,c("gameid", "date", "team", "jersey", "player", "uid", "team_date", "game_number")]
  
  return(lineup_final)
  
}

ha_get_box_score <- function(id) {
  
  url <- read_html(paste('https://stats.swehockey.se/Game/Events/', id, sep=''))
  
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
    dplyr::select(-scorers, -points)
  
  
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
      dplyr::rename(GF1 = V1, GF2 = V2, GF3 = V3, GF4 = V4, GF5 = V5, GF6 = V6)
  }
  
  if(ncol(gf) == 5) {
    gf <- gf %>%
      dplyr::rename(GF1 = V1, GF2 = V2, GF3 = V3, GF4 = V4, GF5 = V5)
  }
  
  if(ncol(gf) == 4) {
    gf <- gf %>%
      dplyr::rename(GF1 = V1, GF2 = V2, GF3 = V3, GF4 = V4)
  }
  
  if(ncol(gf) == 1) {
    gf <- gf %>%
      dplyr::rename(GF1 = V1)
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
      dplyr::rename(GA1 = V1, GA2 = V2, GA3 = V3, GA4 = V4, GA5 = V5, GA6 = V6)
  }
  
  if(ncol(ga) == 5) {
    ga <- ga %>%
      dplyr::rename(GA1 = V1, GA2 = V2, GA3 = V3, GA4 = V4, GA5 = V5)
  }
  
  if(ncol(ga) == 4) {
    ga <- ga %>%
      dplyr::rename(GA1 = V1, GA2 = V2, GA3 = V3, GA4 = V4)
  }
  
  if(ncol(ga) == 1) {
    ga <- ga %>%
      dplyr::rename(GA1 = V1)
  }
  
  box_score_data <- bind_cols(box_score_data, gf, ga) %>%
    dplyr::select(-players)
  
  box_score_data %>%
    mutate(home_team = home_team, away_team = away_team) %>%
    left_join(swehockey_teams, by = c("team" = "abbreviation")) %>%
    dplyr::rename(gf_team = full_name) %>%
    mutate(ga_team = ifelse(gf_team == home_team, away_team, home_team)) %>%
    mutate(date = date) %>%
    mutate(gameid = id) %>%
    mutate(gf_team_date = paste(gf_team, date, sep = '')) %>%
    mutate(ga_team_date = paste(ga_team, date, sep = '')) -> box_score_data
  
  box_score_data <- merge(box_score_data, ha_master_team_schedule_2021[,c("team_date", "game_number")],
                          by.x = "gf_team_date", by.y = "team_date") %>%
    
    dplyr::rename(gf_team_game_number = game_number)
  
  box_score_data <- merge(box_score_data, ha_master_team_schedule_2021[,c("team_date", "game_number")],
                          by.x = "ga_team_date", by.y = "team_date") %>%
    
    dplyr::rename(ga_team_game_number = game_number)
  
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

# HA - Swehockey Boxscore Data --------------------------------------------

ha_box_score_data_2021_raw_update <- c()
for (id in ha_gameID_table$id[39:42]) {
  
  temp <- ha_get_box_score(id)
  
  ha_box_score_data_2021_raw_update <- rbind.fill(ha_box_score_data_2021_raw_update, temp)
  
  print(id)
  rm(temp)
  
}

#write_excel_csv(ha_box_score_data_2021_raw_update, "ha_box_score_data_2021_raw_update.csv")

#ha_box_score_data_2021_raw_update <- read.csv("ha_box_score_data_2021_raw_update.csv", stringsAsFactors = FALSE)

# HA - Swehockey Boxscore Data - Enriched -----------------------------------

#step 1 - join in names from the uid (team, date, jersey number)
ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GF1_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GF2_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GF3_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GF4_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GF5_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GF6_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GF6_name = player)


ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GA1_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GA2_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GA3_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GA4_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GA5_name = player)

ha_box_score_data_2021_raw_update <- merge(ha_box_score_data_2021_raw_update, ha_lineup_data_2021[,c("player", "uid")],
                                        by.x = "GA6_uid", by.y = "uid", all.x = TRUE) %>%
  dplyr::rename(GA6_name = player)

#step 2 - concatenate all GF and GA names into their own columns
ha_box_score_data_2021_raw_update <- ha_box_score_data_2021_raw_update %>%
  mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name, sep = ''),
         GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name, sep = ''))

#step 3 - count the number of goalies and NAs to assess manpower
ha_box_score_data_2021_raw_update <- ha_box_score_data_2021_raw_update %>%
  mutate(GF_GK = str_count(ha_box_score_data_2021_raw_update$GF_names, "GK"),
         GF_NA = str_count(ha_box_score_data_2021_raw_update$GF_names, "NA")) %>%
  
  mutate(GF_skaters = 6 - GF_GK - GF_NA) %>%
  
  mutate(GA_GK = str_count(ha_box_score_data_2021_raw_update$GA_names, "GK"),
         GA_NA = str_count(ha_box_score_data_2021_raw_update$GA_names, "NA")) %>%
  
  mutate(GA_skaters = 6 - GA_GK - GA_NA)

#step 4 - assign a game state
ha_box_score_data_2021_raw_update <- ha_box_score_data_2021_raw_update %>%
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

ha_box_score_data_2021_raw_update <- ha_box_score_data_2021_raw_update %>%
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

ha_box_score_data_2021_raw <- rbind(ha_box_score_data_2021_raw, ha_box_score_data_2021_raw_update)

# HA - Swehockey Boxscore Data - Condensed --------------------------------

ha_box_score_data_2021_condensed <- ha_box_score_data_2021_raw %>%
  
  dplyr::select(gameid, date, home_team, away_team, goal, primary_assist, secondary_assist, game_situation,
                game_situation_general, gf_team, gf_team_game_number, ga_team, ga_team_game_number, GF_names, GA_names,
                GF_skaters, GA_skaters, GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name,
                GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name)

ha_box_score_data_2021_condensed <- ha_box_score_data_2021_condensed %>%
  mutate(season = "20/21")

ha_box_score_data_2021_condensed <- ha_box_score_data_2021_condensed %>%
  
  mutate(goal_player_team = paste(goal, gf_team, sep = "$"),
         primary_assist_player_team = paste(primary_assist, gf_team, sep = "$"),
         secondary_assist_player_team = paste(secondary_assist, gf_team, sep = "$"),
         
         GF1_player_team = paste(GF1_name, gf_team, sep = "$"),
         GF2_player_team = paste(GF2_name, gf_team, sep = "$"),
         GF3_player_team = paste(GF3_name, gf_team, sep = "$"),
         GF4_player_team = paste(GF4_name, gf_team, sep = "$"),
         GF5_player_team = paste(GF5_name, gf_team, sep = "$"),
         GF6_player_team = paste(GF6_name, gf_team, sep = "$"),
         
         GA1_player_team = paste(GA1_name, ga_team, sep = "$"),
         GA2_player_team = paste(GA2_name, ga_team, sep = "$"),
         GA3_player_team = paste(GA3_name, ga_team, sep = "$"),
         GA4_player_team = paste(GA4_name, ga_team, sep = "$"),
         GA5_player_team = paste(GA5_name, ga_team, sep = "$"),
         GA6_player_team = paste(GA6_name, ga_team, sep = "$")
  )

#ha_box_score_data_all <- rbind(box_score_data_all, box_score_data_2021_condensed)

ha_box_score_data_all <- ha_box_score_data_2021_condensed

write_excel_csv(ha_box_score_data_all, "ha_box_score_data_all.csv")


#ha_box_score_data_2021_raw$goal[ha_box_score_data_2021_raw$goal == 'Adam Falk'] <- 'Gustav Willman Borvik'

# HA - Lineup Data --------------------------------------------------------

ha_lineup_data_2021_update <- c()
for(id in ha_gameID_table$id[39:42]) {
  
  temp <- ha_get_lineup(id)
  
  ha_lineup_data_2021_update <- rbind(ha_lineup_data_2021_update, temp)
  
  print(id)
  rm(temp)
  
}

#manual edits will be needed since the get_lineup function is unreliable, by design
write_excel_csv(ha_lineup_data_2021_update, "ha_lineup_data_2021_update.csv")

ha_lineup_data_2021_update <- read.csv("ha_lineup_data_2021_update.csv",  stringsAsFactors = FALSE)

#join updated lineup data with existing base
#this will break the first time it's run
ha_lineup_data_2021 <- rbind(ha_lineup_data_2021, ha_lineup_data_2021_update)

# HA - Game Logs (EP API) -------------------------------------------------
ha_player_game_logs_2021 <- c()
for(id in ha_players$ep_id[1:length(ha_players$ep_id)]) {
  
  path <- paste0("/v1/game-logs?limit=1000&sort=-game.dateTime&player=", id, "&game.league=hockeyallsvenskan&game.season=2020-2021&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H")
  
  response <- ep_api(path)
  
  jsonRespText <- content(response, as = "text", encoding = "UTF-8") 
  df <- fromJSON(jsonRespText)
  tibble <- as_tibble(df$data)
  
  temp <- tibble(playerid = tibble$player$id,
                 player = tibble$player$name,
                 date = tibble$game$date,
                 gameid = tibble$game$id,
                 season = tibble$game$season$slug,
                 team = tibble$team$name,
                 opponent = tibble$opponent$name,
                 toi = tibble$stats$TOI,
                 goals = tibble$stats$G,
                 assists = tibble$stats$A,
                 pts = tibble$stats$PTS,
                 sog = tibble$stats$SOG)
  
  # temp <- temp %>%
  #   mutate(team_date = paste0(team, date)) %>%
  #   
  #   separate(toi, sep = ":", into = c("Minutes", "Seconds")) %>%
  #   mutate_at(c("Minutes", "Seconds"), as.numeric) %>%
  #   
  #   mutate(toi = round((Minutes * 60 + Seconds)/60,1)) %>%
  #   
  #   select(-c("Minutes", "Seconds"))
  
  ha_player_game_logs_2021 <- rbind(ha_player_game_logs_2021, temp)
  
  rm(temp)
  
}


#clean up API data
ha_player_game_logs_2021 <- ha_player_game_logs_2021 %>%
  left_join(ha_team_dictionary, by = c("team" = "ep_team_name")) %>%

  mutate(team_date = paste0(swehockey_team_name, date)) %>%
  
  separate(toi, sep = ":", into = c("Minutes", "Seconds")) %>%
  mutate_at(c("Minutes", "Seconds"), as.numeric) %>%
  mutate(toi = round((Minutes * 60 + Seconds)/60,1)) %>%
  
  left_join(ha_master_team_schedule_2021, by = c("team_date" = "team_date")) %>%
  
  dplyr::select(playerid, player, date.x, gameid, season, team.x, opponent,
                toi, goals, assists, pts, sog, team_date, game_number) %>%
  
  dplyr::rename(date = date.x, team = team.x) %>%
  
  left_join(ha_player_lookup_2021, by = c("player" = "ep")) %>%
  left_join(ha_team_dictionary, by = c("team" = "ep_team_name")) %>%
  dplyr::mutate(player_team = paste0(swehockey, "$", swehockey_team_name))

#left join in swehockey name
#ha_player_game_logs_2021 <- left_join(ha_player_game_logs_2021, ha_player_lookup_2021, by = c("player" = "ep"))

#left join in 

#Export to CSVs for Shiny file
write_excel_csv(ha_player_game_logs_2021, "ha_player_game_logs_2021.csv")

ha_bad_games <- c("25876", "26516")

# HA - Player Stats -------------------------------------------------------

ha_player_team_IDs <- unique(ha_player_db$player_team) %>%
  sort()

ha_players_2021_new <- tibble(player = ha_player_team_IDs)

write_excel_csv(ha_players_2021_new, "ha_players_2021_new.csv")

ha_players_2021_new <- read.csv("ha_players_2021_new.csv", stringsAsFactors = FALSE)

# ha_players_2021_new[!grepl("NA$AIK", ha_players_2021_new$player),] -> ha_players_2021_new
# 
# df[!grepl("REVERSE", df$Name),]

ha_get_5v5_data <- function(player) {

  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '5v5', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'EV', manpower = '5v5', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]

  return(data)
  
}

ha_get_ev_data <- function(player) {
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = 'All EV', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'EV', manpower = 'All EV', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]
  
  return(data)
  
}

ha_get_pp_data <- function(player) {
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = 'All PP', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'PP', manpower = 'All PP', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]
  
  return(data)
  
}

ha_get_sh_data <- function(player) {
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'SH', manpower = 'All SH', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'SH', manpower = 'All SH', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]
  
  return(data)
  
}

ha_get_5v4_data <- function(player) {
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v4', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'PP', manpower = '5v4', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]
  
  return(data)
  
}

ha_get_5v3_data <- function(player) {
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v3', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'PP', manpower = '5v3', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]
  
  return(data)
  
}

ha_get_4v4_data <- function(player) {
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '4v4', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'EV', manpower = '4v4', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]
  
  return(data)
  
}

ha_get_3v3_data <- function(player) {
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(game_situation_general != 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '3v3', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'EV', manpower = '3v3', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]
  
  return(data)
  
}

ha_get_eng_data <- function(player) {
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'ENG', manpower = 'ENG', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'ENG', manpower = 'ENG', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]
  
  return(data)
  
}

ha_get_allsits_data <- function(player) {
  
  SOG <- ha_player_game_logs_2021 %>%
    filter(player_team == 'Marcus Sörensen$HC Vita Hästen') %>%
    #filter(player_team == player) %>%
    summarise(SOG = sum(sog))
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  A1 <- ha_box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  A2 <- ha_box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- ha_box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF2 <- ha_box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF3 <- ha_box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF4 <- ha_box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF5 <- ha_box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GF6 <- ha_box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- ha_box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA2 <- ha_box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA3 <- ha_box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA4 <- ha_box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA5 <- ha_box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  GA6 <- ha_box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(season == '20/21') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)

  #`Sh%` <- round((G / SOG) * 100, 1)
  
  data <- tibble(player = player, situation = 'All', manpower = 'All', G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'All', manpower = 'All', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")]
  
  # data <- data %>%
  #   .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "SOG", "Sh%", "GF", "GA", "GF%",
  #        "IPP")]
  
  return(data)
  #return(SOG)
  #return(sum(game_log$sog))
  
}

ha_get_1v0_data <- function(player) {
  
  G <- ha_box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '1v0') %>%
    filter(season == '20/21') %>%
    nrow()
  
  data <- tibble(player = player, situation = 'PS/SO', manpower = '1v0', G = G)
  
  player_friendly <- str_split(data$player, "[$]")[[1]][1]
  
  GP <- ha_lineup_data_2021 %>%
    filter(player == player_friendly) %>%
    nrow()
  
  data <- tibble(player = player_friendly, situation = 'PS/SO', manpower = '1v0', GP = GP, G = G)
  
  data <- left_join(data, ha_player_db, by = c("player" = "swehockey")) %>%
    mutate(Season = "20/21")
  
  data <- left_join(data, ha_player_lookup_2021_v2, by = c("player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    dplyr::rename(Player = player,
                  Situation = situation,
                  Manpower = manpower,
                  Team = team_name,
                  Pos = position)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower","GP", "G", "NHL Rights")]
  
  return(data)
  
}


ha_player_5v5_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_5v5_data(player)
  
  ha_player_5v5_data_2021 <- rbind(ha_player_5v5_data_2021, temp)
  
  print(player)
  
}

ha_player_ev_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_ev_data(player)
  
  ha_player_ev_data_2021 <- rbind(ha_player_ev_data_2021, temp)
  
  print(player)
  
}

ha_player_pp_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_pp_data(player)
  
  ha_player_pp_data_2021 <- rbind(ha_player_pp_data_2021, temp)
  
  print(player)
  
}

ha_player_sh_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_sh_data(player)
  
  ha_player_sh_data_2021 <- rbind(ha_player_sh_data_2021, temp)
  
  print(player)
  
}

ha_player_5v4_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_5v4_data(player)
  
  ha_player_5v4_data_2021 <- rbind(ha_player_5v4_data_2021, temp)
  
  print(player)
  
}

ha_player_5v3_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_5v3_data(player)
  
  ha_player_5v3_data_2021 <- rbind(ha_player_5v3_data_2021, temp)
  
  print(player)
  
}

ha_player_4v4_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_4v4_data(player)
  
  ha_player_4v4_data_2021 <- rbind(ha_player_4v4_data_2021, temp)
  
  print(player)
  
}

ha_player_3v3_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_3v3_data(player)
  
  ha_player_3v3_data_2021 <- rbind(ha_player_3v3_data_2021, temp)
  
  print(player)
  
}

ha_player_eng_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_eng_data(player)
  
  ha_player_eng_data_2021 <- rbind(ha_player_eng_data_2021, temp)
  
  print(player)
  
}

ha_player_allsits_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_allsits_data(player)
  
  ha_player_allsits_data_2021 <- rbind(ha_player_allsits_data_2021, temp)
  
  print(player)
  
}

ha_player_1v0_data_2021 <- c()
for(player in ha_players_2021_new$player[1:length(ha_players_2021_new$player)]) {
  
  temp <- ha_get_1v0_data(player)
  
  ha_player_1v0_data_2021 <- rbind(ha_player_1v0_data_2021, temp)
  
  print(player)
  
}

#Remove the Orebro N/A that doesn't make any sense
ha_player_5v5_data_2021 <- ha_player_5v5_data_2021 %>% filter(Player != 'NA')
ha_player_ev_data_2021 <- ha_player_ev_data_2021 %>% filter(Player != 'NA')
ha_player_pp_data_2021 <- ha_player_pp_data_2021 %>% filter(Player != 'NA')
ha_player_5v4_data_2021 <- ha_player_5v4_data_2021 %>% filter(Player != 'NA')
ha_player_5v3_data_2021 <- ha_player_5v3_data_2021 %>% filter(Player != 'NA')
ha_player_sh_data_2021 <- ha_player_sh_data_2021 %>% filter(Player != 'NA')
ha_player_3v3_data_2021 <- ha_player_3v3_data_2021 %>% filter(Player != 'NA')
ha_player_4v4_data_2021 <- ha_player_4v4_data_2021 %>% filter(Player != 'NA')
ha_player_1v0_data_2021 <- ha_player_1v0_data_2021 %>% filter(Player != 'NA')
ha_player_allsits_data_2021 <- ha_player_allsits_data_2021 %>% filter(Player != 'NA')
ha_player_eng_data_2021 <- ha_player_eng_data_2021 %>% filter(Player != 'NA')

#Make N/As equal 0
ha_player_5v5_data_2021[is.na(ha_player_5v5_data_2021)] <- 0
ha_player_ev_data_2021[is.na(ha_player_ev_data_2021)] <- 0
ha_player_pp_data_2021[is.na(ha_player_pp_data_2021)] <- 0
ha_player_5v4_data_2021[is.na(ha_player_5v4_data_2021)] <- 0
ha_player_5v3_data_2021[is.na(ha_player_5v3_data_2021)] <- 0
ha_player_sh_data_2021[is.na(ha_player_sh_data_2021)] <- 0
ha_player_3v3_data_2021[is.na(ha_player_3v3_data_2021)] <- 0
ha_player_4v4_data_2021[is.na(ha_player_4v4_data_2021)] <- 0
ha_player_1v0_data_2021[is.na(ha_player_1v0_data_2021)] <- 0
ha_player_allsits_data_2021[is.na(ha_player_allsits_data_2021)] <- 0
ha_player_eng_data_2021[is.na(ha_player_eng_data_2021)] <- 0

# #get rid of 20/21 stats in the master frame, so I can then just bind it - prob figure out better way (update frames?)
# player_5v5_data_master <- player_5v5_data_master %>% filter(Season != "20/21")
# player_es_data_master <- player_es_data_master %>% filter(Season != "20/21")
# player_pp_data_master <- player_pp_data_master %>% filter(Season != "20/21")
# player_5v4_data_master <- player_5v4_data_master %>% filter(Season != "20/21")
# player_5v3_data_master <- player_5v3_data_master %>% filter(Season != "20/21")
# player_sh_data_master <- player_sh_data_master %>% filter(Season != "20/21")
# player_3v3_data_master <- player_3v3_data_master %>% filter(Season != "20/21")
# player_4v4_data_master <- player_4v4_data_master %>% filter(Season != "20/21")
# player_1v0_data_master <- player_1v0_data_master %>% filter(Season != "20/21")
# player_allsits_data_master <- player_allsits_data_master %>% filter(Season != "20/21")
# player_eng_data_master <- player_eng_data_master %>% filter(Season != "20/21")
# 
# #Join in with master data frames
# #come back to this the next time - can't keep binding new shit
# player_5v5_data_master <- rbind(player_5v5_data_master, player_5v5_data_2021)
# player_es_data_master <- rbind(player_es_data_master, player_es_data_2021)
# player_pp_data_master <- rbind(player_pp_data_master, player_pp_data_2021)
# player_5v4_data_master <- rbind(player_5v4_data_master, player_5v4_data_2021) 
# player_5v3_data_master <- rbind(player_5v3_data_master, player_5v3_data_2021)
# player_sh_data_master <- rbind(player_sh_data_master, player_sh_data_2021)
# player_3v3_data_master <- rbind(player_3v3_data_master, player_3v3_data_2021)
# player_4v4_data_master <- rbind(player_4v4_data_master, player_4v4_data_2021)
# player_1v0_data_master <- rbind(player_1v0_data_master, player_1v0_data_2021)
# player_allsits_data_master <- rbind(player_allsits_data_master, player_allsits_data_2021)
# player_eng_data_master <- rbind(player_eng_data_master, player_eng_data_2021)

#Export to CSVs for Shiny file
write_excel_csv(ha_player_5v5_data_2021, "ha_player_5v5_data_2021.csv")
write_excel_csv(ha_player_ev_data_2021, "ha_player_ev_data_2021.csv")
write_excel_csv(ha_player_pp_data_2021, "ha_player_pp_data_2021.csv")
write_excel_csv(ha_player_5v4_data_2021, "ha_player_5v4_data_2021.csv")
write_excel_csv(ha_player_5v3_data_2021, "ha_player_5v3_data_2021.csv")
write_excel_csv(ha_player_sh_data_2021, "ha_player_sh_data_2021.csv")
write_excel_csv(ha_player_3v3_data_2021, "ha_player_3v3_data_2021.csv")
write_excel_csv(ha_player_4v4_data_2021, "ha_player_4v4_data_2021.csv")
write_excel_csv(ha_player_1v0_data_2021, "ha_player_1v0_data_2021.csv")
write_excel_csv(ha_player_allsits_data_2021, "ha_player_allsits_data_2021.csv")
write_excel_csv(ha_player_eng_data_2021, "ha_player_eng_data_2021.csv")

# HA - Run Once Per Season ------------------------------------------------

ha_schedule_2021 <- read.csv("ha_schedule_2021.csv", stringsAsFactors = FALSE)

#add a game number
ha_schedule_2021 <- ha_schedule_2021 %>% 
  dplyr::mutate(game_number = row_number())

#join in other versions of team names
ha_schedule_2021 <- merge(ha_schedule_2021, ha_team_dictionary, by.x = "home_team", by.y = "swehockey_team_name")

ha_schedule_2021 <- ha_schedule_2021 %>%
  dplyr::rename(home_team_name = team_name, home_team_swehockey_name = home_team)

ha_schedule_2021 <- merge(ha_schedule_2021, ha_team_dictionary, by.x = "away_team", by.y = "swehockey_team_name")

ha_schedule_2021 <- ha_schedule_2021 %>%
  dplyr::rename(away_team_name = team_name, away_team_swehockey_name = away_team)

#reorder columns
ha_schedule_2021 <- ha_schedule_2021[c("game_number", "date", "home_team_swehockey_name", "away_team_swehockey_name", "combined_teams", 
                                       "home_team_name","away_team_name")]

#sort by game number
ha_schedule_2021 <- ha_schedule_2021[order(ha_schedule_2021$game_number),]

#individual team schedules
#AIK
aik_schedule_2021 <- subset(ha_schedule_2021, grepl("AIK", ha_schedule_2021$combined_teams))

aik_schedule_2021 <- aik_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "AIK",
                team_date = paste0(team,date))

#Almtuna
ais_schedule_2021 <- subset(ha_schedule_2021, grepl("Almtuna IS", ha_schedule_2021$combined_teams))

ais_schedule_2021 <- ais_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "Almtuna IS",
                team_date = paste0(team,date))

#Karlskoga
bik_schedule_2021 <- subset(ha_schedule_2021, grepl("BIK Karlskoga", ha_schedule_2021$combined_teams))

bik_schedule_2021 <- bik_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "BIK Karlskoga",
                team_date = paste0(team,date))

#Vita Hästen
vit_schedule_2021 <- subset(ha_schedule_2021, grepl("HC Vita Hästen", ha_schedule_2021$combined_teams))

vit_schedule_2021 <- vit_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "HC Vita Hästen",
                team_date = paste0(team,date))

#Björklöven	
ifb_schedule_2021 <- subset(ha_schedule_2021, grepl("IF Björklöven", ha_schedule_2021$combined_teams))

ifb_schedule_2021 <- ifb_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "IF Björklöven",
                team_date = paste0(team,date))

#Kristianstad	
kik_schedule_2021 <- subset(ha_schedule_2021, grepl("Kristianstads IK", ha_schedule_2021$combined_teams))

kik_schedule_2021 <- kik_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "Kristianstads IK",
                team_date = paste0(team,date))

#MODO
modo_schedule_2021 <- subset(ha_schedule_2021, grepl("MODO Hockey", ha_schedule_2021$combined_teams))

modo_schedule_2021 <- modo_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "MODO Hockey",
                team_date = paste0(team,date))

#Mora
mik_schedule_2021 <- subset(ha_schedule_2021, grepl("Mora IK", ha_schedule_2021$combined_teams))

mik_schedule_2021 <- mik_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "Mora IK",
                team_date = paste0(team,date))

#Södertälje
ssk_schedule_2021 <- subset(ha_schedule_2021, grepl("Södertälje SK", ha_schedule_2021$combined_teams))

ssk_schedule_2021 <- ssk_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "Södertälje SK",
                team_date = paste0(team,date))

#Timrå IK
tik_schedule_2021 <- subset(ha_schedule_2021, grepl("Timrå IK", ha_schedule_2021$combined_teams))

tik_schedule_2021 <- tik_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "Timrå IK",
                team_date = paste0(team,date))

#Tingsryd
taif_schedule_2021 <- subset(ha_schedule_2021, grepl("Tingsryds AIF", ha_schedule_2021$combined_teams))

taif_schedule_2021 <- taif_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "Tingsryds AIF",
                team_date = paste0(team,date))

#Väsby
vikhk_schedule_2021 <- subset(ha_schedule_2021, grepl("Väsby IK HK", ha_schedule_2021$combined_teams))

vikhk_schedule_2021 <- vikhk_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "Väsby IK HK",
                team_date = paste0(team,date))

#Västervik
vaik_schedule_2021 <- subset(ha_schedule_2021, grepl("Västerviks IK", ha_schedule_2021$combined_teams))

vaik_schedule_2021 <- vaik_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "Västerviks IK",
                team_date = paste0(team,date))

#Västerås
vik_schedule_2021 <- subset(ha_schedule_2021, grepl("Västerås IK", ha_schedule_2021$combined_teams))

vik_schedule_2021 <- vik_schedule_2021[,c(2:7)] %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                team = "Västerås IK",
                team_date = paste0(team,date))
  
ha_master_team_schedule_2021 <- rbind(aik_schedule_2021, ais_schedule_2021, bik_schedule_2021, vit_schedule_2021,
                                      ifb_schedule_2021, kik_schedule_2021, modo_schedule_2021, mik_schedule_2021,
                                      ssk_schedule_2021, tik_schedule_2021, taif_schedule_2021, vikhk_schedule_2021,
                                      vaik_schedule_2021, vik_schedule_2021)  





#https://api.eliteprospects.com/v1/game-logs?limit=1000&sort=-game.dateTime&player=20065,21860&game.league=hockeyallsvenskan&game.season=2020-2021&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H


game_log_test <- c()
for(id in ha_players$ep_id[1:length(ha_players$ep_id)]) {
  
  path <- paste0("/v1/game-logs?limit=1000&sort=-game.dateTime&player=", id, "&game.league=hockeyallsvenskan&game.season=2020-2021&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H")
                 
  response <- ep_api(path)
  
  jsonRespText <- content(response, as = "text", encoding = "UTF-8") 
  df <- fromJSON(jsonRespText)
  tibble <- as_tibble(df$data)
  
  temp <- tibble(playerid = tibble$player$id,
                 player = tibble$player$name,
                 date = tibble$game$date,
                 gameid = tibble$game$id,
                 season = tibble$game$season$slug,
                 team = tibble$team$name,
                 opponent = tibble$opponent$name,
                 toi = tibble$stats$TOI,
                 goals = tibble$stats$G,
                 assists = tibble$stats$A,
                 pts = tibble$stats$PTS,
                 sog = tibble$stats$SOG)
  
  game_log_test <- rbind(game_log_test, temp)
  
  rm(temp)
  
}

tibble(playerid = tibble$player$id,
       player = tibble$player$name,
       date = tibble$game$date,
       gameid = tibble$game$id,
       season = tibble$game$season$slug,
       team = tibble$team$name,
       opponent = tibble$opponent$name,
       toi = tibble$stats$TOI,
       goals = tibble$stats$G,
       assists = tibble$stats$A,
       pts = tibble$stats$PTS,
       sog = tibble$stats$SOG)


#https://api.eliteprospects.com/v1/leagues/shl/player-stats?limit=100&sort=-regularStats.PTS&season=2020-2021&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H

tpath <- "/v1/leagues/shl/player-stats?limit=1000&sort=-regularStats.PTS&season=2020-2021&apiKey=w03WSxFbpci1hUTWb9sJB4sHcA9Hby4H"

tresponse <- ep_api(tpath)

tjsonRespText <- content(tresponse, as = "text", encoding = "UTF-8") 
tdf <- fromJSON(tjsonRespText)
ttibble <- as_tibble(tdf$data)

ttibble$player$dateOfBirth[1]

lubridate::as_date(ttibble$player$dateOfBirth[1])

lubridate::as_date("2020-10-12") - lubridate::as_date(ttibble$player$dateOfBirth[1])

# Past Season Stats Updates -----------------------------------------------

#19/20

get_5v5_data <- function(player) {
  
  GP <- player_toi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  
  fenwick_data <- player_corsi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '19/20') %>%
    dplyr::select(FF, FA, CF, CA)
  
  CF <- sum(fenwick_data$CF)
  CA <- sum(fenwick_data$CA)
  
  `CF%` <- (round(CF / (CF + CA),3)) * 100
  #`CF%` <- (signif(CF / (CF + CA),3)) * 100
  
  FF <- sum(fenwick_data$FF)
  FA <- sum(fenwick_data$FA)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  FenSh <- round((GF/FF) * 100, 1)
  FenSv <- 100 - round((GA/FA) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '5v5', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP, CF = CF, CA = CA, `CF%` = `CF%`, FenSh = FenSh, FenSv = FenSv)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  #data <- left_join(data, player_toi_data_1920, by = c("player" = "player_team")) %>% unique()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "CF", "CA", "CF%", "FenSh", "FenSv", "NHL Rights")] %>% dplyr::distinct()
  
  #data <- data %>% unique()
  
  # data <- data %>%
  #   .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
  #        "IPP", "CF", "CA", "CF%", "FenSh", "FenSv", "nhl_team")] %>% unique()
  
  return(data)
}

player_5v5_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_5v5_data(player)
  
  player_5v5_data_1920 <- rbind(player_5v5_data_1920, temp)
  
  print(player)
  
}

get_ev_data <- function(player) {
  
  GP <- player_toi_data_1920_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1920_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$ESTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = 'All EV', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_es_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_ev_data(player)
  
  player_es_data_1920 <- rbind(player_es_data_1920, temp)
  
  print(player)
  
}

get_pp_data <- function(player) {
  
  GP <- player_toi_data_1920_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1920_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$PPTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2)
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = 'All PP', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_pp_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_pp_data(player)
  
  player_pp_data_1920 <- rbind(player_pp_data_1920, temp)
  
  print(player)
  
}

get_5v4_data <- function(player) {
  
  GP <- player_toi_data_1920_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v4', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_5v4_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_5v4_data(player)
  
  player_5v4_data_1920 <- rbind(player_5v4_data_1920, temp)
  
  print(player)
  
}

get_5v3_data <- function(player) {
  
  GP <- player_toi_data_1920_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v3', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_5v3_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_5v3_data(player)
  
  player_5v3_data_1920 <- rbind(player_5v3_data_1920, temp)
  
  print(player)
  
}

get_sh_data <- function(player) {
  
  GP <- player_toi_data_1920_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1920_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$SHTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2)
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'SH', manpower = 'All SH', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60,  IPP = IPP)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_sh_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_sh_data(player)
  
  player_sh_data_1920 <- rbind(player_sh_data_1920, temp)
  
  print(player)
  
}

get_3v3_data <- function(player) {
  
  GP <- player_toi_data_1920_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '3v3', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_3v3_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_3v3_data(player)
  
  player_3v3_data_1920 <- rbind(player_3v3_data_1920, temp)
  
  print(player)
  
}

get_4v4_data <- function(player) {
  
  GP <- player_toi_data_1920_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '4v4', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_4v4_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_4v4_data(player)
  
  player_4v4_data_1920 <- rbind(player_4v4_data_1920, temp)
  
  print(player)
  
}

get_1v0_data <- function(player) {
  
  GP <- player_toi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '1v0') %>%
    filter(season == '19/20') %>%
    nrow()
  
  data <- tibble(player = player, situation = 'PS/SO', manpower = '1v0', G = G, GP = GP)
  #data <- tibble(player = player, situation = 'PS/SO', manpower = '1v0', G = G)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  #data <- data %>%
  #.[,c("Player", "Season", "Team", "Pos", "GP", "G")]
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos","GP", "G", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
  
}

player_1v0_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_1v0_data(player)
  
  player_1v0_data_1920 <- rbind(player_1v0_data_1920, temp)
  
  print(player)
  
}

get_allsits_data <- function(player) {
  
  GP <- player_toi_data_1920_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1920_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$TOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  
  data <- tibble(player = player, situation = 'All', manpower = 'All', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60, GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_allsits_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_allsits_data(player)
  
  player_allsits_data_1920 <- rbind(player_allsits_data_1920, temp)
  
  print(player)
  
}

get_eng_data <- function(player) {
  
  GP <- player_toi_data_1920_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '19/20') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '19/20') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '19/20') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '19/20') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '19/20') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '19/20') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '19/20') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '19/20') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '19/20') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '19/20') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '19/20') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '19/20') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'ENG', manpower = 'ENG', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1920_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '19/20')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1920_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_eng_data_1920 <- c()
for(player in players_1920_new$player[1:length(players_1920_new$player)]) {
  
  temp <- get_eng_data(player)
  
  player_eng_data_1920 <- rbind(player_eng_data_1920, temp)
  
  print(player)
  
}

#Remove the Orebro N/A that doesn't make any sense
player_5v5_data_1920 <- player_5v5_data_1920 %>% filter(Player != 'NA')
player_es_data_1920 <- player_es_data_1920 %>% filter(Player != 'NA')
player_pp_data_1920 <- player_pp_data_1920 %>% filter(Player != 'NA')
player_5v4_data_1920 <- player_5v4_data_1920 %>% filter(Player != 'NA')
player_5v3_data_1920 <- player_5v3_data_1920 %>% filter(Player != 'NA')
player_sh_data_1920 <- player_sh_data_1920 %>% filter(Player != 'NA')
player_3v3_data_1920 <- player_3v3_data_1920 %>% filter(Player != 'NA')
player_4v4_data_1920 <- player_4v4_data_1920 %>% filter(Player != 'NA')
player_1v0_data_1920 <- player_1v0_data_1920 %>% filter(Player != 'NA')
player_allsits_data_1920 <- player_allsits_data_1920 %>% filter(Player != 'NA')
player_eng_data_1920 <- player_eng_data_1920 %>% filter(Player != 'NA')

#Make N/As equal 0
player_5v5_data_1920[is.na(player_5v5_data_1920)] <- 0
player_es_data_1920[is.na(player_es_data_1920)] <- 0
player_pp_data_1920[is.na(player_pp_data_1920)] <- 0
player_5v4_data_1920[is.na(player_5v4_data_1920)] <- 0
player_5v3_data_1920[is.na(player_5v3_data_1920)] <- 0
player_sh_data_1920[is.na(player_sh_data_1920)] <- 0
player_3v3_data_1920[is.na(player_3v3_data_1920)] <- 0
player_4v4_data_1920[is.na(player_4v4_data_1920)] <- 0
player_1v0_data_1920[is.na(player_1v0_data_1920)] <- 0
player_allsits_data_1920[is.na(player_allsits_data_1920)] <- 0
player_eng_data_1920[is.na(player_eng_data_1920)] <- 0

#18/19

player_toi_data_1819_v2 <- player_toi_data_master_v3 %>%
  filter(season == '18/19')

get_5v5_data <- function(player) {
  
  GP <- player_toi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  
  fenwick_data <- player_corsi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '18/19') %>%
    dplyr::select(FF, FA, CF, CA)
  
  CF <- sum(fenwick_data$CF)
  CA <- sum(fenwick_data$CA)
  
  `CF%` <- (round(CF / (CF + CA),3)) * 100
  #`CF%` <- (signif(CF / (CF + CA),3)) * 100
  
  FF <- sum(fenwick_data$FF)
  FA <- sum(fenwick_data$FA)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  FenSh <- round((GF/FF) * 100, 1)
  FenSv <- 100 - round((GA/FA) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '5v5', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP, CF = CF, CA = CA, `CF%` = `CF%`, FenSh = FenSh, FenSv = FenSv)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  #data <- left_join(data, player_toi_data_1819, by = c("player" = "player_team")) %>% unique()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "CF", "CA", "CF%", "FenSh", "FenSv", "NHL Rights")] %>% dplyr::distinct()
  
  #data <- data %>% unique()
  
  # data <- data %>%
  #   .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
  #        "IPP", "CF", "CA", "CF%", "FenSh", "FenSv", "nhl_team")] %>% unique()
  
  return(data)
}

player_5v5_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_5v5_data(player)
  
  player_5v5_data_1819 <- rbind(player_5v5_data_1819, temp)
  
  print(player)
  
}

get_ev_data <- function(player) {
  
  GP <- player_toi_data_1819_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1819_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$ESTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = 'All EV', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_es_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_ev_data(player)
  
  player_es_data_1819 <- rbind(player_es_data_1819, temp)
  
  print(player)
  
}

get_pp_data <- function(player) {
  
  GP <- player_toi_data_1819_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1819_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$PPTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2)
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = 'All PP', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_pp_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_pp_data(player)
  
  player_pp_data_1819 <- rbind(player_pp_data_1819, temp)
  
  print(player)
  
}

get_5v4_data <- function(player) {
  
  GP <- player_toi_data_1819_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v4', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_5v4_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_5v4_data(player)
  
  player_5v4_data_1819 <- rbind(player_5v4_data_1819, temp)
  
  print(player)
  
}

get_5v3_data <- function(player) {
  
  GP <- player_toi_data_1819_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v3', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_5v3_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_5v3_data(player)
  
  player_5v3_data_1819 <- rbind(player_5v3_data_1819, temp)
  
  print(player)
  
}

get_sh_data <- function(player) {
  
  GP <- player_toi_data_1819_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1819_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$SHTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2)
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'SH', manpower = 'All SH', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60,  IPP = IPP)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_sh_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_sh_data(player)
  
  player_sh_data_1819 <- rbind(player_sh_data_1819, temp)
  
  print(player)
  
}

get_3v3_data <- function(player) {
  
  GP <- player_toi_data_1819_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '3v3', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_3v3_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_3v3_data(player)
  
  player_3v3_data_1819 <- rbind(player_3v3_data_1819, temp)
  
  print(player)
  
}

get_4v4_data <- function(player) {
  
  GP <- player_toi_data_1819_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '4v4', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_4v4_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_4v4_data(player)
  
  player_4v4_data_1819 <- rbind(player_4v4_data_1819, temp)
  
  print(player)
  
}

get_1v0_data <- function(player) {
  
  GP <- player_toi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '1v0') %>%
    filter(season == '18/19') %>%
    nrow()
  
  data <- tibble(player = player, situation = 'PS/SO', manpower = '1v0', G = G, GP = GP)
  #data <- tibble(player = player, situation = 'PS/SO', manpower = '1v0', G = G)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  #data <- data %>%
  #.[,c("Player", "Season", "Team", "Pos", "GP", "G")]
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos","GP", "G", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
  
}

player_1v0_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_1v0_data(player)
  
  player_1v0_data_1819 <- rbind(player_1v0_data_1819, temp)
  
  print(player)
  
}

get_allsits_data <- function(player) {
  
  GP <- player_toi_data_1819_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1819_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$TOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  
  data <- tibble(player = player, situation = 'All', manpower = 'All', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60, GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_allsits_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_allsits_data(player)
  
  player_allsits_data_1819 <- rbind(player_allsits_data_1819, temp)
  
  print(player)
  
}

get_eng_data <- function(player) {
  
  GP <- player_toi_data_1819_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'ENG', manpower = 'ENG', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1819_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '18/19')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1819_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_eng_data_1819 <- c()
for(player in players_1819_new$player[1:length(players_1819_new$player)]) {
  
  temp <- get_eng_data(player)
  
  player_eng_data_1819 <- rbind(player_eng_data_1819, temp)
  
  print(player)
  
}

#Remove the Orebro N/A that doesn't make any sense
player_5v5_data_1819 <- player_5v5_data_1819 %>% filter(Player != 'NA')
player_es_data_1819 <- player_es_data_1819 %>% filter(Player != 'NA')
player_pp_data_1819 <- player_pp_data_1819 %>% filter(Player != 'NA')
player_5v4_data_1819 <- player_5v4_data_1819 %>% filter(Player != 'NA')
player_5v3_data_1819 <- player_5v3_data_1819 %>% filter(Player != 'NA')
player_sh_data_1819 <- player_sh_data_1819 %>% filter(Player != 'NA')
player_3v3_data_1819 <- player_3v3_data_1819 %>% filter(Player != 'NA')
player_4v4_data_1819 <- player_4v4_data_1819 %>% filter(Player != 'NA')
player_1v0_data_1819 <- player_1v0_data_1819 %>% filter(Player != 'NA')
player_allsits_data_1819 <- player_allsits_data_1819 %>% filter(Player != 'NA')
player_eng_data_1819 <- player_eng_data_1819 %>% filter(Player != 'NA')

#Make N/As equal 0
player_5v5_data_1819[is.na(player_5v5_data_1819)] <- 0
player_es_data_1819[is.na(player_es_data_1819)] <- 0
player_pp_data_1819[is.na(player_pp_data_1819)] <- 0
player_5v4_data_1819[is.na(player_5v4_data_1819)] <- 0
player_5v3_data_1819[is.na(player_5v3_data_1819)] <- 0
player_sh_data_1819[is.na(player_sh_data_1819)] <- 0
player_3v3_data_1819[is.na(player_3v3_data_1819)] <- 0
player_4v4_data_1819[is.na(player_4v4_data_1819)] <- 0
player_1v0_data_1819[is.na(player_1v0_data_1819)] <- 0
player_allsits_data_1819[is.na(player_allsits_data_1819)] <- 0
player_eng_data_1819[is.na(player_eng_data_1819)] <- 0

#17/18

player_toi_data_1718_v2 <- player_toi_data_master_v3 %>%
  filter(season == '17/18')

get_5v5_data <- function(player) {
  
  GP <- player_toi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  
  fenwick_data <- player_corsi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '17/18') %>%
    dplyr::select(FF, FA, CF, CA)
  
  CF <- sum(fenwick_data$CF)
  CA <- sum(fenwick_data$CA)
  
  `CF%` <- (round(CF / (CF + CA),3)) * 100
  #`CF%` <- (signif(CF / (CF + CA),3)) * 100
  
  FF <- sum(fenwick_data$FF)
  FA <- sum(fenwick_data$FA)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '5v5') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  FenSh <- round((GF/FF) * 100, 1)
  FenSv <- 100 - round((GA/FA) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '5v5', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP, CF = CF, CA = CA, `CF%` = `CF%`, FenSh = FenSh, FenSv = FenSv)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  #data <- left_join(data, player_toi_data_1718, by = c("player" = "player_team")) %>% unique()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "CF", "CA", "CF%", "FenSh", "FenSv", "NHL Rights")] %>% dplyr::distinct()
  
  #data <- data %>% unique()
  
  # data <- data %>%
  #   .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
  #        "IPP", "CF", "CA", "CF%", "FenSh", "FenSv", "nhl_team")] %>% unique()
  
  return(data)
}

player_5v5_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_5v5_data(player)
  
  player_5v5_data_1718 <- rbind(player_5v5_data_1718, temp)
  
  print(player)
  
}

get_ev_data <- function(player) {
  
  GP <- player_toi_data_1718_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1718_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$ESTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = 'All EV', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_es_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_ev_data(player)
  
  player_es_data_1718 <- rbind(player_es_data_1718, temp)
  
  print(player)
  
}

get_pp_data <- function(player) {
  
  GP <- player_toi_data_1718_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1718_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$PPTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2)
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = 'All PP', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_pp_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_pp_data(player)
  
  player_pp_data_1718 <- rbind(player_pp_data_1718, temp)
  
  print(player)
  
}

get_5v4_data <- function(player) {
  
  GP <- player_toi_data_1718_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v4') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v5') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v4', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_5v4_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_5v4_data(player)
  
  player_5v4_data_1718 <- rbind(player_5v4_data_1718, temp)
  
  print(player)
  
}

get_5v3_data <- function(player) {
  
  GP <- player_toi_data_1718_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '5v3') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v5') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'PP', manpower = '5v3', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_5v3_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_5v3_data(player)
  
  player_5v3_data_1718 <- rbind(player_5v3_data_1718, temp)
  
  print(player)
  
}

get_sh_data <- function(player) {
  
  GP <- player_toi_data_1718_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1718_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$SHTOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2)
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  data <- tibble(player = player, situation = 'SH', manpower = 'All SH', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60,  IPP = IPP)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_sh_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_sh_data(player)
  
  player_sh_data_1718 <- rbind(player_sh_data_1718, temp)
  
  print(player)
  
}

get_3v3_data <- function(player) {
  
  GP <- player_toi_data_1718_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '3v3') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '3v3', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_3v3_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_3v3_data(player)
  
  player_3v3_data_1718 <- rbind(player_3v3_data_1718, temp)
  
  print(player)
  
}

get_4v4_data <- function(player) {
  
  GP <- player_toi_data_1718_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation == '4v4') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '4v4', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_4v4_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_4v4_data(player)
  
  player_4v4_data_1718 <- rbind(player_4v4_data_1718, temp)
  
  print(player)
  
}

get_1v0_data <- function(player) {
  
  GP <- player_toi_data_master_v3 %>%
    filter(player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation == '1v0') %>%
    filter(season == '17/18') %>%
    nrow()
  
  data <- tibble(player = player, situation = 'PS/SO', manpower = '1v0', G = G, GP = GP)
  #data <- tibble(player = player, situation = 'PS/SO', manpower = '1v0', G = G)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  #data <- data %>%
  #.[,c("Player", "Season", "Team", "Pos", "GP", "G")]
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos","GP", "G", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
  
}

player_1v0_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_1v0_data(player)
  
  player_1v0_data_1718 <- rbind(player_1v0_data_1718, temp)
  
  print(player)
  
}

get_allsits_data <- function(player) {
  
  GP <- player_toi_data_1718_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_1718_v2 %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$TOI), 1)
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  GF60 <- round(GF/TOI * 60, 2)
  GA60 <- round(GA/TOI * 60, 2)
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  IPP <- round((P/GF) * 100, 1)
  
  `TOI/GP` <- round(TOI/GP, 1)
  
  
  data <- tibble(player = player, situation = 'All', manpower = 'All', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, P60 = P60, P160 = P160, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60, GA60, IPP = IPP)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "GF", "GA", "GF%",
         "GF60", "GA60", "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_allsits_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_allsits_data(player)
  
  player_allsits_data_1718 <- rbind(player_allsits_data_1718, temp)
  
  print(player)
  
}

get_eng_data <- function(player) {
  
  GP <- player_toi_data_1718_v2 %>%
    filter(player_team == player) %>%
    nrow()
  
  G <- box_score_data_all %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A1 <- box_score_data_all %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A2 <- box_score_data_all %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '17/18') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '17/18') %>%
    nrow()
  GF2 <- box_score_data_all %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '17/18') %>%
    nrow()
  GF3 <- box_score_data_all %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '17/18') %>%
    nrow()
  GF4 <- box_score_data_all %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '17/18') %>%
    nrow()
  GF5 <- box_score_data_all %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '17/18') %>%
    nrow()
  GF6 <- box_score_data_all %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'ENG') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '17/18') %>%
    nrow()
  GA2 <- box_score_data_all %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '17/18') %>%
    nrow()
  GA3 <- box_score_data_all %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '17/18') %>%
    nrow()
  GA4 <- box_score_data_all %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '17/18') %>%
    nrow()
  GA5 <- box_score_data_all %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '17/18') %>%
    nrow()
  GA6 <- box_score_data_all %>%
    filter(GA6_player_team == player) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    filter(season == '17/18') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  data <- tibble(player = player, situation = 'ENG', manpower = 'ENG', GP = GP, G = G, A = A, A1 = A1, A2 = A2, P = P, P1 = P1, GF = GF, GA = GA, `GF%` = `GF%`,
                 IPP = IPP)
  
  data <- merge(data, player_toi_data_1718_v2[,c("player_team", "shlse_team_name", "Pos")],
                by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  
  data <- data %>%
    dplyr::rename(Team = shlse_team_name,
                  Player = player,
                  Situation = situation,
                  Manpower = manpower) %>%
    
    mutate(Season = '17/18')
  
  data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  data <- left_join(data, player_lookup_1718_v2, by = c("Player" = "swehockey")) %>%
    dplyr::rename(`NHL Rights` = abbrev)
  
  data <- data %>%
    .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
         "IPP", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
}

player_eng_data_1718 <- c()
for(player in players_1718_new$player[1:length(players_1718_new$player)]) {
  
  temp <- get_eng_data(player)
  
  player_eng_data_1718 <- rbind(player_eng_data_1718, temp)
  
  print(player)
  
}

#Remove the Orebro N/A that doesn't make any sense
player_5v5_data_1718 <- player_5v5_data_1718 %>% filter(Player != 'NA')
player_es_data_1718 <- player_es_data_1718 %>% filter(Player != 'NA')
player_pp_data_1718 <- player_pp_data_1718 %>% filter(Player != 'NA')
player_5v4_data_1718 <- player_5v4_data_1718 %>% filter(Player != 'NA')
player_5v3_data_1718 <- player_5v3_data_1718 %>% filter(Player != 'NA')
player_sh_data_1718 <- player_sh_data_1718 %>% filter(Player != 'NA')
player_3v3_data_1718 <- player_3v3_data_1718 %>% filter(Player != 'NA')
player_4v4_data_1718 <- player_4v4_data_1718 %>% filter(Player != 'NA')
player_1v0_data_1718 <- player_1v0_data_1718 %>% filter(Player != 'NA')
player_allsits_data_1718 <- player_allsits_data_1718 %>% filter(Player != 'NA')
player_eng_data_1718 <- player_eng_data_1718 %>% filter(Player != 'NA')

#Make N/As equal 0
player_5v5_data_1718[is.na(player_5v5_data_1718)] <- 0
player_es_data_1718[is.na(player_es_data_1718)] <- 0
player_pp_data_1718[is.na(player_pp_data_1718)] <- 0
player_5v4_data_1718[is.na(player_5v4_data_1718)] <- 0
player_5v3_data_1718[is.na(player_5v3_data_1718)] <- 0
player_sh_data_1718[is.na(player_sh_data_1718)] <- 0
player_3v3_data_1718[is.na(player_3v3_data_1718)] <- 0
player_4v4_data_1718[is.na(player_4v4_data_1718)] <- 0
player_1v0_data_1718[is.na(player_1v0_data_1718)] <- 0
player_allsits_data_1718[is.na(player_allsits_data_1718)] <- 0
player_eng_data_1718[is.na(player_eng_data_1718)] <- 0

