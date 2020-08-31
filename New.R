
# Intro -------------------------------------------------------------------

#--------------------------
# Install required packages
#--------------------------

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


# Player Info -------------------------------------------------------------

#player reference data
player_lookup <- read.csv("player_lookup.csv", stringsAsFactors = FALSE)

player_lookup_1920 <- read.csv("player_lookup_1920.csv", stringsAsFactors = FALSE)

#update each refresh with net new players
player_lookup_1920_update <- tibble(swehockey = c("Leo Lööf", "Adam Arvedson"),
                                    shlse = c("Leo Lööf", "Adam Arvedson"))

player_lookup_1920 <- rbind(player_lookup_1920, player_lookup_1920_update) 

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
             "Claes Endre", "Anthony Peters", "Zackarias Skog", "Julius Pohjanoksa", "Kevin Poulin")


# Games & Schedule --------------------------------------------------------

#swehockey

#SHL - Box Scores

#all SHL box score URLs since the 2015/2016 season on
box_score_urls <- get_schedule("SHL", c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20"))

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


# In-Season Game & Schedule Refresh ---------------------------------------

#re-run functions to get most up-to-date full schedule
#(will have to update to most recent season)
get_1920_urls <- get_schedule("SHL", "2019-20")

get_1920_ids <- str_split(get_1920_urls$url, "/", simplify = TRUE)[,6]

get_1920_gameID_table <- tibble(id = get_1920_ids, 
                                season = get_1920_urls$season, 
                                league = "SHL")

#new URL bundle for current season
get_1920_box_score_urls <- tibble(url = paste("http://stats.swehockey.se/Game/Events/", get_1920_ids[1:length(get_1920_ids)], sep = ''),
                                  league = 'SHL', season = '2019-20')

get_1920_lineup_urls <- tibble(url = paste("http://stats.swehockey.se/Game/LineUps/", get_1920_ids[1:length(get_1920_ids)], sep = ''),
                               league = 'SHL', season = '2019-20')

#refresh the existing URL values with the updated info

box_score_urls_1920 <- rbind(box_score_urls_1920, get_1920_box_score_urls) %>%
  unique()

lineup_urls_1920 <- rbind(lineup_urls_1920, get_1920_lineup_urls) %>%
  unique()

gameIDs_1920 <- get_1920_ids

#table is now fully rebuilt
gameID_table_1920 <- tibble(id = gameIDs_1920, 
                            season = box_score_urls_1920$season, 
                            league = "SHL")


# Lineup Data -------------------------------------------------------------

#refresh lineup data for most recent batch of games
lineup_data_2021_update <- c()
for(id in gameID_table_2021$id[358:364]) {
  
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
      
      if (season == "2019-20") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/10371"}
      
      else if (season == "2018-19") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9171"}
      else if (season == "2017-18") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/8121"}
      else if (season == "2016-17") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/7132"}
      else if (season == "2015-16") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/6052"}
      else if (season == "2014-15") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/5056"}
      else if (season == "2013-14") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/3905"}
      else if (season == "2012-13") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/2892"}
      else if (season == "2011-12") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/965"}
      else if (season == "2010-11") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/258"}
      else if (season == "2009-10") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9024"}
      else if (season == "2008-09") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9052"}
      else if (season == "2007-08") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9077"}
      else if (season == "2006-07") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9143"}
      else if (season == "2005-06") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9146"}
      else if (season == "2004-05") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9149"}
      else if (season == "2003-04") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9152"}
      
      else {stop("Season not available. Sorry!")}
      
    }
    
    else if (league == "Allsvenskan") {
      
      if (season == "2019-20") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/10333"}
      
      else if (season == "2018-19") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9168"}
      else if (season == "2017-18") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/8122"}
      else if (season == "2016-17") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/7157"}
      else if (season == "2015-16") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/6053"}
      else if (season == "2014-15") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/5057"}
      else if (season == "2013-14") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/3906"}
      else if (season == "2012-13") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/3005"}
      else if (season == "2011-12") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/966"}
      else if (season == "2010-11") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/259"}
      else if (season == "2009-10") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9301"}
      else if (season == "2008-09") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9305"}
      
      else {stop("Season not available. Sorry!")}
      
    }  
    
    else if (league == "SuperElit") {
      
      if (season == "2018-19") {
        
        url <- c(
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/9169",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/9170")
        
      }
      
      else if (season == "2017-18") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/8702",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/8701",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/8124",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/8125")
        
      }
      
      else if (season == "2016-17") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/7155",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/7154",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/7152",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/7153")
        
      }
      
      else if (season == "2015-16") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/6695",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/6694",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/6054",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/6055")
        
      }
      
      else if (season == "2014-15") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/5784",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/5783",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/5024",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/5025")
        
      }
      
      else if (season == "2013-14") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/4656",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/4655",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3940",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3941")
        
      }
      
      else if (season == "2012-13") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3630",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3629",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3002",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3003")
        
      }
      
      else if (season == "2011-12") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/1651",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/1650",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/960",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/961")
        
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
        str_c("http://stats.swehockey.se", .) %>%
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
  
  #url <- read_html(paste('http://stats.swehockey.se/Game/LineUps/', id, sep=''))
  url <- read_html(GET(paste('https://stats.swehockey.se/Game/LineUps/', id, sep='')))
  
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
    tibble::enframe(name = NULL) #%>%
    
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
    left_join(master_team_schedule_1920, by = "team_date") %>%
    
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  #return(table)
  
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
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'G', 'GA', 'Opp', 'SOG'), as.numeric) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1920, by = "team_date") %>%
    
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
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
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'G', 'GA', 'Opp', 'SOGA'), as.numeric) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1920, by = "team_date") %>%
    
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
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
    mutate(date1) %>%
    
    #make columns numeric
    mutate_at(c("GP", "G", "SOG", "G_PP", "SOG_PP", "MS", "SW", "SIB", "SF", "SS"), as.numeric) %>%
    
    #add a column that removes the space between the double name
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(Lag2, date1, sep = "")) %>%
    
    #join in to get game number for the team/of the season
    left_join(master_team_schedule_1920, by = "team_date") %>%
    
    select(-c(date2, home_team, away_team, combined_teams, home_team_id, away_team_id, combined_ids,
              home_team_name_double, away_team_name_double, combined_team_names_double, date, team))
  
  return(table)
  
}

#player stats
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


# SHL.se Stats Update -----------------------------------------------------

#teams

#get rid of the joined in additional columns, so that the bind rows continues to work on the update
#(might need to add more tables here)
team_corsi_data_2021 <- team_corsi_data_2021[,c(1:18)]
#
#
#
#

#team corsi
team_corsi_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_2021[1:62])) {
    
    temp <- team_corsi(teamID)
    
    team_corsi_data_2021_update <- rbind(team_corsi_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

#join in friendlier team names
left_join(team_corsi_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> team_corsi_data_2021_update

#append updated rows to updated season table
team_corsi_data_2021 <- rbind(team_corsi_data_2021, team_corsi_data_2021_update)

#team goals
team_goals_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_2021[1:62])) {
    
    temp <- team_goals(teamID)
    
    team_goals_data_2021_update <- rbind(team_goals_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(team_goals_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> team_goals_data_2021_update

team_goals_data_2021 <- rbind(team_goals_data_2021, team_goals_data_2021_update)

#team pp
team_pp_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_2021[1:62])) {
    
    temp <- team_pp(teamID)
    
    team_pp_data_2021_update <- rbind(team_pp_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(team_pp_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> team_pp_data_2021_update

team_pp_data_2021 <- rbind(team_pp_data_2021, team_pp_data_2021_update)

#team pk
team_pk_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_2021[1:62])) {
    
    temp <- team_pk(teamID)
    
    team_pk_data_2021_update <- rbind(team_pk_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(team_pk_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> team_pk_data_2021_update

team_pk_data_2021 <- rbind(team_pk_data_2021, team_pk_data_2021_update)

#team shotsfor
team_shotsfor_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_2021[1:62])) {
    
    temp <- team_shotsfor(teamID)
    
    team_shotsfor_data_2021_update <- rbind(team_shotsfor_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(team_shotsfor_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> team_shotsfor_data_2021_update

team_shotsfor_data_2021 <- rbind(team_shotsfor_data_2021, team_shotsfor_data_2021_update)

#join in friendlier team names
team_corsi_data_2021 <- left_join(team_corsi_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))
team_goals_data_2021 <- left_join(team_goals_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))
team_pp_data_2021 <- left_join(team_pp_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))
team_pk_data_2021 <- left_join(team_pk_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))
team_shotsfor_data_2021 <- left_join(team_shotsfor_data_2021, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double"))

#players

#get rid of the joined in additional columns, so that the bind rows continues to work on the update
#(might need to add more tables here)
player_toi_data_2021 <- player_toi_data_2021[,c(1:30)]
player_points_data_2021 <- player_points_data_2021[,c(1:31)]
player_corsi_data_2021 <- player_corsi_data_2021[,c(1:32)]
player_ev_data_2021 <- player_ev_data_2021[,c(1:26)]

#points
player_points_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_2021[1:62])) {
    
    temp <- player_points(teamID)
    
    player_points_data_2021_update <- rbind(player_points_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(player_points_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> player_points_data_2021_update

player_points_data_2021 <- rbind(player_points_data_2021, player_points_data_2021_update)

#player corsi
player_corsi_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_2021[1:62])) {
    
    temp <- player_corsi(teamID)
    
    player_corsi_data_2021_update <- rbind(player_corsi_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(player_corsi_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> player_corsi_data_2021_update

player_corsi_data_2021 <- rbind(player_corsi_data_2021, player_corsi_data_2021_update)

#ev
player_ev_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_2021[1:62])) {
    
    temp <- player_ev(teamID)
    
    player_ev_data_2021_update <- rbind(player_ev_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(player_ev_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> player_ev_data_2021_update

player_ev_data_2021 <- rbind(player_ev_data_2021, player_ev_data_2021_update)

#toi
player_toi_data_2021_update <- c()
for (teamID in shl_team_dictionary_2021$shlse_team_id[1:14]) {
  
  for (date1 in as.list(shl_dates_2021[1:62])) {
    
    temp <- player_toi(teamID)
    
    player_toi_data_2021_update <- rbind(player_toi_data_2021_update, temp)
    
    print(teamID)
    rm(temp)
    
  }
}

left_join(player_toi_data_2021_update, shl_team_dictionary_2021, by = c("Lag" = "shlse_team_name_double")) -> player_toi_data_2021_update

player_toi_data_2021 <- rbind(player_toi_data_2021, player_toi_data_2021_update)

#join in swehockey names
player_toi_data_2021 <- player_toi_data_2021 %>%
  left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey)

player_points_data_2021 <- player_points_data_2021 %>%
  left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey)

player_ev_data_2021 <- player_ev_data_2021 %>%
  left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey)

player_corsi_data_2021 <- player_corsi_data_2021 %>%
  left_join(player_lookup_2021, by = c("name" = "shlse")) %>%
  rename(swehockey_name = swehockey)

#join in friendlier team names
player_toi_data_2021 <- left_join(player_toi_data_2021, shl_team_dictionary_2021, 
                                  by = c("Lag" = "shlse_team_name_double"))

player_corsi_data_2021 <- left_join(player_corsi_data_2021, shl_team_dictionary_2021, 
                                  by = c("Lag" = "shlse_team_name_double"))

player_ev_data_2021 <- left_join(player_ev_data_2021, shl_team_dictionary_2021, 
                                  by = c("Lag" = "shlse_team_name_double"))

player_points_data_2021 <- left_join(player_points_data_2021, shl_team_dictionary_2021, 
                                  by = c("Lag" = "shlse_team_name_double"))

#bind with master datasets

#team

team_corsi_data_master <- rbind(team_corsi_data_master, team_corsi_data_2021)
team_goals_data_master <- rbind(team_goals_data_master, team_goals_data_2021)
team_pp_data_master <- rbind(team_pp_data_master, team_pp_data_2021)
team_pk_data_master <- rbind(team_pk_data_master, team_pk_data_2021)
team_shotsfor_data_master <- rbind(team_shotsfor_data_master, team_shotsfor_data_2021)

#players

player_points_data_master <- rbind(player_points_data_master, player_points_data_2021)
player_toi_data_master <- rbind(player_toi_data_master, player_toi_data_2021)
player_corsi_data_master <- rbind(player_corsi_data_master, player_corsi_data_2021)
player_ev_data_master <- rbind(player_ev_data_master, player_ev_data_2021)

#export CSVs of master datasets for use in Shiny

#team

write.csv(team_corsi_data_master, file = "team_corsi_data_master.csv")
write.csv(team_goals_data_master, file = "team_goals_data_master.csv")
write.csv(team_pp_data_master, file = "team_pp_data_master.csv")
write.csv(team_pk_data_master, file = "team_pk_data_master.csv")
write.csv(team_shotsfor_data_master, file = "team_shotsfor_data_master.csv")

#players
write.csv(player_corsi_data_master, file = "player_corsi_data_master.csv")
write.csv(player_points_data_master, file = "player_points_data_master.csv")
write.csv(player_ev_data_master, file = "player_ev_data_master.csv")
write.csv(player_toi_data_master, file = "player_toi_data_master.csv")

#player names
write.csv(unique(player_corsi_data_2021$swehockey_name), file = "player_names_2021.csv")

# Swehockey Boxscore Data -------------------------------------------------

#run box scores with updated batch of gameids for each refresh
box_score_data_2021_raw_update <- c()
for (id in gameID_table_1920$id[1:280]) {
  
  temp <- get_box_score(id)
  
  box_score_data_2021_raw <- rbind.fill(box_score_data_2021_raw, temp)
  
  print(id)
  rm(temp)
  
}

#this will break the first time, as there is now 2021_raw
#instead just assign raw to the first update refresh
box_score_data_2021_raw <- rbind(box_score_data_2021_raw, box_score_data_2021_raw_update)


# Swehockey Boxscore Data - Enriched --------------------------------------


#step 1 - join in names from the uid (team, date, jersey number)
box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF1_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF1_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF2_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF2_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF3_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF3_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF4_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF4_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF5_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF5_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GF6_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GF6_name = player)


box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA1_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA1_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA2_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA2_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA3_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA3_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA4_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA4_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA5_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA5_name = player)

box_score_data_2021_raw <- merge(box_score_data_2021_raw, lineup_data_2021[,c("player", "uid")],
                                 by.x = "GA6_uid", by.y = "uid", all.x = TRUE) %>%
                                 rename(GA6_name = player)

#step 2 - concatenate all GF and GA names into their own columns
box_score_data_2021_raw <- box_score_data_2021_raw %>%
  mutate(GF_names = paste(GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name, sep = ''),
         GA_names = paste(GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name, sep = ''))

#step 3 - count the number of goalies and NAs to assess manpower
box_score_data_2021_raw <- box_score_data_2021_raw %>%
  mutate(GF_GK = str_count(box_score_data_2021_raw$GF_names, "GK"),
         GF_NA = str_count(box_score_data_2021_raw$GF_names, "NA")) %>%
  
  mutate(GF_skaters = 6 - GF_GK - GF_NA) %>%
  
  mutate(GA_GK = str_count(box_score_data_2021_raw$GA_names, "GK"),
         GA_NA = str_count(box_score_data_2021_raw$GA_names, "NA")) %>%
  
  mutate(GA_skaters = 6 - GA_GK - GA_NA)

#step 4 - assign a game state
box_score_data_2021_raw <- box_score_data_2021_raw %>%
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

box_score_data_2021_raw <- box_score_data_2021_raw %>%
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
  
  select(gameid, date, home_team, away_team, goal, primary_assist, secondary_assist, game_situation,
         game_situation_general, gf_team, gf_team_game_number, ga_team, ga_team_game_number, GF_names, GA_names,
         GF_skaters, GA_skaters, GF1_name, GF2_name, GF3_name, GF4_name, GF5_name, GF6_name,
         GA1_name, GA2_name, GA3_name, GA4_name, GA5_name, GA6_name)

#adding one additional unique identifier - this is for when I go to create
#player stats for a player unique to one team (not combined & duplicated)
box_score_data_2021_condensed <- box_score_data_1920_condensed %>%
  
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

box_score_data_all <- rbind(box_score_data_all, box_score_data_2021_condensed)

