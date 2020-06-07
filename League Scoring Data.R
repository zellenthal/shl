library(tidyverse)
library(rvest)
library(data.table)

# Basic Info --------------------------------------------------------------

years <- tibble(year_num = c(1, 2, 3, 4, 5, 6, 7),
                season = c('2013-2014', '2014-2015',
                           '2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020'))

leagues <- c('shl', 'allsvenskan', 'liiga', 'denmark', 'norway', 'superelit', 'division-1', 'nla', 'del',
             'czech', 'ebel', 'mestis', 'jr.-a-sm-liiga', 'khl', 'nlb', 'del2', 'ahl', 'echl', 'nhl')

league_lookup <- tibble(league = c('shl', 'allsvenskan', 'liiga', 'denmark', 'norway', 'superelit', 'division-1', 'nla', 'del',
                                   'czech', 'ebel', 'mestis', 'jr.-a-sm-liiga', 'khl', 'nlb', 'del2', 'ahl', 'echl',
                                   'nhl'),
                        
                        league_name = c('SHL', "Allsvenskan", "Liiga", "Denmark", "Norway", "SuperElit", "HockeyEttan",
                                        'NLA', 'DEL', 'Extraliga', 'EBEL', 'Mestis', 'Jr. A SM-liiga', "KHL", "NLB", "DEL2",
                                        'AHL', 'ECHL', 'NHL'))


# Scrape EP League Stats Table --------------------------------------------

get_scoring_table <- function(league, year, page) {
  
  url <- read_html(paste('https://www.eliteprospects.com/league/', league, '/stats/', year, '?page=', page, 
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    na.omit()
  
  table <- table %>%
    mutate(season = year,
           league = league)
  
  #last_page <- url %>%
  #html_nodes("hidden-xs") %>%
  #html_attr('href')
  
  return(table)
  
}

# Each league's data
# For some reason, Denmark data for
# 2013-2014 and 2019-2020 does not work
nhl_data <- c()
for (year in years$season[1:7]) {
  
  for (page in c(1:10)) {
    
    for (league in leagues[19]) {
      
      temp <- get_scoring_table(league, year, page)
      
      nhl_data <- rbind(nhl_data, temp)
      
      rm(temp)
      
    }
  }
}


# Master Scoring Data -----------------------------------------------------

scoring_data_raw <- rbind(czech_data, del_data, denmark_data, ebel_data,
                          ettan_data, ha_data, khl_data, liiga_data,
                          nla_data, norway_data, shl_data, superelit_data)

#Remove unnecessary columns
scoring_data_raw <- scoring_data_raw[,c(2, 11:12, 3:8)]

#Left join in a season number
scoring_data_raw <- scoring_data_raw %>%
  left_join(years, by = "season")

#Reorder columns to move year_num next to season
scoring_data_raw <- scoring_data_raw[,c(1:2, 10, 3:9)]

#Exporting so I can re-name players with the same name
write_excel_csv(scoring_data_raw, 'scoring_data_raw_dupes.csv')

#Re-importing the raw data, changing names of dupe names
scoring_data_raw <- read.csv("scoring_data_raw_deduped.csv", stringsAsFactors = FALSE)


# Filter & Order Raw Data -------------------------------------------------

scoring_data <- scoring_data_raw %>%
  filter(GP > 9)

#order them by player by year
scoring_data <- scoring_data[order(scoring_data$Player, scoring_data$year_num),]

#add a row number
scoring_data %>%
  mutate(row_number()) -> scoring_data

#change column name
colnames(scoring_data)[11] <- "row_number"

#add position of each player
scoring_data <- scoring_data %>%
  mutate(Position = case_when(Player %like% "\\(D" ~ "D",
                              Player %like% "\\(C" ~ "F",
                              Player %like% "\\(W" ~ "F",
                              Player %like% "\\(F" ~ "F",
                              Player %like% "\\(L" ~ "F",
                              Player %like% "\\(R" ~ "F"
  ))

#re-order position columne
scoring_data <- scoring_data[,c(1, 12, 2:11)]


# Consecutive Season Check ------------------------------------------------

#check one row before
consecutive_check1 <- function(n) {
  
  response <- 0
  
  if(abs(scoring_data[n,]$year_num - scoring_data[n-1,]$year_num) == 1 &&
     scoring_data[n,]$Player == scoring_data[n-1,]$Player &&
     scoring_data[n,]$league !=  scoring_data[n-1,]$league) {
    
    response <- 1
    
  }
  
  if(abs(scoring_data[n,]$year_num - scoring_data[n-1,]$year_num) == 0 &&
     scoring_data[n,]$Player == scoring_data[n-1,]$Player &&
     scoring_data[n,]$league !=  scoring_data[n-1,]$league) {
    
    response <- 2
    
  }
  
  return(response)
  
}

consecutive_check1_data <- c()
for (i in c(2:length(scoring_data$Player))) {
  
  temp <- consecutive_check1(i)
  
  consecutive_check1_data <- rbind(consecutive_check1_data, temp)
  
  #print(year)
  rm(temp)
  
}

consecutive_check1_data <- c(0,consecutive_check1_data)
consecutive_check1_data <- consecutive_check1_data %>% tibble()

#check two rows before
consecutive_check2 <- function(n) {
  
  response <- 0
  
  if(abs(scoring_data[n,]$year_num - scoring_data[n-2,]$year_num) == 1 &&
     scoring_data[n,]$Player == scoring_data[n-2,]$Player &&
     scoring_data[n,]$league !=  scoring_data[n-2,]$league) {
    
    response <- 1
    
  }
  
  if(abs(scoring_data[n,]$year_num - scoring_data[n-2,]$year_num) == 0 &&
     scoring_data[n,]$Player == scoring_data[n-2,]$Player &&
     scoring_data[n,]$league !=  scoring_data[n-2,]$league) {
    
    response <- 2
    
  }
  
  return(response)
  
}

consecutive_check2_data <- c()
for (i in c(3:length(scoring_data$Player))) {
  
  temp <- consecutive_check2(i)
  
  consecutive_check2_data <- rbind(consecutive_check2_data, temp)
  
  #print(year)
  rm(temp)
  
}

consecutive_check2_data <- c(0,0,consecutive_check2_data)
consecutive_check2_data <- consecutive_check2_data %>% tibble()

#check three rows before
consecutive_check3 <- function(n) {
  
  response <- 0
  
  if(abs(scoring_data[n,]$year_num - scoring_data[n-3,]$year_num) == 1 &&
     scoring_data[n,]$Player == scoring_data[n-3,]$Player &&
     scoring_data[n,]$league !=  scoring_data[n-3,]$league) {
    
    response <- 1
    
  }
  
  if(abs(scoring_data[n,]$year_num - scoring_data[n-3,]$year_num) == 0 &&
     scoring_data[n,]$Player == scoring_data[n-3,]$Player &&
     scoring_data[n,]$league !=  scoring_data[n-3,]$league) {
    
    response <- 2
    
  }
  
  return(response)
  
}

consecutive_check3_data <- c()
for (i in c(4:length(scoring_data$Player))) {
  
  temp <- consecutive_check3(i)
  
  consecutive_check3_data <- rbind(consecutive_check3_data, temp)
  
  #print(year)
  rm(temp)
  
}

consecutive_check3_data <- c(0,0,0,consecutive_check3_data)
consecutive_check3_data <- consecutive_check3_data %>% tibble()

# League Change -----------------------------------------------------------

#first off going to add in some friendly league names
scoring_data <- scoring_data %>%
  left_join(league_lookup, by = "league")

#reorder to move friendly league next to legue slug
scoring_data <- scoring_data[,c(1:5, 13, 6:12)]

#add in first consecutive check
scoring_data <- cbind(scoring_data, consecutive_check1_data)
colnames(scoring_data)[14] <- "flag1"

#first batch of league changes
league_change1 <- function(n) {
  
  response <- ""
  
  if(scoring_data[n,]$flag1 !=  0) {
    
    response <- paste(scoring_data$league_name[n - 1], " to ", scoring_data$league_name[n], sep = '')
  }
  
  return(response)
  
}

league_change1_data <- c()
for (i in c(2:length(scoring_data$Player))) {
  
  temp <- league_change1(i)
  
  league_change1_data <- rbind(league_change1_data, temp)
  
  #print(year)
  rm(temp)
  
}

league_change1_data <- c("",league_change1_data)

league_change1_data <- league_change1_data %>% tibble()

scoring_data <- cbind(scoring_data, league_change1_data)
colnames(scoring_data)[15] <- "league_change1"

#add in second consecutive check
scoring_data <- cbind(scoring_data, consecutive_check2_data)
colnames(scoring_data)[16] <- "flag2"

#second batch of league changes
league_change2 <- function(n) {
  
  response <- ""
  
  if(scoring_data[n,]$flag2 !=  0) {
    
    response <- paste(scoring_data$league_name[n - 2], " to ", scoring_data$league_name[n], sep = '')
  }
  
  return(response)
  
}

league_change2_data <- c()
for (i in c(3:length(scoring_data$Player))) {
  
  temp <- league_change2(i)
  
  league_change2_data <- rbind(league_change2_data, temp)
  
  #print(year)
  rm(temp)
  
}

league_change2_data <- c("", "", league_change2_data)
league_change2_data <- league_change2_data %>% tibble()

scoring_data <- cbind(scoring_data, league_change2_data)
colnames(scoring_data)[17] <- "league_change2"

#add in third consecutive check
scoring_data <- cbind(scoring_data, consecutive_check3_data)
colnames(scoring_data)[18] <- "flag3"

#second batch of league changes
league_change3 <- function(n) {
  
  response <- ""
  
  if(scoring_data[n,]$flag3 !=  0) {
    
    response <- paste(scoring_data$league_name[n - 3], " to ", scoring_data$league_name[n], sep = '')
  }
  
  return(response)
  
}

league_change3_data <- c()
for (i in c(4:length(scoring_data$Player))) {
  
  temp <- league_change3(i)
  
  league_change3_data <- rbind(league_change3_data, temp)
  
  #print(year)
  rm(temp)
  
}

league_change3_data <- c("", "", "", league_change3_data)
league_change3_data <- league_change3_data %>% tibble()

scoring_data <- cbind(scoring_data, league_change3_data)
colnames(scoring_data)[19] <- "league_change3"

#concatenate all league changes
scoring_data <- scoring_data %>% 
  mutate(league_changes = paste(league_change1, league_change2, league_change3, sep = ','))

#add the lookup number for future left join
scoring_data <- scoring_data %>%
  mutate(row_number1 = row_number - 1,
         row_number2 = row_number - 2,
         row_number3 = row_number - 3)

#reorder columns
scoring_data <- scoring_data[,c(1:14, 21, 15:16, 22, 17:18, 23, 19:20)]

# Join in Comparison Seasons ----------------------------------------------

#just the rows with the relevant seasons
scoring_data_change <- scoring_data %>%
  filter(league_changes != ',,') %>%
  select(-league)

#splitting out the league changes
league_change1_leagues <- str_split_fixed(scoring_data_change$league_change1, " to ", 2) 
league_change2_leagues <- str_split_fixed(scoring_data_change$league_change2, " to ", 2) 
league_change3_leagues <- str_split_fixed(scoring_data_change$league_change3, " to ", 2) 

#add in the league change components
#first league set 
scoring_data_change <- cbind(scoring_data_change, league_change1_leagues)

colnames(scoring_data_change)[23] <- "league_from1"
colnames(scoring_data_change)[24] <- "league_to1"

scoring_data_change <- scoring_data_change[,c(1:15, 23:24, 16:22)]

#second league set
scoring_data_change <- cbind(scoring_data_change, league_change2_leagues)

colnames(scoring_data_change)[25] <- "league_from2"
colnames(scoring_data_change)[26] <- "league_to2"

scoring_data_change <- scoring_data_change[,c(1:20, 25:26, 21:24)]

#third league set
scoring_data_change <- cbind(scoring_data_change, league_change3_leagues)

colnames(scoring_data_change)[27] <- "league_from3"
colnames(scoring_data_change)[28] <- "league_to3"

scoring_data_change <- scoring_data_change[,c(1:25, 27:28, 26)]


# Expand Twice Fitting Seasons in Separate Rows ---------------------------

row_expansion <- function(row_number) {
  
  data <- scoring_data_change[row_number,][c(1:12,14:17)]
  
  data <- data %>%
    rename(row_number_comp = row_number1,
           league_change = league_change1,
           league_from = league_from1,
           league_to = league_to1)
  
  
  data2 <- scoring_data_change[FALSE,][c(1:12,14:17)]
  data3 <- scoring_data_change[FALSE,][c(1:12,14:17)]
  
  if(scoring_data_change$flag2[row_number] != 0) {
    
    data2 <- scoring_data_change[row_number,][c(1:12,19:22)]
    
    data2 <- data2 %>%
      rename(row_number_comp = row_number2,
             league_change = league_change2,
             league_from = league_from2,
             league_to = league_to2)
    
  }
  
  if(scoring_data_change$flag3[row_number] != 0) {
    
    data3 <- scoring_data_change[row_number,][c(1:12,24:27)]
    
    data3 <- data3 %>%
      rename(row_number_comp = row_number3,
             league_change = league_change3,
             league_from = league_from3,
             league_to = league_to3)
    
  }
  
  data <- rbind(data, data2, data3)
  return(data)
  
}

#use this function to create the new, split out dataset
scoring_data_change_expanded <- c()
for (row_number in c(1:length(scoring_data_change$Player))) {
  
  temp <- row_expansion(row_number)
  
  scoring_data_change_expanded <- rbind(scoring_data_change_expanded, temp)
  
  #print(row_number)
  rm(temp)
  
}

#clean up this data a bit, remove rows where the first league change is NA
scoring_data_change_expanded <- scoring_data_change_expanded %>%
  na_if("") %>%
  na.omit %>% view()

#join in the comparison season data
scoring_data_change_expanded <- scoring_data_change_expanded %>%
  left_join(scoring_data, by = c("row_number_comp" = "row_number"))

scoring_data_change_expanded <- scoring_data_change_expanded[,c(1:2, 14, 19, 22:28, 3, 5:11)]

#better column names to compare Y1 vs. Y2
scoring_data_change_expanded <- scoring_data_change_expanded %>%
  rename(Player = Player.x,
        Position = Position.x,
        `League Change` = league_change,
        Season_Y1 = season.y,
        League_Y1 = league_name.y,
        Team_Y1 = Team.y,
        GP_Y1 = GP.y, G_Y1 = G.y, A_Y1 = A.y, TP_Y1 = TP.y, PPG_Y1 = PPG.y,
        
        Season_Y2 = season.x,
        League_Y2 = league_name.x,
        Team_Y2 = Team.x,
        GP_Y2 = GP.x, G_Y2 = G.x, A_Y2 = A.x, TP_Y2 = TP.x, PPG_Y2 = PPG.x)

#export this final dataset to be used in a Shiny app
write_excel_csv(scoring_data_change_expanded, "scoring_data_change_expanded.csv")


scoring_data_change_expanded %>%
  group_by(`League Change`, Position) %>%
  summarise(
    
    Seasons = n(),
    
    GP_Y1 = sum(GP_Y1),
    TP_Y1 = sum(TP_Y1),
    PPG_Y1 = round(TP_Y1 / GP_Y1, 3),
    
    GP_Y2 = sum(GP_Y2),
    TP_Y2 = sum(TP_Y2),
    PPG_Y2 = round(TP_Y2 / GP_Y2, 3),
    
    Translation = round(PPG_Y2 / PPG_Y1, 3)

  ) %>% view()


page <- read_html("https://www.eliteprospects.com/league/shl/stats/2019-2020")
page %>% html_nodes("table") %>% .[2] %>% html_attr('href')
page %>% html_nodes("table") %>% html_attr('href')

page <- read_html("http://www.yelp.com/search?find_loc=New+York,+NY,+USA")
page %>% html_nodes(".biz-name") %>% html_attr('href')

page %>%
  html_nodes("table") %>% 
  html_attr("href")

page <- read_html("table.html") #just read the html once

web <- page %>%
  html_nodes("table") %>% .[2] %>% html_nodes("tr") %>% html_nodes("a") %>%
  html_attr("href") #as above

web2 <- page %>%
  html_nodes("table") %>% .[2] %>% html_nodes("tr") %>% html_node("a") %>%
  html_attr("href") #just the first url in each tr

webdf <- data.frame(web=web, #full list
                    group=cumsum(web %in% web2), #grouping indicator by tr
                    stringsAsFactors=FALSE)

