# Code to run Svengelska Hockey Shiny App


# Install Packages --------------------------------------------------------

#install this specific version of broom

#require(remotes)
#install_version("broom", version = "0.5.6", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(broom)

library(shiny)
#library(tidyverse)
library(ggplot2)
library(shinythemes)
library(DT) #make manipulations to data tables
library(dplyr)
library(ggpubr) #player cards
library(ggthemes) #players cards
library(reshape2) #transform data used in player cards
library(extrafont)
library(grid) #align multiple player cards
library(gridExtra) #align multiple player cards
library(zoo) #moving averages
library(purrr) #set_names function
library(rstatix)

library(bootstraplib) #test different styling

#library(plotly)

# Import Stats (Updates Every Refresh) ------------------------------------

# Administrative Data
player_toi_data_master <- read.csv("player_toi_data_master.csv", stringsAsFactors = FALSE)
#player_points_data_master <- read.csv("player_points_data_master.csv", stringsAsFactors = FALSE)
player_corsi_data_master <- read.csv("player_corsi_data_master.csv", stringsAsFactors = FALSE)
player_shots_data_master <- read.csv("player_shots_data_master.csv", stringsAsFactors = FALSE)
#box_score_data_master <- read.csv("box_score_data_master.csv", stringsAsFactors = FALSE)
box_score_data_master <- read.csv("box_score_data_all.csv", stringsAsFactors = FALSE)

box_score_data_master <- box_score_data_master %>%
  dplyr::mutate(GF_player_teams = paste0(GF1_player_team, GF2_player_team, GF3_player_team,
                                         GF4_player_team, GF5_player_team, GF6_player_team),
                
                GA_player_teams = paste0(GA1_player_team, GA2_player_team, GA3_player_team,
                                         GA4_player_team, GA5_player_team, GA6_player_team))
 

#SHL - Team

team_pp_data_master <- read.csv("team_pp_data_master.csv", stringsAsFactors = FALSE)
team_pk_data_master <- read.csv("team_pk_data_master.csv", stringsAsFactors = FALSE)
team_shotsfor_data_master <- read.csv("team_shotsfor_data_master.csv", stringsAsFactors = FALSE)
team_shotsagainst_data_master <- read.csv("team_shotsagainst_data_master.csv", stringsAsFactors = FALSE)
#team_corsi_data_master_team <- read.csv("team_corsi_data_master_team.csv", stringsAsFactors = FALSE)
team_corsi_data_master_team <- read.csv("team_corsi_data_master.csv", stringsAsFactors = FALSE)

swehockey_team_names <- team_corsi_data_master_team$swehockey_team_name %>% unique() %>% sort()

shl_team_dictionary <- read.csv("shl_team_dictionary.csv", stringsAsFactors = FALSE)

# Player Stats Data
`5v5` <- read.csv("player_5v5_data_master.csv", stringsAsFactors = FALSE)
EV <- read.csv("player_es_data_master.csv", stringsAsFactors = FALSE)
PP <- read.csv("player_pp_data_master.csv", stringsAsFactors = FALSE)
SH <- read.csv("player_sh_data_master.csv", stringsAsFactors = FALSE)
`All Sits` <- read.csv("player_allsits_data_master.csv", stringsAsFactors = FALSE)
`PS/SO` <- read.csv("player_1v0_data_master.csv", stringsAsFactors = FALSE)
`3v3` <- read.csv("player_3v3_data_master.csv", stringsAsFactors = FALSE)
`4v4` <- read.csv("player_4v4_data_master.csv", stringsAsFactors = FALSE)
`5v4` <- read.csv("player_5v4_data_master.csv", stringsAsFactors = FALSE)
`5v3` <- read.csv("player_5v3_data_master.csv", stringsAsFactors = FALSE)
`Empty Net` <- read.csv("player_eng_data_master.csv", stringsAsFactors = FALSE)

# Clean up syntax of column names
EV <- EV %>% rename(`GF%` = `GF.`, `TOI/GP` = `TOI.GP`, `NHL Rights` = NHL.Rights, `Sh%` = `Sh.`)
PP <- PP %>% rename(`GF%` = `GF.`, `TOI/GP` = `TOI.GP`, `NHL Rights` = NHL.Rights, `Sh%` = `Sh.`)
SH <- SH %>% rename(`GF%` = `GF.`, `TOI/GP` = `TOI.GP`, `NHL Rights` = NHL.Rights)
`All Sits` <- `All Sits` %>% rename(`GF%` = `GF.`, `TOI/GP` = `TOI.GP`, `NHL Rights` = NHL.Rights, `Sh%` = `Sh.`)
#`5v5` <- `5v5` %>% rename(`GF%` = `GF.`, `CF%` = `CF.`, `NHL Rights` = NHL.Rights)
`5v5` <- `5v5` %>% rename(`GF%` = `GF.`, `CF%` = `CF.`,`CF% Rel` = `CF..Rel`,  `NHL Rights` = NHL.Rights)
`3v3` <- `3v3` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`4v4` <- `4v4` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`5v4` <- `5v4` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`5v3` <- `5v3` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`Empty Net` <- `Empty Net` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`PS/SO` <- `PS/SO` %>% rename(`NHL Rights` = NHL.Rights)


# Team Stats Data
`5v5 ` <- read.csv("team_5v5_data.csv", stringsAsFactors = FALSE)
`PP ` <- read.csv("team_pp_data.csv", stringsAsFactors = FALSE)
`SH ` <- read.csv("team_pk_data.csv", stringsAsFactors = FALSE)
`EV ` <- read.csv("team_ev_data.csv", stringsAsFactors = FALSE)

# Clean up syntax of column names
`5v5 ` <- `5v5 ` %>% rename(`GF%` = `GF.`, `CF%` = `CF.`, `FF%` = `FF.`)
`PP ` <- `PP ` %>% rename(`GF%` = `GF.`, `Sh%` = `Sh.`)
`SH ` <- `SH ` %>% rename(`GF%` = `GF.`, `Sv%` = `Sv.`)
`EV ` <- `EV ` %>% rename(`GF%` = `GF.`)

#Player Names Master
player_names_master <- sort(unique(as.character(EV$Player)))

player_lookup_v2 <- read.csv("player_lookup_v2.csv", stringsAsFactors = FALSE)
shl_playerdb_master <- read.csv("shl_player_db_master.csv", stringsAsFactors = FALSE)
shl_playerdb_v2 <- read.csv("shl_player_db_v2.csv", stringsAsFactors = FALSE)

#HockeyAllsvenskan Player Data
`5v5  ` <- read.csv("ha_player_5v5_data_master.csv", stringsAsFactors = FALSE)
`EV  ` <- read.csv("ha_player_ev_data_master.csv", stringsAsFactors = FALSE)
`PP  ` <- read.csv("ha_player_pp_data_master.csv", stringsAsFactors = FALSE)
`SH  ` <- read.csv("ha_player_sh_data_master.csv", stringsAsFactors = FALSE)
`All Sits  ` <- read.csv("ha_player_allsits_data_master.csv", stringsAsFactors = FALSE)
`PS/SO  ` <- read.csv("ha_player_1v0_data_master.csv", stringsAsFactors = FALSE)
`3v3  ` <- read.csv("ha_player_3v3_data_master.csv", stringsAsFactors = FALSE)
`4v4  ` <- read.csv("ha_player_4v4_data_master.csv", stringsAsFactors = FALSE)
`5v4  ` <- read.csv("ha_player_5v4_data_master.csv", stringsAsFactors = FALSE)
`5v3  ` <- read.csv("ha_player_5v3_data_master.csv", stringsAsFactors = FALSE)
`Empty Net  ` <- read.csv("ha_player_eng_data_master.csv", stringsAsFactors = FALSE)

`5v5  ` <- `5v5  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`EV  ` <- `EV  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`PP  ` <- `PP  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`SH  ` <- `SH  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`All Sits  ` <- `All Sits  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`5v4  ` <- `5v4  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`5v3  ` <- `5v3  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`4v4  ` <- `4v4  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`3v3  ` <- `3v3  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`Empty Net  ` <- `Empty Net  ` %>% rename(`GF%` = `GF.`, `NHL Rights` = NHL.Rights)
`PS/SO  ` <- `PS/SO  ` %>% rename(`NHL Rights` = NHL.Rights)

#HockeyAllsvenskan Detailed Data
ha_box_score_data_master <- read.csv("ha_box_score_data_all.csv", stringsAsFactors = FALSE)
ha_player_game_logs_2021 <- read.csv("ha_player_game_logs_2021.csv", stringsAsFactors = FALSE)
ha_player_db <- read.csv("ha_player_db.csv", stringsAsFactors = FALSE)
ha_playerdb_v2 <- read.csv("ha_playerdb_v2.csv", stringsAsFactors = FALSE)

nhl_abbrev <- c("ANA", "ARI", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL",
                "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "VGK", "WSH", "WPG")

# player_corsi_data_master$swehockey_name[player_corsi_data_master$swehockey_name == 'Jonathan Johnson'] <- 'Jonathan Johnsson'
# player_corsi_data_master$player_team[player_corsi_data_master$player_team == 'Jonathan Johnson$Skellefteå AIK'] <- 'Jonathan Johnsson$Skellefteå AIK'
# 
# player_toi_data_master$swehockey_name[player_toi_data_master$swehockey_name == 'Jonathan Johnson'] <- 'Jonathan Johnsson'
# player_toi_data_master$player_team[player_toi_data_master$player_team == 'Jonathan Johnson$Skellefteå AIK'] <- 'Jonathan Johnsson$Skellefteå AIK'

# Key Functions -----------------------------------------------------------

player_card <- function(player_name, season_year) {
  
  bio <- shl_playerdb_v2 %>%
    dplyr::filter(swehockey == player_name,
                  season == season_year)
  
  Age <- bio$age2[1]
  Nationality <- bio$nationality[1]
  Position <- bio$pos[1]
  
  #for plot title - identify which teams a player played for
  #ends up being plugged into the TOI visual
  teams <- player_toi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
    select(swehockey_name, shlse_team_name, game_number) %>%
    left_join(shl_team_dictionary, by = c("shlse_team_name" = "shlse_team_name"))
  
  teams <- teams[order(teams$game_number),]
  teams_list <- unique(teams$shlse_team_name)
  
  player_team <- paste0(player_name, "$", teams$swehockey_team_name[1]) %>%
    as.character()
  
  #TOI
  #create one-off table to expand y axis as needed - will use this later
  y_toi_axis <- player_toi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year)
  
  #pull relevant data from master
  table_toi <- player_toi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
    select(game_number, ESTOI, PPTOI, SHTOI) %>%
    #rename it for what displays in the legend
    rename(EV = ESTOI, PP = PPTOI, SH = SHTOI)
  
  #melt so can plot a stacked column (only way to create stacked column?)
  table_toi2 <- melt(table_toi, id.var="game_number") %>%
    rename(GameNumber = game_number, Situation = variable, TOI = value)
  
  #create the order of those stacked components - don't fully understand why this is necessary
  levels(table_toi2$Situation)
  table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
  
  #ggplot for the toi visual
  toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),],
                       aes(x = GameNumber, y = TOI, fill = Situation)) +
    geom_bar(stat = "identity") +
    
    #creating the title for the overall plot, plus the TOI subheader data
    labs(title = paste(player_name, " ", season_year, " Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$EV), ' GP',  sep = ''),
    #labs(title = paste(player_name, " ", season_year, " Season | ", y_toi_axis$shlse_team_name, " | ", "Pos: ", Position, " | ", NROW(table_toi$EV), ' GP | ',"Age ", Age,  sep = ''),
         subtitle = paste("TOI by Game Situation \n",
                          round(sum(table_toi$EV)/NROW(table_toi$EV), 1), ' EV  -  ',
                          round(sum(table_toi$PP)/NROW(table_toi$EV), 1), ' PP  -  ',
                          round(sum(table_toi$SH)/NROW(table_toi$EV), 1), ' SH', sep = '')) +
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(plot.title = element_text(face = "bold")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#FFD17F", "#FF7F7F","#7F7FFF")) +
    #x axis spans entire 52 game season
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    #default y axis goes from 0 to 20 minutes
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
    theme(text = element_text(size = 9.5))
  
  #but for players with games > 20 minutes, this is where the y axis table comes in
  if(max(y_toi_axis$TOI) > 20){
    toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
                                                  expand = c(0, 0))
  }
  
  if(length(teams_list) == 2){
    toi_visual <- toi_visual + labs(title = paste(player_name, " ", season_year, " Season | ", teams_list[1], ", ", teams_list[2], " | ", NROW(table_toi$EV), ' GP',  sep = ''),
                                    subtitle = paste("TOI by Game Situation \n",
                                                     round(sum(table_toi$EV)/NROW(table_toi$EV), 1), ' EV  -  ',
                                                     round(sum(table_toi$PP)/NROW(table_toi$EV), 1), ' PP  -  ',
                                                     round(sum(table_toi$SH)/NROW(table_toi$EV), 1), ' SH', sep = ''))
  }
  
  if(length(teams_list) == 3){
    toi_visual <- toi_visual + labs(title = paste(player_name, " ", season_year, " Season | ", teams_list[1], ", ", teams_list[2], ", ", teams_list[3], " | ", NROW(table_toi$EV), ' GP',  sep = ''),
                                    subtitle = paste("TOI by Game Situation \n",
                                                     round(sum(table_toi$EV)/NROW(table_toi$EV), 1), ' EV  -  ',
                                                     round(sum(table_toi$PP)/NROW(table_toi$EV), 1), ' PP  -  ',
                                                     round(sum(table_toi$SH)/NROW(table_toi$EV), 1), ' SH', sep = ''))
  }
  
  
  #Points
  game_number <- tibble(c(1:52)) %>% set_names('number')
  
  # table_points_old <- player_points_data_master %>%
  #   filter(swehockey_name == player_name) %>%
  #   filter(season == season_year) %>%
  #   select(game_number, G, A1, A2)
  # 
  # y_points_axis <- player_points_data_master %>%
  #   filter(swehockey_name == player_name) %>%
  #   filter(season == season_year)
  
  #even strength goals
  EVG_data <- box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'EV') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_EV = sum(count))
  
  #even strength assists
  EVA1_data <- box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'EV') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_EV = sum(count))
  
  EVA2_data <- box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'EV') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_EV = sum(count))
  
  #pp goals
  PPG_data <- box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PP') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_PP = sum(count))
  
  #pp assists
  PPA1_data <- box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PP') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_PP = sum(count))
  
  PPA2_data <- box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PP') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_PP = sum(count))
  
  #sh goals
  SHG_data <- box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SH') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_SH = sum(count))
  
  #sh assists
  SHA1_data <- box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SH') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_SH = sum(count))
  
  SHA2_data <- box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SH') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_SH = sum(count))
  
  #ps goals
  PSG_data <- box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PS') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_PS = sum(count))
  
  SOG_data <- box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SO') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_SO = sum(count))
  
  #empty net goals
  ENG_data <- box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'ENG') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_EN = sum(count))
  
  ENA1_data <- box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'ENG') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_EN = sum(count))
  
  ENA2_data <- box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'ENG') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_EN = sum(count))
  
  #goalie puled
  GPG_data <- box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_GP = sum(count))
  
  GPA1_data <- box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_GP = sum(count))
  
  GPA2_data <- box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_GP = sum(count))
  
  points_master <- left_join(game_number, EVG_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, EVA1_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, EVA2_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, PPG_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, PPA1_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, PPA2_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, SHG_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, SHA1_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, SHA2_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, PSG_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, SOG_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, ENG_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, ENA1_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, ENA2_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, GPG_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, GPA1_data, by = c("number" = "gf_team_game_number"))
  points_master <- left_join(points_master, GPA2_data, by = c("number" = "gf_team_game_number"))
  
  points_master[is.na(points_master)] <- 0
  
  points_master <- points_master %>%
    mutate(A_EV = A1_EV + A2_EV,
           A_PP = A1_PP + A2_PP + A1_GP + A2_GP,
           G_PP = G_PP + G_GP,
           A_SH = A1_SH + A2_SH,
           G_PSSO = G_PS + G_SO,
           A_EN = A1_EN + A2_EN)
  
  table_points <- points_master %>%
    select(number, G_EV, G_PP, G_SH, G_EN, G_PSSO, A_EV, A_PP, A_SH, A_EN) %>%
    rename(game_number = number) %>%
    rename(EV_G = G_EV,
           EV_A = A_EV,
           PP_G = G_PP,
           EN_G = G_EN,
           PP_A = A_PP,
           SH_G = G_SH,
           SH_A = A_SH,
           EN_A = A_EN,
           PS_G = G_PSSO)
  
  y_points_axis <- table_points %>%
    dplyr::mutate(TP = EV_G + PP_G + SH_G + EN_G + PS_G + EV_A + PP_A + SH_A + EN_A)
  
  table_points_2 <- melt(table_points, id.var="game_number") %>%
    rename(GameNumber = game_number, PointType = variable, Points = value)
  
  levels(table_points_2$PointType)
  table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
  
  # table_points_2$PointType <- factor(table_points_2$PointType, levels=c("ES_G", "ES_A", "PP_G", "PP_A",
  #                                                                       "SH_G", "SH_A", "PS_G"))
  
  # table_points_2$PointType <- factor(table_points_2$PointType, levels=c("ES_G", "PP_G", "SH_G", "PS_G",
  #                                                                       "ES_A", "PP_A", "SH_A"))
  
  # table_points_2$PointType <- factor(table_points_2$PointType, levels=c("ES_G", "PP_G", "SH_G", "PS_G",
  #                                                                       "ES_A", "PP_A", "SH_A"))
  
  table_points_2$PointType <- factor(table_points_2$PointType, levels=c("EN_A", "SH_A", "PP_A", "EV_A",
                                                                        "PS_G", "EN_G", "SH_G", "PP_G", "EV_G"))
  
  # table_points_2$PointType <- factor(table_points_2$PointType, levels=c("PS_G", "EN_A", "EN_G", "SH_A",
  #                                                                       "SH_G", "PP_A", "PP_G", "ES_A", "ES_G"))
  
  
  es_g_hex <- "#7f7fff"
  es_a_hex <- "#b2b2ff"
  pp_g_hex <- "#ff7f7f"
  pp_a_hex <- "#ffb3b3"
  sh_g_hex <- "#ffd17f"
  #sh_g_hex <- "#ffb433"
  sh_a_hex <- "#ffe3b3"
  #sh_a_hex <- "#FFE766"
  ps_g_hex <- "#abd3ab"
  en_g_hex <- "#ca80ff"
  en_a_hex <- "#FDB3FF"
  
  
  points_visual <- ggplot(table_points_2[order(table_points_2$PointType),],
                          aes(x = GameNumber, y = Points, fill = PointType)) +
    geom_bar(stat = "identity") +
    
    # labs(subtitle = paste("Point Production \n",
    #                       sum(table_points_old$G), ' G  -  ',
    #                       sum(table_points_old$A1), ' A1  -  ',
    #                       sum(table_points_old$A2), ' A2  (',
    #
    #                       round((sum(y_points_axis$TP) / sum(y_toi_axis$TOI)) * 60, 2),
    #
    #                       ' P/60)',
    #
    #                       sep = '')) +
  
  labs(subtitle = paste("Point Production \n",
                        "EV: ",
                        sum(table_points$EV_G), ' G  -  ',
                        sum(table_points$EV_A), ' A  -  ',
                        (sum(table_points$EV_G) + sum(table_points$EV_A)), ' P  (',
                        
                        round(((sum(table_points$EV_G) + sum(table_points$EV_A)) / sum(table_toi$EV)) * 60, 2),
                        
                        ' P/60)  |  ',
                        
                        "PP: ",
                        sum(table_points$PP_G), ' G  -  ',
                        sum(table_points$PP_A), ' A  -  ',
                        (sum(table_points$PP_G) + sum(table_points$PP_A)), ' P  (',
                        
                        round(((sum(table_points$PP_G) + sum(table_points$PP_A)) / sum(table_toi$PP)) * 60, 2),
                        
                        ' P/60)',
                        
                        sep = '')) +
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +
    # scale_fill_manual(values=c(es_g_hex, es_a_hex, pp_g_hex, pp_a_hex, sh_g_hex, sh_a_hex, ps_g_hex)) +
    #scale_fill_manual(values=c(es_g_hex, pp_g_hex, sh_g_hex, ps_g_hex, es_a_hex, pp_a_hex, sh_a_hex)) +
    scale_fill_manual(values=c(en_a_hex, sh_a_hex, pp_a_hex, es_a_hex, ps_g_hex, en_g_hex, sh_g_hex, pp_g_hex, es_g_hex)) +
    #scale_fill_manual(values=c(ps_g_hex, en_a_hex, en_g_hex, sh_a_hex, sh_g_hex, pp_a_hex, pp_g_hex, es_a_hex, es_g_hex)) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(text = element_text(size = 9.5))
  #guides(fill=guide_legend(ncol=2))
  
  # +
  #   guides(fill = guide_legend(reverse = TRUE))
  
  
  if(max(y_points_axis$TP) > 3){
    points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_points_axis$TP)),
                                                        expand = c(0, 0))
  }
  
  #Corsi
  y_corsi_axis <- player_corsi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
    #select(gameNumber, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2) %>%
    mutate(CA_abs = abs(CA))
  
  #storing values to calculate stats for subheader (CF%, relative CF%)
  player_corsi <- round((sum(y_corsi_axis$CF) / (sum(y_corsi_axis$CF) + sum(y_corsi_axis$CA_abs)) *100), 1)
  player_off_corsi <- round((sum(y_corsi_axis$CF_off) / (sum(y_corsi_axis$CF_off) + sum(y_corsi_axis$CA_off)) *100), 1)
  CF_rel <- player_corsi - player_off_corsi
  
  table_corsi <- player_corsi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
    select(game_number.x, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2)
  
  game_number <- tibble(c(1:52)) %>% set_names('number')
  
  shot_data <-
    left_join(player_corsi_data_master, player_toi_data_master, by = "name_date") %>%
    filter(swehockey_name.x == player_name) %>%
    filter(season.x == season_year) %>%
    select(game_number.x, CF, CA, ESTOI) %>%
    mutate(player_game_number = row_number()) %>%
    
    mutate(CFL5 = ifelse(player_game_number >= 5,
                         rollsumr(CF, 5, fill = NA),
                         0)) %>%
    mutate(CAL5 = ifelse(player_game_number >= 5,
                         rollsumr(CA, 5, fill = NA),
                         0)) %>%
    mutate(ESTOIL5 = ifelse(player_game_number >= 5,
                            rollsumr(ESTOI, 5, fill = NA),
                            0)) %>%
    
    mutate(CF60 = round((CFL5 / ESTOIL5) * 60, 1)) %>%
    mutate(CA60 = round((CAL5 / ESTOIL5) * 60, 1))
  
  shot_data %>%
    select(game_number.x, CF60, CA60) -> shot_data_condensed
  
  left_join(game_number, shot_data_condensed, by = c("number" = "game_number.x")) -> shot_data_condensed
  
  shot_data_condensed <- reshape2::melt(shot_data_condensed, id.var = "number") %>%
    rename(ShotType = variable, Shots = value)
  
  corsi_visual <- ggplot(shot_data_condensed, aes(x = number, y = Shots, color = `ShotType`)) +
    geom_line(size = 1.25) +
    geom_line(aes(y = 53.7), color = "#B2B2FF", size = 0.66, linetype = 'dotted') +
    
    labs(subtitle = paste("Shot Rates Per 60, 5-Game Moving Avg. \n",
                          
                          # round((sum(shot_data$CF) / sum(shot_data$ESTOI)) * 60, 1), ' CF60, ',
                          #
                          # round((sum(shot_data$CA) / sum(shot_data$ESTOI)) * 60, 1), ' CA60, ',
                          
                          round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1),
                          ' CF%',
                          
                          ' (', round(CF_rel, 1),' rel)', sep = '')) +
    
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_color_manual(values=c("#7F7F7F", "#FE7F7E")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(20,80), expand = c(0, 0)) +
    theme(axis.title.x = element_blank()) +
    theme(text = element_text(size = 9.5))
  
  #if someone played 10 or fewer games
  if(NROW(table_toi$EV) < 10){
    
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
      scale_y_continuous(limits = c(-20, 20), expand = c(0, 0)) +
      theme(text = element_text(size = 9.5))
    
    if(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) > 20){
      corsi_visual <- corsi_visual + scale_y_continuous(limits=c(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) * -1, max(y_corsi_axis$CF,y_corsi_axis$CA_abs)),
                                                        expand = c(0, 0))
    }
  }
  
  #Goals
  #creating an empty table - will merge goal data in later
  game_numbers <- tibble(c(1:52)) %>%
    set_names("game_number")
  
  #for people on for no goals
  empty_goals <- tibble(game_number = c(1:52), GF = 0, GA = 0)
  
  empty_goals_2 <- melt(empty_goals, id.var = "game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)
  
  levels(empty_goals_2$GoalType)
  empty_goals_2$GoalType <- factor(empty_goals_2$GoalType, levels = rev(levels(empty_goals_2$GoalType)))
  #end of people on for no goals
  
  GF_data <- box_score_data_master %>%
    subset(grepl(player_name, GF_names)) %>%
    filter(season == season_year) %>%
    filter(game_situation == '5v5') %>%
    select(GF_names, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(GF_names)) %>%
    group_by(gf_team_game_number) %>%
    summarise(GF = sum(count))

  GA_data <- box_score_data_master %>%
    subset(grepl(player_name, GA_names)) %>%
    filter(season == season_year) %>%
    filter(game_situation == '5v5') %>%
    select(GA_names, ga_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(GA_names)) %>%
    group_by(ga_team_game_number) %>%
    summarise(GA = sum(count))
  
  # GF_data <- box_score_data_master %>%
  #   subset(grepl(paste0(player_name, "$", teams$swehockey_team_name[1]), GF_player_teams)) %>%
  #   filter(season == season_year) %>%
  #   filter(game_situation == '5v5') %>%
  #   select(GF_player_teams, gf_team_game_number) %>%
  #   mutate(count = 1) %>%
  #   select(-c(GF_player_teams)) %>%
  #   group_by(gf_team_game_number) %>%
  #   summarise(GF = sum(count))
  # 
  # GA_data <- box_score_data_master %>%
  #   subset(grepl(paste0(player_name, "$", teams$swehockey_team_name[1]), GA_player_teams)) %>%
  #   filter(season == season_year) %>%
  #   filter(game_situation == '5v5') %>%
  #   select(GA_player_teams, ga_team_game_number) %>%
  #   mutate(count = 1) %>%
  #   select(-c(GA_player_teams)) %>%
  #   group_by(ga_team_game_number) %>%
  #   summarise(GA = sum(count))
  
  y_goals_axis <- merge(game_numbers, GF_data, by.x = "game_number",
                        by.y = "gf_team_game_number", all.x = TRUE)
  
  y_goals_axis <- merge(y_goals_axis, GA_data, by.x = "game_number",
                        by.y = "ga_team_game_number", all.x = TRUE)
  
  y_goals_axis <- y_goals_axis %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2) %>%
    mutate(GA_abs = abs(GA))
  
  table_goals <- y_goals_axis %>%
    select(-c(GA_abs))
  
  table_goals_2 <- melt(table_goals, id.var = "game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)
  
  levels(table_goals_2$GoalType)
  table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))
  
  table_goals[is.na(table_goals)] <- 0
  y_goals_axis[is.na(y_goals_axis)] <- 0
  
  goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),],
                         aes(x = GameNumber, y = Goals, fill = GoalType)) +
    geom_bar(stat = "identity") +
    
    labs(subtitle = paste('5v5 Goals For and Against \n',
                          sum(table_goals$GF), ' GF  -  ',
                          sum(y_goals_axis$GA_abs), ' GA ', ' (',
                          round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)) * 100,1),
                          ' GF%)',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 1), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@SvengelskaHky'), sep = '') +
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) +
    theme(text = element_text(size = 9.5))
  
  # if the axis needs to be expanded
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
    
  }
  
  # if someone has only been on for GF, make sure it's black not red
  if(sum(y_goals_axis$GA) == 0){
    goals_visual <- goals_visual + scale_fill_manual(values=c("#7F7F7F", "#7F7FFF"))
    
  }
  
  #if the player hasn't been on for a goal
  if(sum(y_goals_axis$GF) + sum(y_goals_axis$GA_abs) == 0) {
    
    goals_visual <- ggplot(empty_goals_2[order(empty_goals_2$GoalType),],
                           aes(x = GameNumber, y = Goals, fill = GoalType)) +
      geom_bar(stat = "identity") +
      labs(subtitle = paste('5v5 Goals For and Against \n',
                            0, ' GF  -  ',
                            0, ' GA ',
                            #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
                            sep = '')) +
      labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@SvengelskaHky'), sep = '') +
      theme_few() +
      theme(text = element_text(family = "Sweden Sans")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      #red, black
      scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
      scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) +
      theme(text = element_text(size = 9.5))
    
  }
  
  #aliases for each of the visuals
  g2 <- ggplotGrob(toi_visual)
  g3 <- ggplotGrob(corsi_visual)
  g4 <- ggplotGrob(goals_visual)
  g5 <- ggplotGrob(points_visual)
  #old method - this broke after updating corsi visual to rolling average
  g <- rbind(g2, g5, g3, g4, size = "first")
  g$widths <- unit.pmax(g2$widths, g5$widths, g3$widths, g4$widths)
  grid.newpage()
  grid.draw(g)
  
  #fig <- ggplotly(g)
  
  #use cowplot to align the plots now
  #g <- cowplot::plot_grid(g2, g5, g3, g4, ncol = 1, align = 'v', axis = 'lr')
  
  return(g)
  #return(fig)
  
}

player_card_plotly <- function(player_name, season_year) {

  teams <- player_toi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
    select(swehockey_name, shlse_team_name, game_number) %>%
    left_join(shl_team_dictionary, by = c("shlse_team_name" = "shlse_team_name"))

  teams <- teams[order(teams$game_number),]
  teams_list <- unique(teams$shlse_team_name)

  player_team <- paste0(player_name, "$", teams$swehockey_team_name[1]) %>%
    as.character()

  #TOI
  #create one-off table to expand y axis as needed - will use this later
  y_toi_axis <- player_toi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year)

  #pull relevant data from master
  table_toi <- player_toi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
    select(game_number, ESTOI, PPTOI, SHTOI) %>%
    #rename it for what displays in the legend
    rename(EV = ESTOI, PP = PPTOI, SH = SHTOI)

  #melt so can plot a stacked column (only way to create stacked column?)
  table_toi2 <- melt(table_toi, id.var="game_number") %>%
    rename(GameNumber = game_number, Situation = variable, TOI = value)

  #create the order of those stacked components - don't fully understand why this is necessary
  levels(table_toi2$Situation)
  table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))

  a <- list(
    autotick = FALSE,
    tick0 = 0,
    dtick = 1
  )

  sw_sans <- list(
    family = "Sweden Sans",
    size = 10
  )

  #fig <- plot_ly(table_toi, x = ~game_number, y = ~EV, type = 'bar', name = 'EV', color = "#7F7FFF")
  fig <- plot_ly(table_toi, x = ~game_number, y = ~EV, type = 'bar', name = 'EV', color = "rbga(127,127,255)")
  #fig <- plot_ly(table_toi, x = seq(1, 52, by = 1), y = ~EV, type = 'bar', name = 'EV', color = "#FFD17F")
  #fig <- fig %>% add_trace(y = ~PP, name = 'PP', color = "rbga(255,127,127)")
  fig <- fig %>% add_trace(y = ~PP, name = 'PP', color = "#FF7F7F")
  fig <- fig %>% add_trace(y = ~SH, name = 'SH', color = "rbga(255,209,127)")
  fig <- fig %>% layout(yaxis = list(title = 'TOI'), barmode = 'stack', font = sw_sans)
  fig <- fig %>% layout(xaxis = a, font = sw_sans)
  fig <- fig %>% layout(title = paste0("Marek Hrivik 20/21 Season | ", teams_list[1], " | ", NROW(table_toi$EV), " GP"))

  fig

}

team_card <- function(team, season_year) {
  
  #TOI by game situation
  
  # pp_toi <- team_pp_data_1920 %>%
  #   filter(swehockey_team_name == team) %>%
  #   select(game_number, TOI_PP)
  
  team_name <- team_pp_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    select(shlse_team_name) %>%
    unique()
  
  pp_toi <- team_pp_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    select(game_number, TOI_PP)
  
  pk_toi <- team_pk_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    select(game_number, TOI_SH)
  
  toi_total <- merge(pp_toi, pk_toi)
  
  toi_total <- toi_total %>%
    mutate(TOI_ES = 60 - TOI_PP - TOI_SH) %>%
    rename(EV = TOI_ES, PP = TOI_PP, SH = TOI_SH) %>%
    select(game_number, EV, PP, SH) #re-orders the columns
  
  toi_table <- toi_total #need a clean table to do calculations on for subtitle
  
  toi_total <- melt(toi_total, id.var="game_number") %>%
    rename(GameNumber = game_number, Situation = variable, TOI = value)
  
  levels(toi_total$Situation)
  toi_total$Situation <- factor(toi_total$Situation, levels = rev(levels(toi_total$Situation)))
  
  #ggplot code
  toi_visual <- ggplot(toi_total[order(toi_total$Situation),],
                       aes(x = GameNumber, y = TOI, fill = Situation)) +
    
    geom_bar(stat = "identity") +
    
    labs(title = paste(team_name, season_year, "Season", "|", NROW(toi_table$EV), 'GP', sep = ' '),
         
         subtitle = paste("TOI by Game Situation \n",
                          round(sum(toi_table$EV)/NROW(toi_table$EV), 1), ' EV  -  ',
                          round(sum(toi_table$PP)/NROW(toi_table$EV), 1), ' PP  -  ',
                          round(sum(toi_table$SH)/NROW(toi_table$EV), 1), ' SH', sep = '')) +
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(plot.title = element_text(face = "bold")) +
    theme(axis.title.x = element_blank()) +
    scale_fill_manual(values=c("#FFD17F", "#FF7F7F","#7F7FFF")) +
    #x axis spans entire 52 game season
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    #default y axis goes from 0 to 20 minutes
    scale_y_continuous(limits = c(0, 60.5), expand = c(0, 0)) +
    theme(text = element_text(size = 9.5))
  
  GP <- NROW(toi_table$EV)
  
  #5v5 Goals
  game_number <- tibble(c(1:52)) %>% set_names('game_number')
  
  GF <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation == '5v5') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(GF = sum(count))
  
  GA <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation == '5v5') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(GA = sum(count))
  
  y_goals_axis <- merge(game_number, GF, by.x = 'game_number', by.y = 'gf_team_game_number', all.x = TRUE)
  y_goals_axis <- merge(y_goals_axis, GA, by.x = 'game_number', by.y = 'ga_team_game_number', all.x = TRUE)
  y_goals_axis[is.na(y_goals_axis)] <- 0
  
  goals_calc <- y_goals_axis #need a clean table for subtitle calculation
  
  #make GA a negative number
  y_goals_axis <- y_goals_axis %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2) %>%
    mutate(GA_abs = abs(GA)) #in case it's needed to expand the axis later
  
  goals_table <- y_goals_axis %>%
    select(-c(GA_abs))
  
  goals_table <- melt(goals_table, id.var = "game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)
  
  levels(goals_table$GoalType)
  goals_table$GoalType <- factor(goals_table$GoalType, levels = rev(levels(goals_table$GoalType)))
  
  
  #ggplot code
  goals_visual <- ggplot(goals_table[order(goals_table$GoalType),],
                         aes(x = GameNumber, y = Goals, fill = GoalType)) +
    
    geom_bar(stat = "identity") +
    
    labs(subtitle = paste('5v5 Goals For and Against \n',
                          sum(goals_calc$GF), ' GF  -  ',
                          sum(goals_calc$GA), ' GA ',
                          ' (',
                          round((sum(goals_calc$GF)/(sum(goals_calc$GF) +  sum(goals_calc$GA)) * 100), 1), ' GF%',
                          ') ',
                          sep = '')) +
    
    #labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_swe'), sep = '') +
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(axis.title.x = element_blank()) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-5, 5), expand = c(0, 0), breaks = c(-5,-4,-3,-2,-1,0,1,2,3,4,5)) +
    #scale_y_continuous(limits = c(-5, 5), expand = c(0, 0), breaks = seq(-5,5, len = 1)) +
    theme(text = element_text(size = 9.5))
  
  # if the axis needs to be expanded
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 5){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
    
  }
  
  
  #Shot Rates
  corsi_data <- team_corsi_data_master_team %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    select(game_number, CF, CA)
  
  team_corsi <- round((sum(corsi_data$CF) / (sum(corsi_data$CF) + sum(corsi_data$CA)) *100), 1)
  
  corsi_data <- left_join(corsi_data, toi_table, by = c("game_number")) %>%
    select(game_number, CF, CA, EV) %>%
    
    mutate(CFL5 = ifelse(game_number >= 5,
                         rollsumr(CF, 5, fill = NA),
                         0)) %>%
    mutate(CAL5 = ifelse(game_number >= 5,
                         rollsumr(CA, 5, fill = NA),
                         0)) %>%
    mutate(ESTOIL5 = ifelse(game_number >= 5,
                            rollsumr(EV, 5, fill = NA),
                            0)) %>%
    
    mutate(CF60 = round((CFL5 / ESTOIL5) * 60, 1)) %>%
    mutate(CA60 = round((CAL5 / ESTOIL5) * 60, 1))
  
  CF60 <- round(sum(corsi_data$CF) / sum(corsi_data$EV) * 60, 1)
  CA60 <- round(sum(corsi_data$CA) / sum(corsi_data$EV) * 60, 1)
  
  corsi_condensed <- corsi_data %>%
    select(game_number, CF60, CA60)
  
  corsi_condensed <- reshape2::melt(corsi_condensed, id.var = "game_number") %>%
    rename(ShotType = variable, Shots = value)
  
  corsi_visual <- ggplot(corsi_condensed, aes(x = game_number, y = Shots, color = `ShotType`)) +
    geom_line(size = 1.25) +
    #geom_line(aes(y = 53.7), color = "#B2B2FF", size = 0.66, linetype = 'dotted') +
    
    labs(subtitle = paste("Shot Rates Per 60, 5-Game Moving Avg. \n",
                          # 
                          # CF60, ' CF60 - ',
                          # CA60, ' CA60 (',
                          team_corsi,
                          ' CF%',
                          sep = '')) +
    
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_color_manual(values=c("#7F7F7F", "#FE7F7E")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(30,70), expand = c(0, 0)) +
    theme(axis.title.x = element_blank()) +
    theme(text = element_text(size = 9.5))
  
  
  #Shooting & Save Percentage
  game_number2 <- tibble(c(1:52)) %>% set_names('game_number')
  
  #game_sits_include <- c("EV", "PP", "SH", "Goalie Pulled")
  #game_sits_exclude <- c("ENG", "SO", "PS")
  
  SF <- team_shotsfor_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    select(game_number, SOG)
  
  SA <- team_shotsagainst_data_master %>%
    filter(swehockey_team_name == team, season == season_year) %>%
    select(game_number, SOGA)
  
  #Goals For
  EVG <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation_general == 'EV') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(EVG = sum(count))
  
  PPG <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation_general == 'PP') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(PPG = sum(count))
  
  SHG <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation_general == 'SH') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(SHG = sum(count))
  
  GPG <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation_general == 'Goalie Pulled') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(GPG = sum(count))
  
  ENG <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation_general == 'ENG') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(ENG = sum(count))
  
  `S/OG` <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation_general == 'SO') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(`S/OG` = sum(count))
  
  PSG <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation_general == 'PS') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(PSG = sum(count))
  
  #Goals Against
  EVGA <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation_general == 'EV') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(EVGA = sum(count))
  
  PPGA <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation_general == 'PP') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(PPGA = sum(count))
  
  SHGA <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation_general == 'SH') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(SHGA = sum(count))
  
  GPGA <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation_general == 'Goalie Pulled') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(GPGA = sum(count))
  
  ENGA <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation_general == 'ENG') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(ENGA = sum(count))
  
  `S/OGA` <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation_general == 'SO') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(`S/OGA` = sum(count))
  
  PSGA <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation_general == 'PS') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(PSGA = sum(count))
  
  
  goals <- merge(game_number2, EVG, by.x = 'game_number', by.y = 'gf_team_game_number', all.x = TRUE)
  goals <- merge(goals, PPG, by.x = 'game_number', by.y = 'gf_team_game_number', all.x = TRUE)
  goals <- merge(goals, SHG, by.x = 'game_number', by.y = 'gf_team_game_number', all.x = TRUE)
  goals <- merge(goals, GPG, by.x = 'game_number', by.y = 'gf_team_game_number', all.x = TRUE)
  goals <- merge(goals, ENG, by.x = 'game_number', by.y = 'gf_team_game_number', all.x = TRUE)
  goals <- merge(goals, `S/OG`, by.x = 'game_number', by.y = 'gf_team_game_number', all.x = TRUE)
  goals <- merge(goals, PSG, by.x = 'game_number', by.y = 'gf_team_game_number', all.x = TRUE)

  goals <- merge(goals, EVGA, by.x = 'game_number', by.y = 'ga_team_game_number', all.x = TRUE)
  goals <- merge(goals, PPGA, by.x = 'game_number', by.y = 'ga_team_game_number', all.x = TRUE)
  goals <- merge(goals, SHGA, by.x = 'game_number', by.y = 'ga_team_game_number', all.x = TRUE)
  goals <- merge(goals, GPGA, by.x = 'game_number', by.y = 'ga_team_game_number', all.x = TRUE)
  goals <- merge(goals, ENGA, by.x = 'game_number', by.y = 'ga_team_game_number', all.x = TRUE)
  goals <- merge(goals, `S/OGA`, by.x = 'game_number', by.y = 'ga_team_game_number', all.x = TRUE)
  goals <- merge(goals, PSGA, by.x = 'game_number', by.y = 'ga_team_game_number', all.x = TRUE)

  goals <- merge(goals, SF, by.x = 'game_number', by.y = 'game_number', all.x = TRUE)
  goals <- merge(goals, SA, by.x = 'game_number', by.y = 'game_number', all.x = TRUE)

  goals[is.na(goals)] <- 0
  
  goals <- goals %>%
    mutate(GF_include = EVG + PPG + SHG + GPG,
           GF_exclude = ENG + `S/OG` + PSG,
           GA_include = EVGA + PPGA + SHGA + GPGA,
           GA_exclude = ENG + `S/OGA` + PSGA,
           SOG_include = SOG - GF_exclude,
           SOGA_include = SOGA - GA_exclude) %>%
    select(game_number, GF_include, SOG_include, GA_include, SOGA_include) %>%
    filter(game_number <= GP) #prevent the lines from wandering into the future
  
  sp_data <- goals %>%
    rename(GF = GF_include, SOG = SOG_include, GA = GA_include, SOGA = SOGA_include) %>%

    mutate(GFL5 = ifelse(game_number >= 5,
                         rollsumr(GF, 5, fill = NA), 0)) %>%
    mutate(SOGL5 = ifelse(game_number >= 5,
                          rollsumr(SOG, 5, fill = NA), 0)) %>%

    mutate(GAL5 = ifelse(game_number >= 5,
                         rollsumr(GA, 5, fill = NA), 0)) %>%
    mutate(SOGAL5 = ifelse(game_number >= 5,
                          rollsumr(SOGA, 5, fill = NA), 0)) %>%
    
    mutate(`Sh%` = round(GFL5 / SOGL5, 3) * 100) %>%
    mutate(`Opp Sh%` = round(GAL5 / SOGAL5, 3) * 100)
  
  `Sh%` <- round(sum(sp_data$GF) / sum(sp_data$SOG), 3) * 100
  `Sv%` <- 100 - (round(sum(sp_data$GA) / sum(sp_data$SOGA), 3) * 100)
  
  sp_data_condensed <- sp_data %>%
    select(game_number, `Sh%`, `Opp Sh%`)
  
  sp_data_condensed <- reshape2::melt(sp_data_condensed, id.var = "game_number") %>%
    rename(Team = variable, `Sh%` = value)
  
  sh_visual <- ggplot(sp_data_condensed, aes(x = game_number, y = `Sh%`, color = Team)) +
    geom_line(size = 1.25) +
    
    labs(subtitle = paste("All Sits. Shooting Percentages, 5-Game Moving Avg. \n",
                          
                          
                          
                          `Sh%`, " Sh%,",
                          " ", `Sv%`, " Sv%",
                          sep = '')) +
    
    labs(caption = 'All situations included, with empty net goals & penalty shot/shootout goals removed.') +
    
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_color_manual(values=c("#7F7F7F", "#FE7F7E")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,20), expand = c(0, 0)) +
    theme(axis.title.x = element_blank()) +
    theme(text = element_text(size = 9.5))
  
  #Special Teams Goals
  game_number3 <- tibble(c(1:52)) %>% set_names('game_number')

  PPG2 <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation_general == 'PP') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(PPG = sum(count))

  SHG2 <- box_score_data_master %>%
    filter(gf_team == team, season == season_year, game_situation_general == 'SH') %>%
    select(gf_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(SHG = sum(count))

  PPGA2 <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation_general == 'PP') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(PPGA = sum(count))

  SHGA2 <- box_score_data_master %>%
    filter(ga_team == team, season == season_year, game_situation_general == 'SH') %>%
    select(ga_team_game_number, goal) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(ga_team_game_number) %>%
    summarise(SHGA = sum(count))

  st_y_goals_axis <- merge(game_number3, PPG2, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  st_y_goals_axis <- merge(st_y_goals_axis, SHG2, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  st_y_goals_axis <- merge(st_y_goals_axis, PPGA2, by.x = "game_number", by.y = "ga_team_game_number", all.x = TRUE)
  st_y_goals_axis <- merge(st_y_goals_axis, SHGA2, by.x = "game_number", by.y = "ga_team_game_number", all.x = TRUE)
  
  st_y_goals_axis[is.na(st_y_goals_axis)] <- 0
  
  st_goals_calc <- st_y_goals_axis
  
  st_y_goals_axis <- st_y_goals_axis %>%
    mutate(PPGA2 = PPGA * -1,
           SHGA2 = SHGA * -1) %>%
    
    select(-c(PPGA, SHGA)) %>%
    
    rename(PPGA = PPGA2,
           SHGA = SHGA2) %>%
    
    mutate(GF = PPG + SHG,
           GA = (PPGA + SHGA) * -1) %>%
    mutate(GA_abs = abs(GA))
  
  ST_GF <- sum(st_y_goals_axis$PPG) + sum(st_y_goals_axis$SHG)
  ST_GA <- (sum(st_y_goals_axis$PPGA) + sum(st_y_goals_axis$SHGA)) * -1
  
  st_goals_table <- st_y_goals_axis %>%
    select(-c(GF, GA, GA_abs))
  
  st_goals_table <- reshape2::melt(st_goals_table, id.var = "game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)

  levels(st_goals_table$GoalType)
  st_goals_table$GoalType <- factor(st_goals_table$GoalType, levels = rev(levels(st_goals_table$GoalType)))
  
  st_goals_visual <- ggplot(st_goals_table[order(st_goals_table$GoalType),],
                            aes(x = GameNumber, y = Goals, fill = GoalType)) +
    
    geom_bar(stat = "identity") +

    labs(subtitle = paste('Special Teams Goals For and Against \n',
                          ST_GF, " GF - ",
                          ST_GA, " GA",
                          sep = ''

               )) +

    labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_swe'), sep = '') +

    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #shga, ppga, shg, ppg
    scale_fill_manual(values=c("#b2b2ff", "#7F7FFF", "#99D999", "#7fbf7f")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0), breaks = c(-3,-2,-1,0,1,2,3)) +
    theme(text = element_text(size = 9.5))
  
  if(max(st_y_goals_axis$GF, st_y_goals_axis$GA_abs) > 3){
    st_goals_visual <- st_goals_visual + scale_y_continuous(limits=c(max(st_y_goals_axis$GF, st_y_goals_axis$GA_abs) * -1, max(st_y_goals_axis$GF,st_y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))

  }
  
  
  #aliases for each of the visuals
  g2 <- ggplotGrob(toi_visual)
  g3 <- ggplotGrob(corsi_visual)
  g4 <- ggplotGrob(goals_visual)
  g5 <- ggplotGrob(sh_visual)
  g6 <- ggplotGrob(st_goals_visual)
  #g5 <- ggplotGrob(points_visual)
  #old method - this broke after updating corsi visual to rolling average
  g <- rbind(g2, g3, g5, g4, g6, size = "first")
  g$widths <- unit.pmax(g2$widths, g3$widths, g5$widths, g4$widths, g6$widths)
  grid.newpage()
  grid.draw(g)
  
  return(g)
  #return(goals)
  #return(sp_data)
  #return(goals_for)
  #return(team_name)
  #return(st_y_goals_axis)
  #return(st_goals_visual)
  #return(st_goals)
  
}

team_toi <- function(team, season_year, position, situation) {
  
  #kelly_colors <- c('#F2F3F4', '#222222', '#F3C300', '#875692', '#F38400', '#A1CAF1', '#BE0032', '#C2B280', '#848482', '#008856', '#E68FAC', '#0067A5', '#F99379', '#604E97', '#F6A600', '#B3446C', '#DCD300', '#882D17', '#8DB600', '#654522', '#E25822', '#2B3D26')
  
  kelly_colors <- c('#222222', '#F3C300', '#875692', '#F38400', '#A1CAF1', '#BE0032', '#C2B280', '#848482', '#008856', '#E68FAC', '#0067A5', '#F99379', '#604E97', '#F6A600', '#B3446C', '#DCD300', '#882D17', '#8DB600', '#654522', '#E25822', '#2B3D26')
  
  toi_data <- player_toi_data_master %>%
    filter(team == shlse_team_name, season_year == season, position == Pos) %>%
    rename(player = swehockey_name)
  
  #set a minimum games played
  min_gp <- toi_data %>%
    group_by(player) %>%
    dplyr::summarise(gp = sum(GP), toi2 = sum(TOI)) %>%
    filter(gp >= 5) %>%
    arrange(-toi2)
  
  min_gp2 <- toi_data %>%
    group_by(player) %>%
    dplyr::summarise(gp = sum(GP), toi2 = sum(TOI)) %>%
    filter(gp >= 5) %>%
    arrange(-toi2) %>%
    select(player)
  
  if (situation == 'EV') {
    toi_data <- toi_data %>%
      select(player, game_number, ESTOI) %>%
      rename(TOI = ESTOI)
    
    situation_title <- 'Even Strength'
  }
  
  if (situation == 'PP') {
    toi_data <- toi_data %>%
      select(player, game_number, PPTOI) %>%
      rename(TOI = PPTOI)
    
    situation_title <- 'Powerplay'
  }
  
  if (situation == 'SH') {
    toi_data <- toi_data %>%
      select(player, game_number, SHTOI) %>%
      rename(TOI = SHTOI)
    
    situation_title <- 'Shorthanded'
  }
  
  if (situation == 'All') {
    toi_data <- toi_data %>%
      select(player, game_number, TOI) 
    
    situation_title <- 'All Situations'
  }
  
  if (position == 'F') {
    position_title <- 'Forwards'
  }
  
  if (position == 'D') {
    position_title <- 'Defensemen'
  }
  
  #players <- unique(toi_data$player) %>% sort()
  #players <- unique(min_gp$player) %>% sort()
  #players <- unique(min_gp$player) 
  players <- min_gp2$player
  
  n <- length(players)
  
  player_toi <- c()
  for (i in players[1:n]) {
    
    game_number <- tibble(c(1:52)) %>% set_names('game_number')
    
    if(season_year == '20/21') {
      
      game_number <- tibble(c(1:13)) %>% set_names('game_number')
      
      
    }
    
    temp <- toi_data %>%
      filter(player == i) %>%
      mutate(player_game_number = row_number())
    
    temp <- temp %>%
      mutate(TOI_L5 = ifelse(player_game_number >= 5, rollsumr(TOI, 5, fill = NA), NA)) %>%
      mutate(TOI_avg = round((TOI_L5 / 5), 1))
    
    #temp <- left_join(game_number, temp, by = "game_number")
    #temp <- left_join(temp, game_number, by = "game_number")
    temp <- merge(game_number, temp, by.x = "game_number", by.y = "game_number", all.x = TRUE)
    
    temp$player <- ifelse(is.na(temp$player), i, i)
    
    #new 
    temp <- temp %>%
      select(player, game_number, TOI) %>%
      rename(GameNumber = game_number)
      #rename(TOI = TOI_avg, `Game Number` = game_number) 
    
    player_toi <- rbind(player_toi, temp)
    
    rm(temp)
    
  }
  
  #Function for smoothing
  myfunsmooth <- function(x)
  {
    #Model
    model <- loess(TOI ~ GameNumber, data = x)
    #Augment model output in a new dataframe
    y <- broom::augment(model, x)
    #Merge data
    z <- merge(x,y[,c("player","GameNumber",".fitted")],
               by=c("player","GameNumber"),all.x = T)
    #Return
    return(z)
  }
  
  #Create list by player
  list <- split(player_toi,player_toi$player)
  
  #Apply function
  list2 <- lapply(list, myfunsmooth)
  #Bind all
  toi_data_smoothed <- do.call(rbind,list2)
  rownames(toi_data_smoothed) <- NULL
  
  
  toi_visual <- ggplot(toi_data_smoothed, aes(x = GameNumber, y = TOI, group = player, colour = player)) +
    geom_line(aes(y = .fitted), size = 1) +
    scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) +
    
    scale_color_manual(values = kelly_colors) +
    
      theme_few() +
      theme(text = element_text(family = "Sweden Sans"),
            legend.title = element_blank()) +
      #theme(legend.title = element_blank())


      labs(title = paste(team, season_year, position_title, "|", "Smoothed TOI | ", situation_title, sep = ' ')) +
      #labs(title = paste(team, season_year, position_title, "|", "Smoothed TOI", sep = ' '),
           #subtitle = paste(situation_title, "(Min. 10 GP)", sep = ' ')) +

      theme(plot.title = element_text(face = "bold")) 
    
      #theme(text = element_text(size = 9))
  
  
  # if(max(toi_data$TOI) > 26) {
  #   
  #   toi_visual <- toi_visual + 
  #     scale_y_continuous(limits = c(0, 30), expand = c(0, 0))
  #   
  # }
  
  if(position == 'F' && situation == 'All') {
    toi_visual <- toi_visual + 
      scale_y_continuous(limits = c(0, 26), expand = c(0, 0))
  }
  
  if(position == 'D' && situation == 'EV') {
    toi_visual <- toi_visual + 
      scale_y_continuous(limits = c(0, 26), expand = c(0, 0))
  }
  
  if(situation == 'EV' && position == 'F') {
    
    toi_visual <- toi_visual + 
      scale_y_continuous(limits = c(0, 21), expand = c(0, 0))
    
  }
  
  if(situation == 'PP') {
    
    toi_visual <- toi_visual + 
      scale_y_continuous(limits = c(0, 5), expand = c(0, 0))
    
  }
  
  if(situation == 'SH') {
    
    toi_visual <- toi_visual + 
      scale_y_continuous(limits = c(0, 5), expand = c(0, 0))
    
  }
                                  
  
  # toi_visual <- ggplot(df3, aes(x = `Game Number`, y = TOI, group = player, colour = player)) +
  # toi_visual <- ggplot(player_toi_condensed, aes(x = `Game Number`, y = TOI, group = player, colour = player)) +
  # toi_visual <- ggplot(player_toi, aes(x = `Game Number`, y = TOI, group = player, colour = player)) +
  # toi_visual <- ggplot(player_toi, aes(x = game_number, y = TOI_avg, group = player, colour = player)) +

  #plotly_toi_visual <- ggplotly(toi_visual)
  

  return(toi_visual)
  #return(plotly_toi_visual)
  
}

ha_player_card <- function(player_name, season_year) {
  
  #Player Info
  player_info <- ha_player_db %>%
    filter(swehockey == player_name)
  
  team <- player_info$team_name
  position <- player_info$position
  
  #Total Games Played
  GP <- ha_player_game_logs_2021 %>%
    dplyr::filter(player == player_name) %>%
    nrow()
  
  # #TOI
  # toi_game_number <- tibble(c(1:52)) %>% set_names("game_number")
  # 
  # toi_data <- ha_player_game_logs_2021 %>%
  #   dplyr::filter(player == player_name) %>%
  #   dplyr::select(game_number, toi) %>%
  #   dplyr::rename(GameNumber = game_number, TOI = toi)
  # 
  # #toi_data <- left_join(toi_game_number, toi_data, by = c("game_number" = "game_number")) %>%
  #   #dplyr::rename(GameNumber = game_number, TOI = toi)
  # 
  # toi_data[is.na(toi_data)] <- 0
  # 
  # #new
  # toi_data <- toi_data %>%
  #   mutate(Situation = "All Sits")
  # 
  # #toi_data$TOI <- as.numeric(as.character(toi_data$TOI))
  # 
  # toi_data2 <- melt(toi_data, id.var="GameNumber") %>%
  #   rename(Situation = variable, TOI = value)
  # 
  # levels(toi_data2$Situation)
  # toi_data2$Situation <- factor(toi_data2$Situation, levels = rev(levels(toi_data2$Situation)))
  # 
  # # toi_visual <- ggplot(toi_data2[order(toi_data2$Situation),],
  # #                      aes(x = GameNumber, y = TOI, fill = Situation)) +
  # #   geom_bar(stat = "identity") +
  # 
  # toi_visual <- ggplot(toi_data, aes(x = GameNumber, y = TOI)) +
  #   geom_bar(stat = "identity", fill = "#7F7FFF") +
  #   #geom_bar(stat = "identity", aes(color = "#7F7FFF")) +
  # 
  #   labs(title = paste0(player_name, " ", season_year, " Season | ", NROW(toi_data$TOI), " GP"),
  # 
  #        subtitle = paste0("ATOI: ", round(sum(toi_data$TOI)/ GP , 1))
  #        ) +
  # 
  #   theme_few() +
  #   theme(text = element_text(family = "Sweden Sans")) +
  #   theme(plot.title = element_text(face = "bold")) +
  #   theme(axis.title.x = element_blank()) +
  #   #theme(legend.position = "right") +
  #   #scale_fill_manual(values=c("#7F7FFF")) +
  # 
  #   scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
  #   scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
  #   theme(text = element_text(size = 9))
  # 
  # if(max(toi_data$TOI > 20)) {
  #   toi_visual <- toi_visual + scale_y_continuous(limits = c(0, max(toi_data$TOI) + 1), expand = c(0, 0))
  # }
  
  #TOI
  toi_data <- ha_player_game_logs_2021 %>%
    dplyr::filter(player == player_name) %>%
    dplyr::filter(season == '2020-2021') %>%
    dplyr::select(game_number, toi) %>%
    dplyr::rename(GameNumber = game_number, TOI = toi)
  
  toi_data2 <- melt(toi_data, id.var="GameNumber") %>%
    rename(`All Sits` = variable, TOI = value)
  
  levels(toi_data2$`All Sits`)
  toi_data2$`All Sits` <- factor(toi_data2$`All Sits`, levels = rev(levels(toi_data2$`All Sits`)))
  
  toi_visual <- ggplot(toi_data2[order(toi_data2$`All Sits`),],
                       aes(x = GameNumber, y = TOI, fill = `All Sits`)) +
    geom_bar(stat = "identity") +
    
    labs(title = paste0(player_name, " ", season_year, " Season | ", team, " | ", NROW(toi_data$TOI), " GP"),

         subtitle = paste0("All Situations TOI \nATOI: ", round(sum(toi_data$TOI)/ GP , 1))
         ) +

    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(plot.title = element_text(face = "bold")) +
    theme(axis.title.x = element_blank()) +
    #theme(legend.position = "right") +
    #scale_fill_manual(values=c("#7F7FFF")) +
    #scale_fill_manual(values=c("#2a9d8f")) +
    scale_fill_manual(values=c("#457b9d")) +
    
    
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
    theme(text = element_text(size = 9))

  if(max(toi_data$TOI > 20)) {
    toi_visual <- toi_visual + scale_y_continuous(limits = c(0, max(toi_data$TOI) + 1), expand = c(0, 0))
  }
  
  #POINTS
  game_number <- tibble(c(1:52)) %>% set_names("game_number")

  EVG_data <- ha_box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'EV') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_EV = sum(count))

  EVA1_data <- ha_box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'EV') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_EV = sum(count))

  EVA2_data <- ha_box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'EV') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_EV = sum(count))

  PPG_data <- ha_box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PP') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_PP = sum(count))

  PPA1_data <- ha_box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PP') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_PP = sum(count))

  PPA2_data <- ha_box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PP') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_PP = sum(count))

  SHG_data <- ha_box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SH') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_SH = sum(count))

  SHA1_data <- ha_box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SH') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_SH = sum(count))

  SHA2_data <- ha_box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SH') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_SH = sum(count))

  PSG_data <- ha_box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'PS') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_PS = sum(count))

  SOG_data <- ha_box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'SO') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_SO = sum(count))

  ENG_data <- ha_box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'ENG') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_EN = sum(count))

  ENA1_data <- ha_box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'ENG') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_EN = sum(count))

  ENA2_data <- ha_box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'ENG') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_EN = sum(count))

  GPG_data <- ha_box_score_data_master %>%
    filter(goal == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(G_GP = sum(count))

  GPA1_data <- ha_box_score_data_master %>%
    filter(primary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A1_GP = sum(count))

  GPA2_data <- ha_box_score_data_master %>%
    filter(secondary_assist == player_name) %>%
    filter(season == season_year) %>%
    filter(game_situation_general == 'Goalie Pulled') %>%
    select(goal, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(goal)) %>%
    group_by(gf_team_game_number) %>%
    summarise(A2_GP = sum(count))

  points_master <- left_join(game_number, EVG_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, EVA1_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, EVA2_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, PPG_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, PPA1_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, PPA2_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, SHG_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, SHA1_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, SHA2_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, PSG_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, SOG_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, ENG_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, ENA1_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, ENA2_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, GPG_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, GPA1_data, by = c("game_number" = "gf_team_game_number"))
  points_master <- left_join(points_master, GPA2_data, by = c("game_number" = "gf_team_game_number"))

  points_master[is.na(points_master)] <- 0

  points_master <- points_master %>%
    mutate(A_EV = A1_EV + A2_EV,
           A_PP = A1_PP + A2_PP + A1_GP + A2_GP,
           G_PP = G_PP + G_GP,
           A_SH = A1_SH + A2_SH,
           G_PSSO = G_PS + G_SO,
           A_EN = A1_EN + A2_EN)

  table_points <- points_master %>%
    select(game_number, G_EV, G_PP, G_SH, G_EN, G_PSSO, A_EV, A_PP, A_SH, A_EN) %>%
    #rename(game_number = number) %>%
    rename(EV_G = G_EV,
           EV_A = A_EV,
           PP_G = G_PP,
           EN_G = G_EN,
           PP_A = A_PP,
           SH_G = G_SH,
           SH_A = A_SH,
           EN_A = A_EN,
           PS_G = G_PSSO)

  table_points_2 <- melt(table_points, id.var="game_number") %>%
    rename(GameNumber = game_number, PointType = variable, Points = value)

  levels(table_points_2$PointType)
  table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))

  table_points_2$PointType <- factor(table_points_2$PointType, levels=c("EN_A", "SH_A", "PP_A", "EV_A",
                                                                        "PS_G", "EN_G", "SH_G", "PP_G", "EV_G"))

  es_g_hex <- "#7f7fff"
  es_a_hex <- "#b2b2ff"
  pp_g_hex <- "#ff7f7f"
  pp_a_hex <- "#ffb3b3"
  sh_g_hex <- "#ffd17f"
  #sh_g_hex <- "#ffb433"
  sh_a_hex <- "#ffe3b3"
  #sh_a_hex <- "#FFE766"
  ps_g_hex <- "#abd3ab"
  en_g_hex <- "#ca80ff"
  en_a_hex <- "#FDB3FF"

  #GGPLOT VISUAL
  points_visual <- ggplot(table_points_2[order(table_points_2$PointType),],
                          aes(x = GameNumber, y = Points, fill = PointType)) +
    geom_bar(stat = "identity") +

    labs(subtitle = paste0("Point Production \n",
                           "EV: ",
                           sum(table_points$EV_G), ' G  -  ',
                           sum(table_points$EV_A), ' A  -  ',
                           (sum(table_points$EV_G) + sum(table_points$EV_A)), " P  |  ",

                           "PP: ",
                           sum(table_points$PP_G), ' G  -  ',
                           sum(table_points$PP_A), ' A  -  ',
                           (sum(table_points$PP_G) + sum(table_points$PP_A)), ' P'

                           )) +

    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    theme(axis.title.x = element_blank()) +

    scale_fill_manual(values=c(en_a_hex, sh_a_hex, pp_a_hex, es_a_hex, ps_g_hex, en_g_hex, sh_g_hex, pp_g_hex, es_g_hex)) +

    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(text = element_text(size = 9))

  #GOALS FOR & AGAINST DATA
  game_numbers <- tibble(c(1:52)) %>% set_names("game_number")

  empty_goals <- tibble(game_number = c(1:52), GF = 0, GA = 0)

  empty_goals_2 <- melt(empty_goals, id.var = "game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)

  levels(empty_goals_2$GoalType)
  empty_goals_2$GoalType <- factor(empty_goals_2$GoalType, levels = rev(levels(empty_goals_2$GoalType)))

  GF_data <- ha_box_score_data_master %>%
    subset(grepl(player_name, GF_names)) %>%
    filter(season == season_year) %>%
    filter(game_situation == '5v5') %>%
    select(GF_names, gf_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(GF_names)) %>%
    group_by(gf_team_game_number) %>%
    summarise(GF = sum(count))

  GA_data <- ha_box_score_data_master %>%
    subset(grepl(player_name, GA_names)) %>%
    filter(season == season_year) %>%
    filter(game_situation == '5v5') %>%
    select(GA_names, ga_team_game_number) %>%
    mutate(count = 1) %>%
    select(-c(GA_names)) %>%
    group_by(ga_team_game_number) %>%
    summarise(GA = sum(count))

  y_goals_axis <- merge(game_numbers, GF_data, by.x = "game_number",
                        by.y = "gf_team_game_number", all.x = TRUE)

  y_goals_axis <- merge(y_goals_axis, GA_data, by.x = "game_number",
                        by.y = "ga_team_game_number", all.x = TRUE)

  y_goals_axis <- y_goals_axis %>%
    mutate(GA2 = GA * -1) %>%
    select(-c(GA)) %>%
    rename(GA = GA2) %>%
    mutate(GA_abs = abs(GA))

  table_goals <- y_goals_axis %>%
    select(-c(GA_abs))

  table_goals_2 <- melt(table_goals, id.var = "game_number") %>%
    rename(GameNumber = game_number, GoalType = variable, Goals = value)

  levels(table_goals_2$GoalType)
  table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))

  table_goals[is.na(table_goals)] <- 0
  y_goals_axis[is.na(y_goals_axis)] <- 0

  goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),],
                         aes(x = GameNumber, y = Goals, fill = GoalType)) +
    geom_bar(stat = "identity") +

    labs(subtitle = paste('5v5 Goals For and Against \n',
                          sum(table_goals$GF), ' GF  -  ',
                          sum(y_goals_axis$GA_abs), ' GA ', ' (',
                          round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)) * 100,1),
                          ' GF%)',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 1), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se, stats.swehockey.se, and eliteprospects.com', ' | ', '@zellenthal_swe'), sep = '') +

    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) +
    theme(text = element_text(size = 9))


  g2 <- ggplotGrob(toi_visual)
  g3 <- ggplotGrob(points_visual)
  g4 <- ggplotGrob(goals_visual)

  g <- rbind(g2, g3, g4, size = "first")
  g$widths <- unit.pmax(g2$widths, g3$widths, g4$widths)
  #g <- rbind(g3, g4, size = "first")
  #g$widths <- unit.pmax(g3$widths, g4$widths)
  grid.newpage()
  grid.draw(g)
  
  #return(max(toi_data$TOI))
  #return(toi_data2)
  #return(toi_data)
  #return(toi_visual)
  #return(player_info)
  return(g)
  
}

player_profile_5v5 <- function(player_name) {
  
  table <- `5v5` %>%
    filter(Player == player_name)
  
  #return(`5v5`)
  return(table)
  
}

player_profile_ev <- function(player_name) {
  
  table <- EV %>%
    filter(Player == player_name)

  return(table)
  
}

player_profile_pp <- function(player_name) {
  
  table <- PP %>%
    filter(Player == player_name)
  
  return(table)
  
}

player_profile_sh <- function(player_name) {
  
  table <- SH %>%
    filter(Player == player_name)
  
  return(table)
  
}

player_profile_all <- function(player_name) {
  
  table <- `All Sits` %>%
    filter(Player == player_name)
  
  return(table)
  
}

player_profile_eng <- function(player_name) {
  
  table <- `Empty Net` %>%
    filter(Player == player_name)
  
  return(table)
  
}

player_profile_psso <- function(player_name) {
  
  table <- `PS/SO` %>%
    filter(Player == player_name) %>%
    dplyr::select(Player, Season, Age, Team, G)
  
  return(table)
  
}

# player_profile_ev_other <- function(player_name) {
#   
#   table <- `Empty Net` %>%
#     filter(Player == player_name)
#   
#   return(table)
#   
# }

player_profile_bio1 <- function(player_name) {
  
  ep <- player_lookup_v2 %>%
    dplyr::filter(swehockey == player_name) %>%
    dplyr::distinct() %>%
    dplyr::select(ep) %>%
    .[,1]
  
  bio <- shl_playerdb_master %>%
    dplyr::filter(ep_name == ep) %>%
    dplyr::select(-c(season, team, gp, age, age2)) %>%
    dplyr::distinct()
  
  bio1 <- paste0(player_name, " | Pos: ", bio$pos)
  
  return(bio1)
  
}

player_profile_bio2 <- function(player_name) {
  
  ep <- player_lookup_v2 %>%
    dplyr::filter(swehockey == player_name) %>%
    dplyr::distinct() %>%
    dplyr::select(ep) %>%
    .[,1]
  
  bio <- shl_playerdb_master %>%
    dplyr::filter(ep_name == ep) %>%
    dplyr::select(-c(season, team, gp, age, age2)) %>%
    dplyr::distinct()
  
  today <- lubridate::today() %>% lubridate::as_date()
  
  dob <- lubridate::as_date(bio$dob)
  
  age <- round(lubridate::time_length(today - dob, "year"), 1)
  
  bio2 <- paste0("Nationality: ", bio$nationality, " | DOB: ", bio$dob, " | Age: ", age)
  
  return(bio2)
  
}

ha_player_profile_5v5 <- function(player_name) {
  
  table <- `5v5  ` %>%
    filter(Player == player_name)
  
  #return(`5v5`)
  return(table)
  
}

ha_player_profile_ev <- function(player_name) {
  
  table <- `EV  ` %>%
    filter(Player == player_name)
  
  return(table)
  
}

ha_player_profile_pp <- function(player_name) {
  
  table <- `PP  ` %>%
    filter(Player == player_name)
  
  return(table)
  
}

ha_player_profile_sh <- function(player_name) {
  
  table <- `SH  ` %>%
    filter(Player == player_name)
  
  return(table)
  
}

ha_player_profile_all <- function(player_name) {
  
  table <- `All Sits  ` %>%
    filter(Player == player_name)
  
  return(table)
  
}

ha_player_profile_eng <- function(player_name) {
  
  table <- `Empty Net  ` %>%
    filter(Player == player_name)
  
  return(table)
  
}

ha_player_profile_psso <- function(player_name) {
  
  table <- `PS/SO  ` %>%
    filter(Player == player_name) %>%
    dplyr::select(Player, Season, Age, Team, G)
  
  return(table)
  
}

ha_player_profile_bio1 <- function(player_name) {
  
  # ep <- player_lookup_v2 %>%
  #   dplyr::filter(swehockey == player_name) %>%
  #   dplyr::distinct() %>%
  #   dplyr::select(ep) %>%
  #   .[,1]
  
  bio <- ha_playerdb_v2 %>%
    dplyr::filter(swehockey == player_name) %>%
    dplyr::select(-c(season, team, gp, age, age2)) %>%
    dplyr::distinct()
  
  bio1 <- paste0(player_name, " | Pos: ", bio$pos[1])
  
  return(bio1)
  
}

ha_player_profile_bio2 <- function(player_name) {
  
  # ep <- player_lookup_v2 %>%
  #   dplyr::filter(swehockey == player_name) %>%
  #   dplyr::distinct() %>%
  #   dplyr::select(ep) %>%
  #   .[,1]
  
  bio <- ha_playerdb_v2 %>%
    dplyr::filter(swehockey == player_name) %>%
    dplyr::select(-c(season, team, gp, age, age2)) %>%
    dplyr::distinct()
  
  today <- lubridate::today() %>% lubridate::as_date()
  
  dob <- lubridate::as_date(bio$dob[1])
  
  age <- round(lubridate::time_length(today - dob, "year"), 1)
  
  bio2 <- paste0("Nationality: ", bio$nationality[1], " | DOB: ", dob, " | Age: ", age)
  
  return(bio2)
  #return(dob)
  
}

game_log <- function(player_name, season_year) {
  
  #toi
  toi <- player_toi_data_master %>%
    dplyr::filter(swehockey_name == player_name, season == season_year) %>%
    dplyr::select(swehockey_name, shlse_team_name, season, game_number, TOI, PPTOI, ESTOI, SHTOI, name_date, swehockey_team_name) %>%
    mutate(date = str_right(name_date, 10)) %>%
    mutate(team_date = paste0(swehockey_team_name, date)) %>%
    mutate(game = row_number()) %>%
    select(-name_date) %>%
    dplyr::rename(Player = swehockey_name, Team = shlse_team_name, Season = season, team_game = game_number,
                  Game = game, Date = date, TOI_EV = ESTOI, TOI_PP = PPTOI, TOI_SH = SHTOI) %>%
    dplyr::select(team_date, team_game, Date, Game, Player, Season, Team, TOI, TOI_EV, TOI_PP, TOI_SH)
  
  team_games_participated <- toi %>%
    select(team_game) %>% dplyr::distinct()
  
  #shots
  shots <- player_shots_data_master %>%
    dplyr::filter(swehockey_name == player_name, season == season_year) %>%
    dplyr::select(SOG, SOG_EV, SOG_PP)
    
  #corsi
  game_number_corsi <- tibble(c(team_games_participated$team_game)) %>% set_names('game_number')
  
  # corsi <- player_corsi_data_master %>%
  #   dplyr::filter(swehockey_name == player_name, season == season_year) %>%
  #   dplyr::select(game_number.x, CF, CA, CF._off) %>%
  #   dplyr::mutate(CF_team = CF._off * 100) %>%
  #   dplyr::mutate(`CF%` = (round((CF / (CF + CA)),3)) * 100) %>%
  #   dplyr::mutate(`CF% Rel` = round(`CF%` - CF_team, 1)) %>%
  #   dplyr::select(-c(CF._off, CF_team))
  # 
  # corsi <- merge(game_number_corsi, corsi, by.x = "game_number", by.y = "game_number.x", all.x = TRUE)
  # corsi$CF[is.na(corsi$CF)] <- 0
  # corsi$CA[is.na(corsi$CA)] <- 0
  # corsi <- corsi %>%
  #   dplyr::select(-game_number.x)
  
  corsi <- player_corsi_data_master %>%
    dplyr::filter(swehockey_name == player_name, season == season_year) %>%
    dplyr::select(game_number.x, CF, CA, CF._off)
  
  corsi <- merge(game_number_corsi, corsi, by.x = "game_number", by.y = "game_number.x", all.x = TRUE)
  corsi$CF[is.na(corsi$CF)] <- 0
  corsi$CA[is.na(corsi$CA)] <- 0
  corsi$CF._off[is.na(corsi$CF._off)] <- 0
  
  corsi <- corsi %>%
    dplyr::mutate(CF_team = CF._off * 100) %>%
    dplyr::mutate(`CF%` = (round((CF / (CF + CA)),3)) * 100) %>%
    dplyr::mutate(`CF% Rel` = round(`CF%` - CF_team, 1)) %>%
    dplyr::select(-c(game_number, CF._off, CF_team))
  
  corsi$`CF%`[is.na(corsi$`CF%`)] <- 0
  corsi$`CF% Rel`[is.na(corsi$`CF% Rel`)] <- 0
  
  #EV Scoring
  game_number_scoring <- tibble(c(team_games_participated$team_game)) %>% set_names('game_number')
  
  goals_ev <- box_score_data_master %>%
    dplyr::filter(goal == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation_general == 'EV') %>%
    dplyr::select(goal, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-c(goal)) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(G_EV = sum(count))
  
  scoring_ev <- merge(game_number_scoring, goals_ev, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  scoring_ev$G_EV[is.na(scoring_ev$G_EV)] <- 0
  
  primary_assist_ev <- box_score_data_master %>%
    dplyr::filter(primary_assist == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation_general == 'EV') %>%
    dplyr::select(primary_assist, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-c(primary_assist)) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(A1_EV = sum(count))
  
  primary_assist_ev <- merge(game_number_scoring, primary_assist_ev, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  primary_assist_ev$A1_EV[is.na(primary_assist_ev$A1_EV)] <- 0
  
  secondary_assist_ev <- box_score_data_master %>%
    dplyr::filter(secondary_assist == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation_general == 'EV') %>%
    dplyr::select(secondary_assist, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-c(secondary_assist)) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(A2_EV = sum(count))
  
  secondary_assist_ev <- merge(game_number_scoring, secondary_assist_ev, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE) %>%
    dplyr::rename(game_number2 = game_number)
  secondary_assist_ev$A2_EV[is.na(secondary_assist_ev$A2_EV)] <- 0
  
  #combine primary and secondary assists
  assists_ev <- cbind(primary_assist_ev, secondary_assist_ev)
  
  assists_ev <- assists_ev %>%
    dplyr::mutate(A_EV = A1_EV + A2_EV) %>%
    dplyr::select(game_number, A_EV) %>%
    dplyr::rename(game_number2 = game_number)
  
  scoring_ev <- cbind(scoring_ev, assists_ev) %>%
    dplyr::select(-c(game_number,game_number2))
  
  #PP Scoring
  game_number_scoring_pp <- tibble(c(team_games_participated$team_game)) %>% set_names('game_number')
  
  goals_pp <- box_score_data_master %>%
    dplyr::filter(goal == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation_general == 'PP') %>%
    dplyr::select(goal, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-c(goal)) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(G_PP = sum(count))
  
  scoring_pp <- merge(game_number_scoring_pp, goals_pp, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  scoring_pp$G_PP[is.na(scoring_pp$G_PP)] <- 0
  
  primary_assist_pp <- box_score_data_master %>%
    dplyr::filter(primary_assist == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation_general == 'PP') %>%
    dplyr::select(primary_assist, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-c(primary_assist)) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(A1_PP = sum(count))
  
  primary_assist_pp <- merge(game_number_scoring_pp, primary_assist_pp, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  primary_assist_pp$A1_PP[is.na(primary_assist_pp$A1_PP)] <- 0
  
  secondary_assist_pp <- box_score_data_master %>%
    dplyr::filter(secondary_assist == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation_general == 'PP') %>%
    dplyr::select(secondary_assist, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-c(secondary_assist)) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(A2_PP = sum(count))
  
  secondary_assist_pp <- merge(game_number_scoring_pp, secondary_assist_pp, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE) %>%
    dplyr::rename(game_number2 = game_number)
  secondary_assist_pp$A2_PP[is.na(secondary_assist_pp$A2_PP)] <- 0
  
  #combine primary and secondary assists
  assists_pp <- cbind(primary_assist_pp, secondary_assist_pp)
  
  assists_pp <- assists_pp %>%
    dplyr::mutate(A_PP = A1_PP + A2_PP) %>%
    dplyr::select(game_number, A_PP) %>%
    dplyr::rename(game_number2 = game_number)
  
  scoring_pp <- cbind(scoring_pp, assists_pp) %>%
    dplyr::select(-c(game_number,game_number2))
  
  #5v5 goals
  game_number_gf <- tibble(c(team_games_participated$team_game)) %>% set_names('game_number')
  
  GF1 <- box_score_data_master %>%
    dplyr::filter(GF1_name == player_name) %>%
    #dplyr::filter(GF1_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GF1_name, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GF1_name) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(GF1 = sum(count))
  
  GF1 <- merge(game_number_gf, GF1, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  GF1$GF1[is.na(GF1$GF1)] <- 0
  GF1 <- GF1 %>%
    dplyr::select(GF1)
  
  GF2 <- box_score_data_master %>%
    dplyr::filter(GF2_name == player_name) %>%
    #dplyr::filter(GF2_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GF2_name, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GF2_name) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(GF2 = sum(count))
  
  GF2 <- merge(game_number_gf, GF2, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  GF2$GF2[is.na(GF2$GF2)] <- 0
  GF2 <- GF2 %>%
    dplyr::select(GF2)
  
  GF3 <- box_score_data_master %>%
    dplyr::filter(GF3_name == player_name) %>%
    #dplyr::filter(GF3_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GF3_name, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GF3_name) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(GF3 = sum(count))
  
  GF3 <- merge(game_number_gf, GF3, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  GF3$GF3[is.na(GF3$GF3)] <- 0
  GF3 <- GF3 %>%
    dplyr::select(GF3)
  
  GF4 <- box_score_data_master %>%
    dplyr::filter(GF4_name == player_name) %>%
    #dplyr::filter(GF4_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GF4_name, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GF4_name) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(GF4 = sum(count))
  
  GF4 <- merge(game_number_gf, GF4, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  GF4$GF4[is.na(GF4$GF4)] <- 0
  GF4 <- GF4 %>%
    dplyr::select(GF4)
  
  GF5 <- box_score_data_master %>%
    dplyr::filter(GF5_name == player_name) %>%
    #dplyr::filter(GF5_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GF5_name, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GF5_name) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(GF5 = sum(count))
  
  GF5 <- merge(game_number_gf, GF5, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  GF5$GF5[is.na(GF5$GF5)] <- 0
  GF5 <- GF5 %>%
    dplyr::select(GF5)
  
  GF6 <- box_score_data_master %>%
    dplyr::filter(GF6_name == player_name) %>%
    #dplyr::filter(GF6_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GF6_name, gf_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GF6_name) %>%
    dplyr::group_by(gf_team_game_number) %>%
    dplyr::summarise(GF6 = sum(count))
  
  GF6 <- merge(game_number_gf, GF6, by.x = "game_number", by.y = "gf_team_game_number", all.x = TRUE)
  GF6$GF6[is.na(GF6$GF6)] <- 0
  GF6 <- GF6 %>%
    dplyr::select(GF6)
  
  
  GF <- cbind(GF1, GF2, GF3, GF4, GF5, GF6) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`5v5 GF` = sum(GF1, GF2, GF3, GF4, GF5, GF6)) %>%
    dplyr::select(`5v5 GF`)
  
  #GA
  game_number_ga <- tibble(c(team_games_participated$team_game)) %>% set_names('game_number')
  
  GA1 <- box_score_data_master %>%
    dplyr::filter(GA1_name == player_name) %>%
    #dplyr::filter(GA1_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GA1_name, ga_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GA1_name) %>%
    dplyr::group_by(ga_team_game_number) %>%
    dplyr::summarise(GA1 = sum(count))
  
  GA1 <- merge(game_number_ga, GA1, by.x = "game_number", by.y = "ga_team_game_number", all.x = TRUE)
  GA1$GA1[is.na(GA1$GA1)] <- 0
  GA1 <- GA1 %>%
    dplyr::select(GA1)
  
  GA2 <- box_score_data_master %>%
    dplyr::filter(GA2_name == player_name) %>%
    #dplyr::filter(GA2_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GA2_name, ga_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GA2_name) %>%
    dplyr::group_by(ga_team_game_number) %>%
    dplyr::summarise(GA2 = sum(count))
  
  GA2 <- merge(game_number_ga, GA2, by.x = "game_number", by.y = "ga_team_game_number", all.x = TRUE)
  GA2$GA2[is.na(GA2$GA2)] <- 0
  GA2 <- GA2 %>%
    dplyr::select(GA2)
  
  GA3 <- box_score_data_master %>%
    dplyr::filter(GA3_name == player_name) %>%
    #dplyr::filter(GA3_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GA3_name, ga_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GA3_name) %>%
    dplyr::group_by(ga_team_game_number) %>%
    dplyr::summarise(GA3 = sum(count))
  
  GA3 <- merge(game_number_ga, GA3, by.x = "game_number", by.y = "ga_team_game_number", all.x = TRUE)
  GA3$GA3[is.na(GA3$GA3)] <- 0
  GA3 <- GA3 %>%
    dplyr::select(GA3)
  
  GA4 <- box_score_data_master %>%
    dplyr::filter(GA4_name == player_name) %>%
    #dplyr::filter(GA4_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GA4_name, ga_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GA4_name) %>%
    dplyr::group_by(ga_team_game_number) %>%
    dplyr::summarise(GA4 = sum(count))
  
  GA4 <- merge(game_number_ga, GA4, by.x = "game_number", by.y = "ga_team_game_number", all.x = TRUE)
  GA4$GA4[is.na(GA4$GA4)] <- 0
  GA4 <- GA4 %>%
    dplyr::select(GA4)
  
  GA5 <- box_score_data_master %>%
    dplyr::filter(GA5_name == player_name) %>%
    #dplyr::filter(GA5_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GA5_name, ga_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GA5_name) %>%
    dplyr::group_by(ga_team_game_number) %>%
    dplyr::summarise(GA5 = sum(count))
  
  GA5 <- merge(game_number_ga, GA5, by.x = "game_number", by.y = "ga_team_game_number", all.x = TRUE)
  GA5$GA5[is.na(GA5$GA5)] <- 0
  GA5 <- GA5 %>%
    dplyr::select(GA5)
  
  GA6 <- box_score_data_master %>%
    dplyr::filter(GA6_name == player_name) %>%
    #dplyr::filter(GA6_name == player_name) %>%
    dplyr::filter(season == season_year) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::select(GA6_name, ga_team_game_number) %>%
    dplyr::mutate(count = 1) %>%
    dplyr::select(-GA6_name) %>%
    dplyr::group_by(ga_team_game_number) %>%
    dplyr::summarise(GA6 = sum(count))
  
  GA6 <- merge(game_number_ga, GA6, by.x = "game_number", by.y = "ga_team_game_number", all.x = TRUE)
  GA6$GA6[is.na(GA6$GA6)] <- 0
  GA6 <- GA6 %>%
    dplyr::select(GA6)
  
  
  GA <- cbind(GA1, GA2, GA3, GA4, GA5, GA6) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`5v5 GA` = sum(GA1, GA2, GA3, GA4, GA5, GA6)) %>%
    dplyr::select(`5v5 GA`)
  
  
  # %>%
  #   dplyr::mutate(GF = rowsum())
  
  summary <- cbind(toi, scoring_ev, scoring_pp, shots, GF, GA, corsi)

  #return(goals_ev)
  #return(summary)
  return(summary)
  #return(corsi)
  
}

team_game_log <- function(team_name, season_year) {
  
  corsi <- team_corsi_data_master_team %>%
    dplyr::filter(shlse_team_name == team_name, season == season_year) %>%
    dplyr::select(date1, season, CF, CA, FF, FA) %>%
    dplyr::mutate(`CF%` = (round((CF / (CF + CA)),3)) * 100,
                  `FF%` = (round((FF / (FF + FA)),3)) * 100) %>%
    dplyr::select(date1, season, CF, CA, `CF%`, FF, FA, `FF%`)
  
  shotsfor <- team_shotsfor_data_master %>%
    dplyr::filter(shlse_team_name == team_name, season == season_year) %>%
    dplyr::select(SOG, SOG_PP)
  
  shotsagainst <- team_shotsagainst_data_master %>%
    dplyr::filter(shlse_team_name == team_name, season == season_year) %>%
    dplyr::select(SOGA)
  
  
  summary <- cbind(corsi, shotsfor, shotsagainst)
  
  return(summary)
  
}

str_right <- function(string, n) {
  substr(string, nchar(string) - (n - 1), nchar(string))
}

# UI ----------------------------------------------------------------------

bs_theme_new()

bs_theme_add_variables(
  primary = "#005293"
)

bs_theme_fonts(
  base = "sweden_sansregular", code = "sweden_sansregular"
)

# 
# remotes::install_github("rstudio/thematic", force = TRUE)
# library(thematic)
# 
# remotes::install_github("rstudio/shiny#2740")


#bs_theme_accent_colors(primary = "#005293")

ui <- bootstrapPage(
  
  tags$head(includeHTML("google-analytics.html")),
  
  tags$head(
    #includeHTML(includeHTML("google-analytics.html")),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
    tags$meta(name = "title", content = "Svengelska Hockey | Swedish Hockey Stats Site"),
    tags$meta(name = "description", content = "Miscellaneous stats and visualizations on Swedish hockey. Contains player & team data on the Swedish Hockey League and HockeyAllsvenskan.")
  ),
  
  #tags$head(includeHTML("google-analytics.html")),
  
  bootstraplib::bootstrap(),
  
  #navbarPage("Svengelska Hockey",
  navbarPage(#title = div(img(src='logo.png',style="margin-top: -14px; padding-right:10px", height = 50.4, width = 225)),
             title = div(img(src='logo2.png',style="margin-top: -7px; padding-right:10px", height = 50.4, width = 225)),
             #theme = "journal",
             windowTitle="Svengelska Hockey",
             
             #;padding-bottom:10px
    
    #App Home
    tabPanel("Home",
             h1("Svengelska Hockey"),
             #img(src = "logo.png", height = 166, width = 400),
             p("Miscellaneous data on Swedish hockey. Currently there are two main sections to this site:"),
             tags$ul(
               #tags$li(tags$b("Player & Team Statistics"), " - something"),
               tags$li(tags$em("Player & Team Statistics"), "- here you'll find a variety of traditional and advanced stats broken out into several game situations. 
                       You can isolate performance to specific manpower situations (5v5, 3v3, Empty Net, etc.), split assists into primary and secondary, see percentage and rate stats, and other things you can't easily find elsewhere."),
               br(),
               tags$li(tags$em("Stats Visualizations"), "- these are meant to be appealing ways to represent a player or team's season over time. The player cards, for example,
                       show a game-by-game look at performance in a variety of areas - ice time, scoring, shot differential, and on-ice goals for and against.")),
             
             p("I'm planning to add lots more going forward - more stats, more visualizations, more leagues - this is just a starting point."),
             
             h3("Data Sources"),
             p("All data on this site originially comes from:"),
              tags$ul(
                tags$li("SHL Official League Website - ", a(target = "_blank", "shl.se", href = "https://shl.se")),
                tags$li("Swedish Ice Hockey Association - ", a(target = "_blank", "stats.swehockey.se", href = "https://stats.swehockey.se")),
                tags$li("Eliteprospects - ", a(target = "_blank", "eliteprospects.com", href = "https://eliteprospects.com"))
              ),
             hr(),
             p("You can find me on Twitter at ", a(target = "_blank", "@SvengelskaHky",
                                                   href = "https://twitter.com/SvengelskaHky"), " or ", a(target = "_blank", "@zellenthal_swe",
                                                                                                      href = "https://twitter.com/zellenthal_swe"))
    ),
    
    navbarMenu("Statistics",
    tabPanel("SHL - Player Stats",
             
             fluidPage(
               titlePanel("SHL - Player Stats"),
               br(),
               
               fluidRow(column(2, selectInput("stats_situation", "Game Situation", choices = c("All Sits", "EV", "PP", "SH", "5v5", "4v4", "3v3", "5v4", "5v3","Empty Net", "PS/SO"), selected = "EV")),
             
                        column(2, selectInput("stats_team", "Team", choices = c("All", sort(unique(as.character(EV$Team)))), selected = "All")),
               
                        #column(2, selectInput("stats_pos", "Position", choices = c("All", "F", "D"), selected = "All"))),
               
                        column(2, selectInput("stats_pos", "Position", choices = c("All", "F", "D"), selected = "All")),
                        
                        column(1, numericInput("min_age", "Min Age", value = 15)),
                        column(1, numericInput("max_age", "Max Age", value = 50))),
             
               fluidRow(#column(2, selectInput("stats_player", "Player", choices = c("All", sort(unique(as.character(EV$Player))))), selected = "All", multiple = FALSE),
                        
                        #column(2, selectInput("stats_player2", "Player2", choices = c("All", sort(unique(as.character(EV$Player)))), selected = "All", multiple = TRUE, selectize = TRUE)),
                        column(2, selectInput("stats_season", "Season", choices = c("All", "17/18", "18/19", "19/20", "20/21"), selected = "20/21")),
                        column(2, numericInput("stats_minGP", "Min GP", value = 0)),
                        column(2, selectInput("nationality", "Nationality", choices = c("All", sort(unique(as.character(EV$Nationality)))))),
                        column(2, selectInput("nhl_rights", "NHL Rights", choices = c("All", nhl_abbrev[1:31]), selected = "All")),
                        column(2, selectizeInput("stats_player2", "Player", choices = sort(unique(as.character(EV$Player))), multiple = TRUE))),
                        
                        
                      
                        #column(1, numericInput("stats_minGP", "Min. GP", value = 10))),
               
                        submitButton("Submit"),
                        #actionButton("stats_submit", "Submit"),
               
               
                        #column(2, actionButton("submit"))),
                        
                        #submitButton("Submit")),
               
               hr(),
               
             
             #Stats Table
             fluidRow(DT::dataTableOutput("stats_table")),
             
             #Download button
             downloadButton("stats_download"),
             br(),
             br(),
             
             )
             ),
    
    #V2 PROFILE
    tabPanel("SHL - Player Profiles",
             fluidPage(
               titlePanel("SHL - Player Profile"),
               br(),
               fluidRow(column(3, selectInput("player_profile", "Select Player", choices = sort(unique(as.character(EV$Player))), selected = "Lucas Raymond")),
                        #column(2, selectInput("season_gamelog", "Season", choices = c("17/18", "18/19", "19/20", "20/21"), selected = "20/21")),
                        div(column(2, submitButton(text = "Submit"), style = "margin-top: 29px; margin-left: 0px"))),
                        #div(helpText(paste0("Season only apples to \n", "Player Card and Game Log")), style = "margin-top: 29px")),
              
               h4(textOutput("player_profile_bio1")),
               h6(textOutput("player_profile_bio2")),
               
               br(),
    tabsetPanel(
      tabPanel("Statistics", fluid = TRUE,
               
               br(),
               #fluidRow(column(3, selectInput("player_profile", "Select Player", choices = sort(unique(as.character(EV$Player))), selected = "Marek Hrivik")),
                        #div(column(2, submitButton(text = "Submit"), style = "margin-top: 29px"))),
               
               #h4("5v5"),
               #fluidRow(DT::dataTableOutput("player_profile_5v5_v2")),
              
               h4("5v5"),
               fluidRow(column(12,DT::dataTableOutput("player_profile_5v5"))),
               
               br(),
               
               h4("Even Strength"),
               fluidRow(column(12,DT::dataTableOutput("player_profile_ev"))),
               
               br(),
               
               h4("Powerplay"),
               fluidRow(column(12,DT::dataTableOutput("player_profile_pp"))),
               
               br(),
               
               h4("All Situations"),
               fluidRow(column(12,DT::dataTableOutput("player_profile_all"))),
               
               br(),
               
               h4("Shorthanded"),
               fluidRow(column(10,DT::dataTableOutput("player_profile_sh"))),
               
               br(),
               
               h4("Empty Net"),
               fluidRow(column(8,DT::dataTableOutput("player_profile_eng"))),
               
               br(),
               
               h4("PS/SO"),
               fluidRow(column(4,DT::dataTableOutput("player_profile_psso"))),
               
               br(), br()
               

               ),

      tabPanel("Player Card", fluid = TRUE,
               br(),
               
               fluidRow(column(2, selectInput("player_card_profile_season", "Season", choices = c("17/18", "18/19", "19/20", "20/21"), selected = "20/21")),
                        div(column(2, submitButton(text = "Submit"), style = "margin-top: 29px"))),
               fluidRow(column(8, plotOutput("player_card_v2"))),
              
               br(), br(),
              ),
      

      tabPanel("Game Log", fluid = TRUE,
               br(),
               
               fluidRow(column(2, selectInput("player_card_gamelog_season", "Season", choices = c("17/18", "18/19", "19/20", "20/21"), selected = "20/21")),
                        div(column(2, submitButton(text = "Submit"), style = "margin-top: 29px"))),
               
               # fluidRow(column(3, selectInput("player_gamelog", "Select Player", choices = sort(unique(as.character(EV$Player))), selected = "Marek Hrivik")),
               #          column(2, selectInput("season_gamelog", "Season", choices = c("17/18", "18/19", "19/20", "20/21"), selected = "20/21")),
               #          div(column(2, submitButton(text = "Submit"), style = "margin-top: 29px")))
               
               
               fluidRow(DT::dataTableOutput("gamelog")),
               # br(),
               # #Download button
               # downloadButton("stats_download_gamelog"),
               br(), br(),
               )

    ))),
    
    # tabPanel("SHL - Player Profiles",
    #          fluidPage(
    #            #tabsetPanel(
    #            titlePanel("SHL - Player Profile"),
    #            #tabPanel("SHL - Player Profile", fluid = TRUE),
    #            br(),
    # 
    #            #fluidRow(column(3, selectInput("player_profile", "Select Player", choices = sort(unique(as.character(EV$Player))), selected = "Marek Hrivik"))),
    #            #submitButton("Submit"),
    #            #title = div(img(src='logo2.png',style="margin-top: -7px; padding-right:10px", height = 50.4, width = 225))
    #            fluidRow(column(3, selectInput("player_profile", "Select Player", choices = sort(unique(as.character(EV$Player))), selected = "Marek Hrivik")),
    #                     div(column(2, submitButton(text = "Submit"), style = "margin-top: 29px"))),
    # 
    #            #fluidRow(column(2, submitButton(text = "Submit"))),
    #            #br(),
    # 
    #            h4(textOutput("player_profile_bio1")),
    #            h6(textOutput("player_profile_bio2")),
    # 
    #            br(),
    #            h4("5v5"),
    #            fluidRow(DT::dataTableOutput("player_profile_5v5")),
    # 
    #            br(),
    # 
    #            h4("Even Strength"),
    #            fluidRow(DT::dataTableOutput("player_profile_ev")),
    # 
    #            br(),
    # 
    #            h4("Powerplay"),
    #            fluidRow(DT::dataTableOutput("player_profile_pp")),
    # 
    #            br(),
    # 
    #            h4("All Situations"),
    #            fluidRow(DT::dataTableOutput("player_profile_all")),
    # 
    #            br(),
    # 
    #            h4("Shorthanded"),
    #            fluidRow(DT::dataTableOutput("player_profile_sh")),
    # 
    #            br(),
    # 
    #            h4("Empty Net"),
    #            fluidRow(DT::dataTableOutput("player_profile_eng")),
    # 
    #            br(), br()
    # 
    #          )),

    # tabPanel("SHL - Game Logs",
    #          fluidPage(
    #            titlePanel("SHL - Game Logs"),
    #            #tabPanel("SHL - Game Logs", fluid = TRUE),
    #            br(),
    # 
    #            fluidRow(column(3, selectInput("player_gamelog", "Select Player", choices = sort(unique(as.character(EV$Player))), selected = "Marek Hrivik")),
    #                     column(2, selectInput("season_gamelog", "Season", choices = c("All","17/18", "18/19", "19/20", "20/21"), selected = "20/21")),
    #                     div(column(2, submitButton(text = "Submit"), style = "margin-top: 29px"))),
    # 
    # 
    #            fluidRow(DT::dataTableOutput("gamelog"))
    # 
    #          )),
    
    
    
    tabPanel("SHL - Team Stats",
             fluidPage(
                titlePanel("SHL - Team Stats"),
                br(),
                
                fluidRow(
                  column(2, selectInput("stats_situation_team", "Game Situation", choices = c("EV ", "PP ", "SH ", "5v5 "), selected = "EV ")),
                  column(2, selectInput("stats_team_team", "Team", choices = c("All", sort(unique(as.character(`EV `$Team)))), selected = "All")),
                  column(2, selectInput("stats_season_team", "Season", choices = c("All", "17/18", "18/19", "19/20", "20/21"), selected = "20/21")),
                  div(column(2, submitButton(text = "Submit"), style = "margin-top: 28px"))),
                #),
                
                #submitButton("Submit"),
                hr(),
                
                #Team Stats Table
                fluidRow(column(10, DT::dataTableOutput("stats_table_team"))),
                
                #Download button
                downloadButton("stats_download_team"),
                br(),
                br(),
                
             )
    ),
    
    
      
    
    tabPanel("HA - Player Stats",
             fluidPage(
               titlePanel("HockeyAllsvenskan - Player Stats"),
               br(),
               
               fluidRow(column(2, selectInput("stats_situation_hap", "Game Situation", choices = c("All Sits  ", "EV  ", "PP  ", "SH  ", "5v5  ", "4v4  ", "3v3  ", "5v4  ", "5v3  ","Empty Net  ", "PS/SO  "), selected = "5v5  ")),
                        
                        column(2, selectInput("stats_team_hap", "Team", choices = c("All", sort(unique(as.character(`EV  `$Team)))), selected = "All")),
                        
                        column(2, selectInput("stats_pos_hap", "Position", choices = c("All", "F", "D"), selected = "All")),
                        
                        column(1, numericInput("ha_min_age", "Min Age", value = 15)),
                        column(1, numericInput("ha_max_age", "Max Age", value = 50))),
               
               fluidRow(#column(2, selectInput("stats_player", "Player", choices = c("All", sort(unique(as.character(EV$Player))))), selected = "All", multiple = FALSE),
                 
                 #column(2, selectInput("stats_player2", "Player2", choices = c("All", sort(unique(as.character(EV$Player)))), selected = "All", multiple = TRUE, selectize = TRUE)),
                 
                 column(2, selectInput("stats_season_hap", "Season", choices = c("All", "19/20", "20/21"), selected = "20/21")),
                 column(2, numericInput("stats_minGP_hap", "Min GP", value = 0)),
                 column(2, selectInput("ha_nationality", "Nationality", choices = c("All", sort(unique(as.character(`EV  `$Nationality)))))),
                 column(2, selectInput("nhl_rights_hap", "NHL Rights", choices = c("All", nhl_abbrev[1:31]), selected = "All")),
                 column(2, selectizeInput("stats_player2_hap", "Player", choices = sort(unique(as.character(`EV  `$Player))), multiple = TRUE))),

               
               submitButton("Submit"),
               hr(),
               
               #Stats Table
               fluidRow(DT::dataTableOutput("stats_table_hap")),
               
               #Download button
               downloadButton("stats_download_hap"),
               br(),
               br(),
               
             )),
    
    tabPanel("HA - Player Profiles",
             fluidPage(
               titlePanel("HockeyAllsvenskan - Player Profile"),
               br(),
               
               #fluidRow(column(3, selectInput("player_profile", "Select Player", choices = sort(unique(as.character(EV$Player))), selected = "Marek Hrivik"))),
               #submitButton("Submit"),
               #title = div(img(src='logo2.png',style="margin-top: -7px; padding-right:10px", height = 50.4, width = 225))
               fluidRow(column(3, selectInput("player_profile_ha", "Select Player", choices = sort(unique(as.character(`EV  `$Player))), selected = "Jonathan Dahlén")),
                        div(column(2, submitButton(text = "Submit"), style = "margin-top: 29px"))),
               
               #fluidRow(column(2, submitButton(text = "Submit"))),
               #br(),
               
               # h4(textOutput("ha_player_profile_bio1")),
               # h6(textOutput("ha_player_profile_bio2")),
               
               br(),
               h4("5v5"),
               fluidRow(column(10,DT::dataTableOutput("ha_player_profile_5v5"))),
               
               br(),
               
               h4("Even Strength"),
               fluidRow(column(10,DT::dataTableOutput("ha_player_profile_ev"))),
               
               br(),
               
               h4("Powerplay"),
               fluidRow(column(10,DT::dataTableOutput("ha_player_profile_pp"))),
               
               br(),
               
               h4("All Situations"),
               fluidRow(column(10,DT::dataTableOutput("ha_player_profile_all"))),
               
               br(),
               
               h4("Shorthanded"),
               fluidRow(column(10,DT::dataTableOutput("ha_player_profile_sh"))),
               
               br(),
               
               h4("Empty Net"),
               fluidRow(column(10,DT::dataTableOutput("ha_player_profile_eng"))),
               
               br(),
               
               h4("PS/SO"),
               fluidRow(column(4,DT::dataTableOutput("ha_player_profile_psso"))),
               
               br(), br()
               
             )),
    
    tabPanel("HA - Team Stats (Coming Soon)",
             fluidPage(
               titlePanel("HockeyAllsvenskan - Team Stats"),
               p("Coming Soon"),
               p("Here's a picture of Marcus Sörensen instead:"),
               img(src = "sorensen2.png", height = 417.5, width = 600),
             ))
    
    
    ),
    
    #Start messing aorund here
    
    # ,
    # 
    # tabPanel("Player Profiles",
    #          fluidPage(
    #            titlePanel("Player Profile"),
    #            fluidRow(column(2, selectInput("player_profile", "Select Player", choices = sort(unique(as.character(EV$Player))), selected = "Nils Lundkvist")))
    #          ))
    # 
    # ),
    
    navbarMenu("Visualizations",
               tabPanel("Player Cards",
                        fluidPage(
                          titlePanel("SHL Game-by-Game Player Cards"),
                          hr(),
                          
                          sidebarLayout(sidebarPanel(selectInput("card_player", "Player", choices = as.list(player_names_master[1:length(player_names_master)]),
                                                                 selected = "Nils Lundkvist", width = "225px"),
                                                     
                                                     selectInput("card_season", "Season", choices = c('17/18', '18/19', '19/20', '20/21'),
                                                                 selected = '20/21', width = "225px"),
                                                     
                                                     
                                                     submitButton("Submit"),
                                                     br(),
                                                     helpText("Note: If you get an error, it's most likely because",
                                                              "that player did not play in the season you selected.",
                                                              "Example: Elias Pettersson only played in the 17/18 SHL season."),
                                                     #bookmarkButton(),
                                                     #br(),
                                                     #code("If you get an error message, it's most likely because the player you selected did not play in the league that season.")
                                                     
                                                     ),
                                        
                            
                          mainPanel(plotOutput("player_card")),
                          
                        ),
                        
                        br(), 
                        br(),
                        )
                        
                        ),
               
               tabPanel("Player Usage",
                        fluidPage(
                          titlePanel("Player Usage - Smoothed TOI"),
                          hr(),
                          
                          sidebarLayout(sidebarPanel(selectInput("toi_team", "Team", choices = sort(unique(as.character(EV$Team))),
                                                                 selected = "Luleå", width = "225px"),
                                                     
                                                     selectInput("toi_season", "Season", choices = c("17/18", "18/19", "19/20", "20/21"), selected = "20/21", width = "225px"),
                                                     
                                                     selectInput("toi_pos", "Position", choices = c("F", "D"), selected = "D", width = "225px"),
                                                     
                                                     selectInput("toi_sit", "Situation", choices = c("All", "EV", "PP", "SH"), selected = "All", width = "225px"),
                                                     
                                                     #selectInput("toi_season", "Season", choices = c("17/18", "18/19", "19/20"), selected = "19/20", width = "225px"),
                                                     
                                                     submitButton("Submit")),
                                                     
                                          mainPanel(plotOutput("toi_smoothed")),
                                          #mainPanel(plotlyOutput("toi_smoothed", width = "740px", height = "770px")),
                                        
                                                     
                                                     )),
                        
                        br(),
                        br()
                          
                        ),
               
               tabPanel("Team Cards",
                        fluidPage(
                          titlePanel("SHL Game-by-Game Team Cards"),
                          hr(),
                          
                          sidebarLayout(sidebarPanel(selectInput("card_team", "Team", choices = as.list(swehockey_team_names[1:length(swehockey_team_names)]),
                                                                 selected = "Luleå HF", width = "225px"),
                                                     
                                                     selectInput("card_season_team", "Season", choices = c('17/18', '18/19', '19/20', '20/21'),
                                                                 selected = '20/21', width = "225px"),
                                                     
                                                     
                                                     submitButton("Submit")),
                                        
                                        
                                        mainPanel(plotOutput("team_card")),
                              
                                        
                          ),
                          
                          br(), br()
                          )
                        )
                        
               
               ),
    
    tabPanel("Blogs",
             fluidPage(
               h2("Random Things I've Written"),
               p("About once or twice a year I get the urge to write something related to Swedish hockey. Usually it's very random. Here are some of those blogs:"),
               
               br(),
               
               h5(a(target = "_blank", "The Charm of Swedish Hockey",
                    href = "https://medium.com/@zellenthal/the-charm-of-swedish-hockey-669cfc4fe87c")),
               p("Why I enjoy Swedish hockey so much, and why I think it offers a more pure fan experience than the NHL."),
               br(),
               
               h5(a(target = "_blank", "Ligaen Overload",
                    href = "https://medium.com/@zellenthal/ligaen-overload-4ac66cd0170c")),
               p("An incredibly rough attempt at creating league translation factors to determine if the Norwegian GET-Ligaen or the Danish Metal-Ligaen is better."),
               br(),
               
               h5(a(target = "_blank", "2018/19 SHL Watchability Rankings",
                    href = "https://medium.com/@zellenthal/2018-19-shl-watchability-rankings-3f4c62701822")),
               p("Built a model to rank the SHL teams in order of watchability based on several on-ice and off-ice critera. Maybe I'll revisit this on this site some time soon."),
               br(),
               
               h5(a(target = "_blank", "My 5 Seasons as  Djurgården Fan, Ranked",
                    href = "https://medium.com/@zellenthal/my-5-seasons-as-a-djurg%C3%A5rden-fan-ranked-d0ff45c0e199")),
               p("I'm barely a Djurgården fan anymore, but my love for Swedish hockey started with them. This is really more about becoming a fan than the actual ranking.")
               
             )
             )
    # ,
    # 
    # tabPanel("Test Blog",
    #          fluidPage(
    #            titlePanel("Test Post"),
    #            mainPanel(
    #              includeHTML("test_post.html")
    #            )
    #          )
    #          )
    
  ) #navbarPage
  
) #bootstrapPage


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Here you read the URL parameter from session$clientData$url_search
  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if (!is.null(query[['card_player']])) {
  #     updateSelectInput(session, 'card_player', value = query[['card_player']])
  #   }
  # })
  
  # player_profile <- reactive({
  #   player_profile_selected <- input$player_profile
  # })
  
  player_profile_selected <- reactive(input$player_profile)
  season_selected <- reactive(input$season_gamelog)
  
  #Statistics table output
  output$stats_table <- DT::renderDataTable(DT::datatable({
    
    stats_data <- get(input$stats_situation)
    
    if (input$stats_team != "All") {
      stats_data <- stats_data[stats_data$Team == input$stats_team, ]
    }
    
    if (input$stats_pos != "All") {
      stats_data <- stats_data[stats_data$Pos == input$stats_pos,]
    }
    
    if (input$stats_season != "All") {
      stats_data <- stats_data[stats_data$Season == input$stats_season,]
    }
    
    if (input$stats_minGP != 0) {
      stats_data <- stats_data[stats_data$GP >= input$stats_minGP,]
    }
    
    if (input$nhl_rights != "All") {
      stats_data <- stats_data[stats_data$`NHL Rights` == input$nhl_rights,]
    }
    
    if (input$nationality != "All") {
      stats_data <- stats_data[stats_data$Nationality == input$nationality,]
    }
    
    if (input$min_age != 15) {
      stats_data <- stats_data[stats_data$Age >= input$min_age,]
    }
    
    if (input$max_age != 50) {
      stats_data <- stats_data[stats_data$Age <= input$max_age,]
    }
    
    # if (input$stats_player != "All") {
    #   stats_data <- stats_data[stats_data$Player == input$stats_player, ]
    # }
    
    if (isTRUE(input$stats_player2 != "")) {
      stats_data <- stats_data[stats_data$Player == input$stats_player2,]
      #stats_data <- stats_data %>% dplyr::filter(Player %in% c(input$stats_player2[1], input$stats_player2[2]))
      #stats_data <- stats_data %>% dplyr::filter(Player == input$stats_player2[1] | Player == input$stats_player2[2])
    }
  
    #df<- select(filter(dat,name=='tom'| name=='Lynn'), c('days','name))
  
    
    # if (isTRUE(input$stats_player2 != "")) {
    #   stats_data <- stats_data[stats_data$Player == input$stats_player2[1] | stats_data$Player == input$stats_player2[2], ]
    # }
    
    output$stats_download <- downloadHandler(
      filename = function() {
        paste0("player_stats", ".csv")
      },
      content = function(file) {
        write_excel_csv(stats_data, file)
      }
    )
    
    rownames(stats_data) <- NULL
    
    stats_data
    
  },
  
  #extensions = c('FixedHeader', 'FixedColumns'),
  extensions = 'FixedHeader',
  
  options = list(
    #change the pagination options
    pageLength = 25,
    lengthMenu = c(25, 50, 100, 500),
    
    #center character columns
    #columnDefs = list(list(className = 'dt-center', targets = c(2,4:5))),
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5:6,7)),
                           list(targets = c(-1, -2, 6), visible = FALSE)),
    
    # datatable(head(iris, 20), options = list(
    #   columnDefs = list(list(className = 'dt-center', targets = 1:5),
    #                     list(targets = 5, visible = FALSE)),
    
    #order the table by GP - applies to all tables
    order = list(list(1, 'asc')),
    
    #removes the search bar! everything but 's'
    dom = 'ltipr',
    
    fixedHeader = TRUE,
    
    #freezes the first 5 columns
    #scrollX = TRUE,
    #autoWidth = TRUE,
    #fixedColumns = list(leftColumns = 2),
    
    #makes the column blue - the swedish blue in this case
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#005293', 'color': '#fff'});",
      "}")
    
  ),
  
  #makes the table look a lot nicer
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #Team Stats Output
  output$stats_table_team <- DT::renderDataTable(DT::datatable({
    
    team_stats_data <- get(input$stats_situation_team)
    
    if (input$stats_team_team != "All") {
      team_stats_data <- team_stats_data[team_stats_data$Team == input$stats_team_team, ]
    }
    
    if (input$stats_season_team != "All") {
      team_stats_data <- team_stats_data[team_stats_data$Season == input$stats_season_team, ]
    }
    
    output$stats_download_team <- downloadHandler(
      filename = function() {
        paste0("team_stats", ".csv")
      },
      content = function(file) {
        write_excel_csv(team_stats_data, file)
      }
    )
  
    rownames(team_stats_data) <- NULL
    
    team_stats_data
    
  },
  
  #extensions = c('FixedHeader', 'FixedColumns'),
  extensions = 'FixedHeader',
  
  options = list(
    #change the pagination options
    pageLength = 25,
    lengthMenu = c(25, 50, 100),
    
    #center character columns
    columnDefs = list(list(className = 'dt-center', targets = c(2))),
    
    #order the table by GP - applies to all tables
    order = list(list(1, 'asc')),
    
    #removes the search bar! everything but 's'
    dom = 'ltipr',
    
    fixedHeader = TRUE,
    
    #freezes the first 5 columns
    #scrollX = TRUE,
    #autoWidth = TRUE,
    #fixedColumns = list(leftColumns = 2),
    
    #makes the column blue - the swedish blue in this case
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#005293', 'color': '#fff'});",
      "}") 
    
  ),
    
  
  #makes the table look a lot nicer
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #Hockey Allsvenskan Player Stats Table
  output$stats_table_hap <- DT::renderDataTable(DT::datatable({
    
    stats_data_hap <- get(input$stats_situation_hap)
    
    if (input$stats_team_hap != "All") {
      stats_data_hap <- stats_data_hap[stats_data_hap$Team == input$stats_team_hap, ]
    }
    
    if (input$stats_pos_hap != "All") {
      stats_data_hap <- stats_data_hap[stats_data_hap$Pos == input$stats_pos_hap,]
    }
    
    if (input$stats_season_hap != "All") {
      stats_data_hap <- stats_data_hap[stats_data_hap$Season == input$stats_season_hap,]
    }
    
    if (input$stats_minGP_hap != 0) {
      stats_data_hap <- stats_data_hap[stats_data_hap$GP >= input$stats_minGP_hap,]
    }
    
    if (input$nhl_rights_hap != "All") {
      stats_data_hap <- stats_data_hap[stats_data_hap$`NHL Rights` == input$nhl_rights_hap,]
    }
    
    if (input$ha_nationality != "All") {
      stats_data_hap <- stats_data_hap[stats_data_hap$Nationality == input$ha_nationality,]
    }
    
    if (input$ha_min_age != 15) {
      stats_data_hap <- stats_data_hap[stats_data_hap$Age >= input$ha_min_age,]
    }
    
    if (input$ha_max_age != 50) {
      stats_data_hap <- stats_data_hap[stats_data_hap$Age <= input$ha_max_age,]
    }
    
    # if (input$stats_minGP != 0) {
    #   stats_data <- stats_data[stats_data$GP >= input$stats_minGP,]
    # }
    
    # if (input$stats_player != "All") {
    #   stats_data <- stats_data[stats_data$Player == input$stats_player, ]
    # }
    
    if (isTRUE(input$stats_player2_hap != "")) {
      stats_data_hap <- stats_data_hap[stats_data_hap$Player == input$stats_player2_hap,]
      #stats_data <- stats_data %>% dplyr::filter(Player %in% c(input$stats_player2[1], input$stats_player2[2]))
      #stats_data <- stats_data %>% dplyr::filter(Player == input$stats_player2[1] | Player == input$stats_player2[2])
    }
    
    #df<- select(filter(dat,name=='tom'| name=='Lynn'), c('days','name))
    
    
    # if (isTRUE(input$stats_player2 != "")) {
    #   stats_data <- stats_data[stats_data$Player == input$stats_player2[1] | stats_data$Player == input$stats_player2[2], ]
    # }
    
    output$stats_download_hap <- downloadHandler(
      filename = function() {
        paste0("player_stats_ha", ".csv")
      },
      content = function(file) {
        write_excel_csv(stats_data_hap, file)
      }
    )
    
    rownames(stats_data_hap) <- NULL
    
    stats_data_hap
    
  },
  
  #extensions = c('FixedHeader', 'FixedColumns'),
  extensions = 'FixedHeader',
  
  options = list(
    #change the pagination options
    pageLength = 25,
    lengthMenu = c(25, 50, 100, 500),
    
    #center character columns
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6,7)),
                      list(targets = c(-1,-2,6), visible = FALSE)),
    
    #order the table by GP - applies to all tables
    order = list(list(1, 'asc')),
    
    #removes the search bar! everything but 's'
    dom = 'ltipr',
    
    fixedHeader = TRUE,
    
    #freezes the first 5 columns
    #scrollX = TRUE,
    #autoWidth = TRUE,
    #fixedColumns = list(leftColumns = 2),
    
    #makes the column blue - the swedish blue in this case
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#005293', 'color': '#fff'});",
      "}")
    
  ),
  
  #makes the table look a lot nicer
  class = 'row-border stripe compact hover nowrap',
  
  ))
    
  
  #Player Card output
  output$player_card <- renderPlot({
    
    player_card(input$card_player, input$card_season)
    #player_card(player_profile_selected(), input$card_season)
    
  },
  
  height = 770, width = 740,
  res = 96
  
  )
  
  #Player Card output
  output$player_card_v2 <- renderPlot({
    
    #player_card(player_profile_selected(), season_selected())
    player_card(player_profile_selected(), input$player_card_profile_season)
    #player_card(player_profile_selected(), input$card_season)
    
  },
  
  height = 770, width = 740,
  res = 96
  
  )
  
  #TOI Smoothed output
  output$toi_smoothed <- renderPlot({

    team_toi(input$toi_team, input$toi_season, input$toi_pos, input$toi_sit)

  },

  height = 770, width = 740,
  res = 96

  )
  
  # #TOI Smoothed output
  # output$toi_smoothed <- renderPlotly(
  # 
  #   team_toi(input$toi_team, input$toi_season, input$toi_pos, input$toi_sit)
  # 
  # 
  # 
  # )

  #Player Card output
  output$team_card <- renderPlot({

    team_card(input$card_team, input$card_season_team)

  },

  height = 960, width = 740,
  res = 96

  )

  
  #Team Card output
  output$team_card <- renderPlot({
    
    team_card(input$card_team, input$card_season_team)
    
  },
  
  height = 960, width = 740,
  res = 96
  
  )
  
  # output$player_profile_bio1 <- renderText({
  # 
  #   player_profile_bio1(input$player_profile)
  # 
  # })
  # 
  # output$player_profile_bio2 <- renderText({
  #   
  #   player_profile_bio2(input$player_profile)
  #   
  # })
  
  output$player_profile_bio1 <- renderText({
    
    player_profile_bio1(player_profile_selected())
    
  })
  
  output$player_profile_bio2 <- renderText({
    
    player_profile_bio2(player_profile_selected())
    
  })
  
  #Player Profile output
  #5v5
  output$player_profile_5v5 <- DT::renderDataTable(DT::datatable({
    
    #player_profile_5v5(input$player_profile)
    player_profile_5v5(player_profile_selected())
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1, -2, 5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
    
    class = 'row-border stripe compact hover nowrap',
  
  ))
  
  # #5v5 V2
  # output$player_profile_5v5_v2 <- DT::renderDataTable(DT::datatable({
  #   
  #   player_profile_5v5(player_profile_selected())
  #   
  # },
  # 
  # extensions = 'FixedHeader',
  # options = list(
  #   
  #   columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
  #                     list(targets = c(-1, -2, 5,6,7), visible = FALSE)),
  #   
  #   order = list(list(2, 'asc')),
  #   #dom = 'ltipr',
  #   dom = 't',
  #   fixedHeader = TRUE,
  #   
  #   initComplete = JS(
  #     "function(settings, json) {",
  #     #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
  #     "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
  #     "}")),
  # 
  # class = 'row-border stripe compact hover nowrap',
  # 
  # ))
  
  #EV
  output$player_profile_ev <- DT::renderDataTable(DT::datatable({
    
    #player_profile_ev(input$player_profile)
    player_profile_ev(player_profile_selected())
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1, -2, 5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#005293', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #PP
  output$player_profile_pp <- DT::renderDataTable(DT::datatable({
    
    #player_profile_pp(input$player_profile)
    player_profile_pp(player_profile_selected())
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #SH
  output$player_profile_sh <- DT::renderDataTable(DT::datatable({
    
    #player_profile_sh(input$player_profile)
    player_profile_sh(player_profile_selected())
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #All
  output$player_profile_all <- DT::renderDataTable(DT::datatable({
    
    #player_profile_all(input$player_profile)
    player_profile_all(player_profile_selected())
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #ENG
  output$player_profile_eng <- DT::renderDataTable(DT::datatable({
    
    #player_profile_eng(input$player_profile)
    player_profile_eng(player_profile_selected())
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #PS/SO
  output$player_profile_psso <- DT::renderDataTable(DT::datatable({
    
    #player_profile_eng(input$player_profile)
    player_profile_psso(player_profile_selected())
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5))),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #HA Player Profile output
  #5v5
  output$ha_player_profile_5v5 <- DT::renderDataTable(DT::datatable({
    
    ha_player_profile_5v5(input$player_profile_ha)
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #EV
  output$ha_player_profile_ev <- DT::renderDataTable(DT::datatable({
    
    ha_player_profile_ev(input$player_profile_ha)
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #PP
  output$ha_player_profile_pp <- DT::renderDataTable(DT::datatable({
    
    ha_player_profile_pp(input$player_profile_ha)
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #EV
  output$ha_player_profile_sh <- DT::renderDataTable(DT::datatable({
    
    ha_player_profile_sh(input$player_profile_ha)
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #All
  output$ha_player_profile_all <- DT::renderDataTable(DT::datatable({
    
    ha_player_profile_all(input$player_profile_ha)
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #ENG
  output$ha_player_profile_eng <- DT::renderDataTable(DT::datatable({
    
    ha_player_profile_eng(input$player_profile_ha)
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5,6)),
                      list(targets = c(-1,-2,5,6,7), visible = FALSE)),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #PS/SO
  output$ha_player_profile_psso <- DT::renderDataTable(DT::datatable({
    
    ha_player_profile_psso(input$player_profile_ha)
    
  },
  
  extensions = 'FixedHeader',
  options = list(
    
    columnDefs = list(list(className = 'dt-center', targets = c(2,3,5))),
    
    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,
    
    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),
  
  class = 'row-border stripe compact hover nowrap',
  
  ))
  
  #Gamelog
  output$gamelog <- DT::renderDataTable(DT::datatable({

    #game_log(input$player_gamelog, input$season_gamelog)
    #game_log(player_profile_selected(), season_selected())
    game_log(player_profile_selected(), input$player_card_gamelog_season)
    
    #gamelog_download <- game_log(player_profile_selected(), season_selected())
    #gamelog <- game_log(input$player_profile, input$season_gamelog)

    # if (input$season_gamelog != "All") {
    #   gamelog <- gamelog[gamelog$Season == input$season_gamelog, ]
    # }

  },

  extensions = 'FixedHeader',
  options = list(

    pageLength = 55,

    #columnDefs = list(list(className = 'dt-center', targets = c(4,5,6,7)),
    columnDefs = list(list(className = 'dt-center', targets = c(1:24)),
                      list(targets = c(1,2,4,5), visible = FALSE)),

    order = list(list(2, 'asc')),
    #dom = 'ltipr',
    dom = 't',
    fixedHeader = TRUE,

    initComplete = JS(
      "function(settings, json) {",
      #"$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
      "$(this.api().table().header()).css({'background-color': '#035292', 'color': '#fff'});",
      "}")),

  class = 'row-border stripe compact hover nowrap',

  ))
  
  
  # output$ha_player_profile_bio1 <- renderText({
  #   
  #   ha_player_profile_bio1(input$ha_player_profile_bio1)
  #   
  # })
  # 
  # output$ha_player_profile_bio2 <- renderText({
  #   
  #   ha_player_profile_bio2(input$ha_player_profile_bio2)
  #   
  # })
  
}

enableBookmarking(store = "url")

shinyApp(ui = ui, server = server)


