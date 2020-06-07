library(shiny)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggthemes)
library(reshape2)
library(extrafont)
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyverse)
library(zoo)
#library(cowplot)

font_import()

# Data --------------------------------------------------------------------


#player_toi_data_1920 <- read.csv("player_toi_data_1920.csv", stringsAsFactors = FALSE)
#player_points_data_1920 <- read.csv("player_points_data_1920.csv", stringsAsFactors = FALSE)
#player_corsi_data_1920 <- read.csv("player_corsi_data_1920.csv", stringsAsFactors = FALSE)
#player_ev_data_1920 <- read.csv("player_ev_data_1920.csv", stringsAsFactors = FALSE)
#player_names_1920 <- read.csv("player_names_1920.csv", stringsAsFactors = FALSE)
#box_score_data_1920 <- read.csv("box_score_data_1920_condensed.csv", stringsAsFactors = FALSE)

#sort(player_names_1920$x) -> player_names_1920

#new master data
player_names_master <- read.csv("player_names_master.csv", stringsAsFactors = FALSE)
player_toi_data_master <- read.csv("player_toi_data_master.csv", stringsAsFactors = FALSE)
player_points_data_master <- read.csv("player_points_data_master.csv", stringsAsFactors = FALSE)
player_corsi_data_master <- read.csv("player_corsi_data_master.csv", stringsAsFactors = FALSE)
box_score_data_master <- read.csv("box_score_data_master.csv", stringsAsFactors = FALSE)

sort(player_names_master$x) -> player_names_master

# Player Card Function ----------------------------------------------------

player_card3 <- function(player_name, season_year) {
  
  
  #for plot title - identify which teams a player played for
  #ends up being plugged into the TOI visual
  teams <- player_toi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
    select(swehockey_name, shlse_team_name, game_number)
  
  teams <- teams[order(teams$game_number),]
  teams_list <- unique(teams$shlse_team_name)
  
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
    rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
  
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
    labs(title = paste(player_name, " ", season_year, " Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
         subtitle = paste("TOI by Game Situation \n",
                          round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
                          round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
                          round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = '')) +
    
    theme_few() +
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
  
  if(length(teams_list) == 2){
    toi_visual <- toi_visual + labs(title = paste(player_name, " ", season_year, " Season | ", teams_list[1], ", ", teams_list[2], " | ", NROW(table_toi$ES), ' GP',  sep = ''),
                                    subtitle = paste("TOI by Game Situation \n",
                                                     round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
                                                     round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
                                                     round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = ''))
  }
  
  if(length(teams_list) == 3){
    toi_visual <- toi_visual + labs(title = paste(player_name, " ", season_year, " Season | ", teams_list[1], ", ", teams_list[2], ", ", teams_list[3], " | ", NROW(table_toi$ES), ' GP',  sep = ''),
                                    subtitle = paste("TOI by Game Situation \n",
                                                     round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
                                                     round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
                                                     round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = ''))
  }
  
  
  #Points
  y_points_axis <- player_points_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year)
      
  table_points <- player_points_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
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
    theme(axis.title.x = element_blank()) 
  
  #if someone played 10 or fewer games
  if(NROW(table_toi$ES) < 11){
    
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
                          sum(y_goals_axis$GA_abs), ' GA ',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_swe'), sep = '') +
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0))
  
  # if the axis needs to be expanded
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
    
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
      labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_swe'), sep = '') +
      theme_few() +
      theme(text = element_text(family = "Sweden Sans")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      #red, black
      scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
      scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-3, 3), expand = c(0, 0))
    
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
  
  #use cowplot to align the plots now
  #g <- cowplot::plot_grid(g2, g5, g3, g4, ncol = 1, align = 'v', axis = 'lr')
  
  return(g)
  
}

player_card4 <- function(player_name, season_year) {
  
  
  #for plot title - identify which teams a player played for
  #ends up being plugged into the TOI visual
  teams <- player_toi_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
    select(swehockey_name, shlse_team_name, game_number)
  
  teams <- teams[order(teams$game_number),]
  teams_list <- unique(teams$shlse_team_name)
  
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
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) 
  
  #but for players with games > 20 minutes, this is where the y axis table comes in
  if(max(y_toi_axis$TOI) > 20){
    toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
                                                  expand = c(0, 0))
  }
  
  if(length(teams_list) == 2){
    toi_visual <- toi_visual + labs(title = paste(player_name, " ", season_year, " Season | ", teams_list[1], ", ", teams_list[2], " | ", NROW(table_toi$ES), ' GP',  sep = ''),
                                    subtitle = paste("TOI by Game Situation \n",
                                                     round(sum(table_toi$EV)/NROW(table_toi$EV), 1), ' EV  -  ',
                                                     round(sum(table_toi$PP)/NROW(table_toi$EV), 1), ' PP  -  ',
                                                     round(sum(table_toi$SH)/NROW(table_toi$EV), 1), ' SH', sep = ''))
  }
  
  if(length(teams_list) == 3){
    toi_visual <- toi_visual + labs(title = paste(player_name, " ", season_year, " Season | ", teams_list[1], ", ", teams_list[2], ", ", teams_list[3], " | ", NROW(table_toi$ES), ' GP',  sep = ''),
                                    subtitle = paste("TOI by Game Situation \n",
                                                     round(sum(table_toi$EV)/NROW(table_toi$EV), 1), ' EV  -  ',
                                                     round(sum(table_toi$PP)/NROW(table_toi$EV), 1), ' PP  -  ',
                                                     round(sum(table_toi$SH)/NROW(table_toi$EV), 1), ' SH', sep = ''))
  }
  
  
  #Points
  game_number <- tibble(c(1:52)) %>% set_names('number')
  
  table_points_old <- player_points_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year) %>%
    select(game_number, G, A1, A2)
  
  y_points_axis <- player_points_data_master %>%
    filter(swehockey_name == player_name) %>%
    filter(season == season_year)
  
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
    guides(fill = guide_legend(reverse = TRUE)) 
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
    theme(axis.title.x = element_blank()) 
  
  #if someone played 10 or fewer games
  if(NROW(table_toi$EV) < 11){
    
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
                          sum(y_goals_axis$GA_abs), ' GA ',
                          #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
                          sep = '')) +
    labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_swe'), sep = '') +
    
    theme_few() +
    theme(text = element_text(family = "Sweden Sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0))
  
  # if the axis needs to be expanded
  if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
    goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
                                                      expand = c(0, 0))
    
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
      labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_swe'), sep = '') +
      theme_few() +
      theme(text = element_text(family = "Sweden Sans")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      #red, black
      scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
      scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-3, 3), expand = c(0, 0))
    
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
  
  #use cowplot to align the plots now
  #g <- cowplot::plot_grid(g2, g5, g3, g4, ncol = 1, align = 'v', axis = 'lr')
  
  return(g)
  
}

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel("SHL Game-by-Game Player Cards"),
  sidebarLayout(
    sidebarPanel(
      
      
      
      selectInput("playerName", "Player",
                  choices = as.list(player_names_master[1:length(player_names_master)]),
                  #selected = player_names_1920[1],
                  selected = 'Nils Lundkvist',
                  width = "250px")
      
      ,

      selectInput("season", "Season",
                  choices = c('17/18', '18/19', '19/20'),
                  selected = '19/20',
                  width = '250px')
      
    ),
    mainPanel(
      plotOutput("playerCard")
      #tableOutput("toiData")
    )
  )
)

# Server ------------------------------------------------------------------


server <- function(input, output) {
  
  
  output$playerCard <- renderPlot({
    
    #player_card2(input$playerName)
    
    player_card4(input$playerName, input$season)
    
  },
  
  
  height = 770, width = 740
  
  )
  
}


# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)


# Messing Around ----------------------------------------------------------


# Old ---------------------------------------------------------------------



# player_card <- function(player_name) {
#   
#   #TOI
#   
#   teams <- player_toi_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     select(swehockey_name, shlse_team_name, game_number)
#   teams <- teams[order(teams$game_number),]
#   teams_list <- unique(teams$shlse_team_name)
#   
#   #create one-off table to expand y axis as needed
#   #will use this later to expand the y-axis if needed
#   y_toi_axis <- player_toi_data_1920 %>%
#     filter(swehockey_name == player_name)
#   
#   #pull relevant data from master
#   table_toi <- player_toi_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     select(game_number, ESTOI, PPTOI, SHTOI) %>%
#     #rename it for what I want to display in the legend
#     rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
#   
#   #melt so can plot a stacked column (only way to create stacked column?)
#   table_toi2 <- melt(table_toi, id.var="game_number") %>%
#     rename(GameNumber = game_number, Situation = variable, TOI = value)
#   
#   #create the order of those stacked components
#   #don't fully understand this one - needed to create the order I want the stacking in
#   levels(table_toi2$Situation)
#   table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
#   
#   #ggplot the toi visual
#   toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),], 
#                        aes(x = GameNumber, y = TOI, fill = Situation)) +
#     geom_bar(stat = "identity") +
#     
#     #creating the title for the overall plot, plus the TOI subheader data
#     labs(title = paste(player_name, " 19/20 Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
#          subtitle = paste("TOI by Game Situation \n",
#                           round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
#                           round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
#                           round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = '')) +
#    
#     theme_few() +
#     #use the font I loaded in specifically for this visual
#     theme(text = element_text(family = "Sweden Sans")) +
#     theme(plot.title = element_text(face = "bold")) +
#     theme(axis.title.x = element_blank()) +
#     scale_fill_manual(values=c("#FF7F7F", "#FFD17F", "#7F7FFF")) +
#     #x axis spans entire 52 game season
#     scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
#     #default y axis goes from 0 to 20 minutes
#     scale_y_continuous(limits = c(0, 20), expand = c(0, 0))
#   
#   #but for players with games > 20 minutes, this is where the y axis table comes in
#   if(max(y_toi_axis$TOI) > 20){
#     toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
#                                                   expand = c(0, 0))
#   }
# 
#   if(length(teams_list) == 2){
#     toi_visual <- toi_visual + labs(title = paste(player_name, " 19/20 Season | ", teams_list[1], ", ", teams_list[2], " | ", NROW(table_toi$ES), ' GP',  sep = ''),
#                                     subtitle = paste("TOI by Game Situation \n",
#                                                      round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
#                                                      round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
#                                                      round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = ''))
#   }
#   
#   if(length(teams_list) == 3){
#     toi_visual <- toi_visual + labs(title = paste(player_name, " 19/20 Season | ", teams_list[1], ", ", teams_list[2], ", ", teams_list[3], " | ", NROW(table_toi$ES), ' GP',  sep = ''),
#                                     subtitle = paste("TOI by Game Situation \n",
#                                                      round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
#                                                      round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
#                                                      round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = ''))
#   }
#   
#   
#   #Player point production
#   #all same as above
#   y_points_axis <- player_points_data_1920 %>%
#     filter(swehockey_name == player_name)
#   
#   table_points <- player_points_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     select(game_number, G, A1, A2)
#   
#   table_points_2 <- melt(table_points, id.var="game_number") %>%
#     rename(GameNumber = game_number, PointType = variable, Points = value)
#   
#   levels(table_points_2$PointType)
#   table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
#   
#   points_visual <- ggplot(table_points_2[order(table_points_2$PointType),], 
#                           aes(x = GameNumber, y = Points, fill = PointType)) +
#     geom_bar(stat = "identity") +
#     labs(subtitle = paste("Point Production \n",
#                           sum(table_points$G), ' G  -  ',
#                           sum(table_points$A1), ' A1  -  ',
#                           sum(table_points$A2), ' A2  (',
#                           
#                           round((sum(y_points_axis$TP) / sum(y_toi_axis$TOI)) * 60, 2),
#                           
#                           ' P/60)',
#                           
#                           sep = '')) +
#     theme_few() +
#     theme(text = element_text(family = "Sweden Sans")) +
#     theme(axis.title.x = element_blank()) +
#     scale_fill_manual(values=c("#CCE4CC", "#B2B2FF", "#7F7FFF")) +
#     scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
#     scale_y_continuous(limits = c(0, 3), expand = c(0, 0))
#   
#   
#   
#   
#   if(max(y_points_axis$TP) > 3){
#     points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_points_axis$TP)),
#                                                         expand = c(0, 0))
#   }
#   
#   #corsi (158 - 207)
#   y_corsi_axis <- player_corsi_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     #select(gameNumber, CF, CA) %>%
#     mutate(CA2 = CA * -1) %>%
#     select(-c(CA)) %>%
#     rename(CA = CA2) %>%
#     mutate(CA_abs = abs(CA))
# 
#   #storing values to calculate stats for subheader (CF%, relative CF%)
#   player_corsi <- round((sum(y_corsi_axis$CF) / (sum(y_corsi_axis$CF) + sum(y_corsi_axis$CA_abs)) *100), 1)
#   player_off_corsi <- round((sum(y_corsi_axis$CF_off) / (sum(y_corsi_axis$CF_off) + sum(y_corsi_axis$CA_off)) *100), 1)
#   CF_rel <- player_corsi - player_off_corsi
# 
#   table_corsi <- player_corsi_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     select(game_number.x, CF, CA) %>%
#     mutate(CA2 = CA * -1) %>%
#     select(-c(CA)) %>%
#     rename(CA = CA2)
# 
#   table_corsi_2 <- melt(table_corsi, id.var="game_number.x") %>%
#     rename(GameNumber = game_number.x, ShotType = variable, Shots = value)
# 
#   levels(table_corsi_2$Type)
#   table_corsi_2$ShotType <- factor(table_corsi_2$ShotType, levels = rev(levels(table_corsi_2$ShotType)))
# 
#   corsi_visual <- ggplot(table_corsi_2[order(table_corsi_2$ShotType),],
#                          aes(x = GameNumber, y = Shots, fill = ShotType)) +
#     geom_bar(stat = "identity") +
#     labs(subtitle = paste("Corsi For and Against \n",
#                           round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1),
#                           ' CF%',
# 
#                           ' (', round(CF_rel, 1),' rel)', sep = '')) +
#     theme_few() +
#     theme(text = element_text(family = "Sweden Sans")) +
#     theme(axis.title.x = element_blank()) +
#     #flip the order of the legend so CF is on top and CA is on the bottom
#     guides(fill = guide_legend(reverse = TRUE)) +
#     #red, black
#     scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
#     scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
#     scale_y_continuous(limits = c(-20, 20), expand = c(0, 0))
# 
# 
#   if(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) > 20){
#     corsi_visual <- corsi_visual + scale_y_continuous(limits=c(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) * -1, max(y_corsi_axis$CF,y_corsi_axis$CA_abs)),
#                                                       expand = c(0, 0))
#   }
# 
#   #Shot Rates
#   # game_number <- tibble(c(1:52)) %>% set_names('number')
#   # 
#   # shot_data <-
#   #   left_join(player_corsi_data_1920, player_toi_data_1920, by = "name_date") %>%
#   #   filter(swehockey_name.x == player_name) %>%
#   #   select(game_number.x, CF, CA, ESTOI) %>%
#   #   mutate(player_game_number = row_number()) %>%
#   #   
#   #   mutate(CFL5 = ifelse(player_game_number >= 5,
#   #                        rollsumr(CF, 5, fill = NA),
#   #                        0)) %>%
#   #   mutate(CAL5 = ifelse(player_game_number >= 5,
#   #                        rollsumr(CA, 5, fill = NA),
#   #                        0)) %>%
#   #   mutate(ESTOIL5 = ifelse(player_game_number >= 5,
#   #                           rollsumr(ESTOI, 5, fill = NA),
#   #                           0)) %>%
#   #   
#   #   mutate(CF60 = round((CFL5 / ESTOIL5) * 60, 1)) %>%
#   #   mutate(CA60 = round((CAL5 / ESTOIL5) * 60, 1))
#   # 
#   # 
#   # shot_data %>%
#   #   select(game_number.x, CF60, CA60) -> shot_data_condensed
#   # 
#   # #shot_data_condensed[is.na(shot_data_condensed)] <- 0
#   # 
#   # left_join(game_number, shot_data_condensed, by = c("number" = "game_number.x")) -> shot_data_condensed
#   # 
#   # melt_test <- reshape2::melt(shot_data_condensed, id.var = "number") %>%
#   #   rename(ShotType = variable, Shots = value)
#   # 
#   # shot_rate_visual <- ggplot(shot_data_condensed, aes(x = number, y = `Shot Rates`)) +
#   #   geom_line(aes(y = CF60), color = "#7F7F7F", size = 1) +
#   #   geom_line(aes(y = CA60), color = "#FF7F7E", size = 1) +
#   #   theme_few() +
#   #   theme(text = element_text(family = "Sweden Sans")) +
#   #   theme(axis.title.x = element_blank()) +
#   #   scale_x_continuous(limits = c(0, 53), expand = c(0, 0)) + 
#   #   scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
#   #   labs(subtitle = paste("Shot Rates For and Against \n",
#   #                     round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1),
#   #                           ' CF%',
#   # 
#   #                           ' (', round(CF_rel, 1),' rel)', sep = ''))
#   
#   #Goals
#   game_numbers <- tibble(c(1:52)) %>%
#     set_names("game_number")
#   
#   #for people on for no goals
#   empty_goals <- tibble(game_number = c(1:52), GF = 0, GA = 0)
#   
#   empty_goals_2 <- melt(empty_goals, id.var = "game_number") %>%
#     rename(GameNumber = game_number, GoalType = variable, Goals = value)
#   
#   levels(empty_goals_2$GoalType)
#   empty_goals_2$GoalType <- factor(empty_goals_2$GoalType, levels = rev(levels(empty_goals_2$GoalType)))
#   #end of people on for no goals
#   
#   GF_data <- box_score_data_1920 %>%
#     subset(grepl(player_name, GF_names)) %>%
#     filter(game_situation == '5v5') %>%
#     select(GF_names, gf_team_game_number) %>%
#     mutate(count = 1) %>%
#     select(-c(GF_names)) %>%
#     group_by(gf_team_game_number) %>%
#     summarise(GF = sum(count))
#   
#   GA_data <- box_score_data_1920 %>%
#     subset(grepl(player_name, GA_names)) %>%
#     filter(game_situation == '5v5') %>%
#     select(GA_names, ga_team_game_number) %>%
#     mutate(count = 1) %>%
#     select(-c(GA_names)) %>%
#     group_by(ga_team_game_number) %>%
#     summarise(GA = sum(count))
#   
#   y_goals_axis <- merge(game_numbers, GF_data, by.x = "game_number",
#                         by.y = "gf_team_game_number", all.x = TRUE)
#   
#   y_goals_axis <- merge(y_goals_axis, GA_data, by.x = "game_number",
#                         by.y = "ga_team_game_number", all.x = TRUE)
#   
#   y_goals_axis <- y_goals_axis %>%
#     mutate(GA2 = GA * -1) %>%
#     select(-c(GA)) %>%
#     rename(GA = GA2) %>%
#     mutate(GA_abs = abs(GA))
#   
#   table_goals <- y_goals_axis %>%
#     select(-c(GA_abs))
#   
#   table_goals_2 <- melt(table_goals, id.var = "game_number") %>%
#     rename(GameNumber = game_number, GoalType = variable, Goals = value)
#   
#   levels(table_goals_2$GoalType)
#   table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))
#   
#   table_goals[is.na(table_goals)] <- 0
#   y_goals_axis[is.na(y_goals_axis)] <- 0
#     
#   goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),],
#                          aes(x = GameNumber, y = Goals, fill = GoalType)) +
#     geom_bar(stat = "identity") +
#     labs(subtitle = paste('5v5 Goals For and Against \n',
#                           sum(table_goals$GF), ' GF  -  ',
#                           sum(y_goals_axis$GA_abs), ' GA ',
#                           #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
#                           sep = '')) +
#     labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_DIF'), sep = '') +
#     theme_few() +
#     theme(text = element_text(family = "Sweden Sans")) +
#     guides(fill = guide_legend(reverse = TRUE)) +
#     #red, black
#     scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
#     scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
#     scale_y_continuous(limits = c(-3, 3), expand = c(0, 0))
# 
# # if the axis needs to be expanded
#   if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
#     goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
#                                                       expand = c(0, 0))
#     
#   }
#   
# #if the player hasn't been on for a goal
# if(sum(y_goals_axis$GF) + sum(y_goals_axis$GA_abs) == 0) {
# 
#   goals_visual <- ggplot(empty_goals_2[order(empty_goals_2$GoalType),],
#                          aes(x = GameNumber, y = Goals, fill = GoalType)) +
#     geom_bar(stat = "identity") +
#     labs(subtitle = paste('5v5 Goals For and Against \n',
#                           0, ' GF  -  ',
#                           0, ' GA ',
#                           #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
#                           sep = '')) +
#     labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_DIF'), sep = '') +
#     theme_few() +
#     theme(text = element_text(family = "Sweden Sans")) +
#     guides(fill = guide_legend(reverse = TRUE)) +
#     #red, black
#     scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
#     scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
#     scale_y_continuous(limits = c(-3, 3), expand = c(0, 0))
# 
# }
#   
#   
#   #using the ggpubr package so that I can align all the ggplots on one visual
#   #and so all the x axes align
#   g2 <- ggplotGrob(toi_visual)
#   g3 <- ggplotGrob(corsi_visual)
#   #g3 <- ggplotGrob(shot_rate_visual)
#   g4 <- ggplotGrob(goals_visual)
#   g5 <- ggplotGrob(points_visual)
#   g <- rbind(g2, g5, g3, g4, size = "first")
#   g$widths <- unit.pmax(g2$widths, g5$widths, g3$widths, g4$widths)
#   grid.newpage()
#   grid.draw(g)
#   
#   #aligned <- cowplot::align_plots(plotlist = c(g2, g5, g3, g4), align = "v")
#   
#   #aligned = cowplot::align_plots(plotlist = df$plots, align = 'v')
#   
#   
#   return(g)
#   #return(aligned)
#   
# }



# player_card2 <- function(player_name) {
#   
#   
#   #for plot title - identify which teams a player played for
#   #ends up being plugged into the TOI visual
#   teams <- player_toi_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     select(swehockey_name, shlse_team_name, game_number)
#   
#   teams <- teams[order(teams$game_number),]
#   teams_list <- unique(teams$shlse_team_name)
#   
#   #TOI
#   #create one-off table to expand y axis as needed - will use this later
#   y_toi_axis <- player_toi_data_1920 %>%
#     filter(swehockey_name == player_name)
#   
#   #pull relevant data from master
#   table_toi <- player_toi_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     select(game_number, ESTOI, PPTOI, SHTOI) %>%
#     #rename it for what displays in the legend
#     rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
#   
#   #melt so can plot a stacked column (only way to create stacked column?)
#   table_toi2 <- melt(table_toi, id.var="game_number") %>%
#     rename(GameNumber = game_number, Situation = variable, TOI = value)
#   
#   #create the order of those stacked components - don't fully understand why this is necessary
#   levels(table_toi2$Situation)
#   table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
#   
#   #ggplot for the toi visual
#   toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),], 
#                        aes(x = GameNumber, y = TOI, fill = Situation)) +
#     geom_bar(stat = "identity") +
#     
#     #creating the title for the overall plot, plus the TOI subheader data
#     labs(title = paste(player_name, " 19/20 Season | ", y_toi_axis$shlse_team_name, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
#          subtitle = paste("TOI by Game Situation \n",
#                           round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
#                           round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
#                           round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = '')) +
#     
#     theme_few() +
#     theme(text = element_text(family = "Sweden Sans")) +
#     theme(plot.title = element_text(face = "bold")) +
#     theme(axis.title.x = element_blank()) +
#     scale_fill_manual(values=c("#FF7F7F", "#FFD17F", "#7F7FFF")) +
#     #x axis spans entire 52 game season
#     scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
#     #default y axis goes from 0 to 20 minutes
#     scale_y_continuous(limits = c(0, 20), expand = c(0, 0))
#   
#   #but for players with games > 20 minutes, this is where the y axis table comes in
#   if(max(y_toi_axis$TOI) > 20){
#     toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_toi_axis$TOI) + 1),
#                                                   expand = c(0, 0))
#   }
#   
#   if(length(teams_list) == 2){
#     toi_visual <- toi_visual + labs(title = paste(player_name, " 19/20 Season | ", teams_list[1], ", ", teams_list[2], " | ", NROW(table_toi$ES), ' GP',  sep = ''),
#                                     subtitle = paste("TOI by Game Situation \n",
#                                                      round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
#                                                      round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
#                                                      round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = ''))
#   }
#   
#   if(length(teams_list) == 3){
#     toi_visual <- toi_visual + labs(title = paste(player_name, " 19/20 Season | ", teams_list[1], ", ", teams_list[2], ", ", teams_list[3], " | ", NROW(table_toi$ES), ' GP',  sep = ''),
#                                     subtitle = paste("TOI by Game Situation \n",
#                                                      round(sum(table_toi$ES)/NROW(table_toi$ES), 1), ' ES  -  ',
#                                                      round(sum(table_toi$PP)/NROW(table_toi$ES), 1), ' PP  -  ',
#                                                      round(sum(table_toi$SH)/NROW(table_toi$ES), 1), ' SH', sep = ''))
#   }
#   
#   
#   #Points
#   y_points_axis <- player_points_data_1920 %>%
#     filter(swehockey_name == player_name)
#   
#   table_points <- player_points_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     select(game_number, G, A1, A2)
#   
#   table_points_2 <- melt(table_points, id.var="game_number") %>%
#     rename(GameNumber = game_number, PointType = variable, Points = value)
#   
#   levels(table_points_2$PointType)
#   table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
#   
#   points_visual <- ggplot(table_points_2[order(table_points_2$PointType),], 
#                           aes(x = GameNumber, y = Points, fill = PointType)) +
#     geom_bar(stat = "identity") +
#     
#     labs(subtitle = paste("Point Production \n",
#                           sum(table_points$G), ' G  -  ',
#                           sum(table_points$A1), ' A1  -  ',
#                           sum(table_points$A2), ' A2  (',
#                           
#                           round((sum(y_points_axis$TP) / sum(y_toi_axis$TOI)) * 60, 2),
#                           
#                           ' P/60)',
#                           
#                           sep = '')) +
#     theme_few() +
#     theme(text = element_text(family = "Sweden Sans")) +
#     theme(axis.title.x = element_blank()) +
#     scale_fill_manual(values=c("#CCE4CC", "#B2B2FF", "#7F7FFF")) +
#     scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
#     scale_y_continuous(limits = c(0, 3), expand = c(0, 0))
#   
#   if(max(y_points_axis$TP) > 3){
#     points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_points_axis$TP)),
#                                                         expand = c(0, 0))
#   }
#   
#   #Corsi
#   y_corsi_axis <- player_corsi_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     #select(gameNumber, CF, CA) %>%
#     mutate(CA2 = CA * -1) %>%
#     select(-c(CA)) %>%
#     rename(CA = CA2) %>%
#     mutate(CA_abs = abs(CA))
#   
#   #storing values to calculate stats for subheader (CF%, relative CF%)
#   player_corsi <- round((sum(y_corsi_axis$CF) / (sum(y_corsi_axis$CF) + sum(y_corsi_axis$CA_abs)) *100), 1)
#   player_off_corsi <- round((sum(y_corsi_axis$CF_off) / (sum(y_corsi_axis$CF_off) + sum(y_corsi_axis$CA_off)) *100), 1)
#   CF_rel <- player_corsi - player_off_corsi
#   
#   table_corsi <- player_corsi_data_1920 %>%
#     filter(swehockey_name == player_name) %>%
#     select(game_number.x, CF, CA) %>%
#     mutate(CA2 = CA * -1) %>%
#     select(-c(CA)) %>%
#     rename(CA = CA2)
#   
#   game_number <- tibble(c(1:52)) %>% set_names('number')
#   
#   shot_data <- 
#     left_join(player_corsi_data_1920, player_toi_data_1920, by = "name_date") %>%
#     filter(swehockey_name.x == player_name) %>%
#     select(game_number.x, CF, CA, ESTOI) %>%
#     mutate(player_game_number = row_number()) %>%
#     
#     mutate(CFL5 = ifelse(player_game_number >= 5,
#                          rollsumr(CF, 5, fill = NA),
#                          0)) %>%
#     mutate(CAL5 = ifelse(player_game_number >= 5,
#                          rollsumr(CA, 5, fill = NA),
#                          0)) %>%
#     mutate(ESTOIL5 = ifelse(player_game_number >= 5,
#                             rollsumr(ESTOI, 5, fill = NA),
#                             0)) %>%
#     
#     mutate(CF60 = round((CFL5 / ESTOIL5) * 60, 1)) %>%
#     mutate(CA60 = round((CAL5 / ESTOIL5) * 60, 1))
#   
#   shot_data %>%
#     select(game_number.x, CF60, CA60) -> shot_data_condensed
#   
#   left_join(game_number, shot_data_condensed, by = c("number" = "game_number.x")) -> shot_data_condensed
#   
#   shot_data_condensed <- reshape2::melt(shot_data_condensed, id.var = "number") %>%
#     rename(ShotType = variable, Shots = value)
#   
#   corsi_visual <- ggplot(shot_data_condensed, aes(x = number, y = Shots, color = `ShotType`)) +
#     geom_line(size = 1.25) +
#     geom_line(aes(y = 53.7), color = "#B2B2FF", size = 0.66, linetype = 'dotted') +
#     
#     labs(subtitle = paste("Shot Rates Per 60, 5-Game Moving Avg. \n",
#                           
#                           # round((sum(shot_data$CF) / sum(shot_data$ESTOI)) * 60, 1), ' CF60, ',
#                           # 
#                           # round((sum(shot_data$CA) / sum(shot_data$ESTOI)) * 60, 1), ' CA60, ',
#                           
#                           round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1),
#                           ' CF%',
#                           
#                           ' (', round(CF_rel, 1),' rel)', sep = '')) +
#     
#     
#     theme_few() +
#     theme(text = element_text(family = "Sweden Sans")) +
#     guides(fill = guide_legend(reverse = TRUE)) +
#     #red, black
#     scale_color_manual(values=c("#7F7F7F", "#FE7F7E")) +
#     scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
#     scale_y_continuous(limits = c(20,80), expand = c(0, 0)) +
#     theme(axis.title.x = element_blank()) 
#   
#   #if someone played 10 or fewer games
#   if(NROW(table_toi$ES) < 11){
#     
#     table_corsi_2 <- melt(table_corsi, id.var="game_number.x") %>%
#       rename(GameNumber = game_number.x, ShotType = variable, Shots = value)
#     
#     levels(table_corsi_2$Type)
#     table_corsi_2$ShotType <- factor(table_corsi_2$ShotType, levels = rev(levels(table_corsi_2$ShotType)))
#     
#     corsi_visual <- ggplot(table_corsi_2[order(table_corsi_2$ShotType),],
#                            aes(x = GameNumber, y = Shots, fill = ShotType)) +
#       geom_bar(stat = "identity") +
#       
#       labs(subtitle = paste("Corsi For and Against \n",
#                             round((sum(table_corsi$CF) / (sum(table_corsi$CF) + sum(y_corsi_axis$CA_abs)) *100), 1),
#                             ' CF%',
#                             
#                             ' (', round(CF_rel, 1),' rel)', sep = '')) +
#       
#       theme_few() +
#       theme(text = element_text(family = "Sweden Sans")) +
#       theme(axis.title.x = element_blank()) +
#       #flip the order of the legend so CF is on top and CA is on the bottom
#       guides(fill = guide_legend(reverse = TRUE)) +
#       #red, black
#       scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
#       scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
#       scale_y_continuous(limits = c(-20, 20), expand = c(0, 0))
#     
#     if(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) > 20){
#       corsi_visual <- corsi_visual + scale_y_continuous(limits=c(max(y_corsi_axis$CF, y_corsi_axis$CA_abs) * -1, max(y_corsi_axis$CF,y_corsi_axis$CA_abs)),
#                                                         expand = c(0, 0))
#     }
#   }
#   
#   #Goals
#   #creating an empty table - will merge goal data in later
#   game_numbers <- tibble(c(1:52)) %>%
#     set_names("game_number")
#   
#   #for people on for no goals
#   empty_goals <- tibble(game_number = c(1:52), GF = 0, GA = 0)
#   
#   empty_goals_2 <- melt(empty_goals, id.var = "game_number") %>%
#     rename(GameNumber = game_number, GoalType = variable, Goals = value)
#   
#   levels(empty_goals_2$GoalType)
#   empty_goals_2$GoalType <- factor(empty_goals_2$GoalType, levels = rev(levels(empty_goals_2$GoalType)))
#   #end of people on for no goals
#   
#   GF_data <- box_score_data_1920 %>%
#     subset(grepl(player_name, GF_names)) %>%
#     filter(game_situation == '5v5') %>%
#     select(GF_names, gf_team_game_number) %>%
#     mutate(count = 1) %>%
#     select(-c(GF_names)) %>%
#     group_by(gf_team_game_number) %>%
#     summarise(GF = sum(count))
#   
#   GA_data <- box_score_data_1920 %>%
#     subset(grepl(player_name, GA_names)) %>%
#     filter(game_situation == '5v5') %>%
#     select(GA_names, ga_team_game_number) %>%
#     mutate(count = 1) %>%
#     select(-c(GA_names)) %>%
#     group_by(ga_team_game_number) %>%
#     summarise(GA = sum(count))
#   
#   y_goals_axis <- merge(game_numbers, GF_data, by.x = "game_number",
#                         by.y = "gf_team_game_number", all.x = TRUE)
#   
#   y_goals_axis <- merge(y_goals_axis, GA_data, by.x = "game_number",
#                         by.y = "ga_team_game_number", all.x = TRUE)
#   
#   y_goals_axis <- y_goals_axis %>%
#     mutate(GA2 = GA * -1) %>%
#     select(-c(GA)) %>%
#     rename(GA = GA2) %>%
#     mutate(GA_abs = abs(GA))
#   
#   table_goals <- y_goals_axis %>%
#     select(-c(GA_abs))
#   
#   table_goals_2 <- melt(table_goals, id.var = "game_number") %>%
#     rename(GameNumber = game_number, GoalType = variable, Goals = value)
#   
#   levels(table_goals_2$GoalType)
#   table_goals_2$GoalType <- factor(table_goals_2$GoalType, levels = rev(levels(table_goals_2$GoalType)))
#   
#   table_goals[is.na(table_goals)] <- 0
#   y_goals_axis[is.na(y_goals_axis)] <- 0
#   
#   goals_visual <- ggplot(table_goals_2[order(table_goals_2$GoalType),],
#                          aes(x = GameNumber, y = Goals, fill = GoalType)) +
#     geom_bar(stat = "identity") +
#     
#     labs(subtitle = paste('5v5 Goals For and Against \n',
#                           sum(table_goals$GF), ' GF  -  ',
#                           sum(y_goals_axis$GA_abs), ' GA ',
#                           #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
#                           sep = '')) +
#     labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_swe'), sep = '') +
#     
#     theme_few() +
#     theme(text = element_text(family = "Sweden Sans")) +
#     guides(fill = guide_legend(reverse = TRUE)) +
#     #red, black
#     scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
#     scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
#     scale_y_continuous(limits = c(-3, 3), expand = c(0, 0))
#   
#   # if the axis needs to be expanded
#   if(max(y_goals_axis$GF, y_goals_axis$GA_abs) > 3){
#     goals_visual <- goals_visual + scale_y_continuous(limits=c(max(y_goals_axis$GF, y_goals_axis$GA_abs) * -1, max(y_goals_axis$GF,y_goals_axis$GA_abs)),
#                                                       expand = c(0, 0))
#     
#   }
#   
#   #if the player hasn't been on for a goal
#   if(sum(y_goals_axis$GF) + sum(y_goals_axis$GA_abs) == 0) {
#     
#     goals_visual <- ggplot(empty_goals_2[order(empty_goals_2$GoalType),],
#                            aes(x = GameNumber, y = Goals, fill = GoalType)) +
#       geom_bar(stat = "identity") +
#       labs(subtitle = paste('5v5 Goals For and Against \n',
#                             0, ' GF  -  ',
#                             0, ' GA ',
#                             #round(sum(table_goals$GF)/(sum(table_goals$GF) +  sum(y_goals_axis$GA_abs)), 2), ' GF% ',
#                             sep = '')) +
#       labs(caption = paste('data from shl.se & stats.swehockey.se', ' | ', '@zellenthal_swe'), sep = '') +
#       theme_few() +
#       theme(text = element_text(family = "Sweden Sans")) +
#       guides(fill = guide_legend(reverse = TRUE)) +
#       #red, black
#       scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
#       scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
#       scale_y_continuous(limits = c(-3, 3), expand = c(0, 0))
#     
#   }
#   
#   #aliases for each of the visuals
#   g2 <- ggplotGrob(toi_visual)
#   g3 <- ggplotGrob(corsi_visual)
#   g4 <- ggplotGrob(goals_visual)
#   g5 <- ggplotGrob(points_visual)
#   #old method - this broke after updating corsi visual to rolling average
#   g <- rbind(g2, g5, g3, g4, size = "first")
#   g$widths <- unit.pmax(g2$widths, g5$widths, g3$widths, g4$widths)
#   grid.newpage()
#   grid.draw(g)
#   
#   #use cowplot to align the plots now
#   #g <- cowplot::plot_grid(g2, g5, g3, g4, ncol = 1, align = 'v', axis = 'lr')
#   
#   return(g)
#   
# }



