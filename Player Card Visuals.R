install.packages("ggpubr")
library(ggpubr)

install.packages("extrafont")
library(extrafont)

font_import()


#Full player card
#This function will create the full player card, creating four graphs
#and plotting them all together
player_card <- function(player_name) {
  
  #TOI
  #create one-off table to expand y axis as needed
  #will use this later to expand the y-axis if needed
  y_toi_axis <- master_player_toi_data %>%
    filter(name == player_name)
  
  #pull relevant data from master
  table_toi <- master_player_toi_data %>%
    filter(name == player_name) %>%
    select(gameNumber, ESTOI, PPTOI, SHTOI) %>%
    #rename it for what I want to display in the legend
    rename(ES = ESTOI, PP = PPTOI, SH = SHTOI)
  
  #melt so can plot a stacked column (only way to create stacked column?)
  table_toi2 <- melt(table_toi, id.var="gameNumber") %>%
    rename(GameNumber = gameNumber, Situation = variable, TOI = value)
  
  #create the order of those stacked components
  #don't fully understand this one - needed to create the order I want the stacking in
  levels(table_toi2$Situation)
  table_toi2$Situation <- factor(table_toi2$Situation, levels = rev(levels(table_toi2$Situation)))
  
  #ggplot the toi visual
  toi_visual <- ggplot(table_toi2[order(table_toi2$Situation),], 
                       aes(x = GameNumber, y = TOI, fill = Situation)) +
    geom_bar(stat = "identity") +
    
    #creating the title for the overall plot, plus the TOI subheader data
    labs(title = paste(player_name, " 18/19 Season | ", y_toi_axis$shlName, " | ", NROW(table_toi$ES), ' GP',  sep = ''),
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
  y_points_axis <- master_player_points_data %>%
    filter(name == player_name)
  
  table_points <- master_player_points_data %>%
    filter(name == player_name) %>%
    select(gameNumber, G, A1, A2)
  
  table_points_2 <- melt(table_points, id.var="gameNumber") %>%
    rename(GameNumber = gameNumber, PointType = variable, Points = value)
  
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
    y_corsi_axis <- master_player_corsi_data %>%
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
    
    table_corsi <- master_player_corsi_data %>%
      filter(name == player_name) %>%
      select(gameNumber, CF, CA) %>%
      mutate(CA2 = CA * -1) %>%
      select(-c(CA)) %>%
      rename(CA = CA2)
    
    table_corsi_2 <- melt(table_corsi, id.var="gameNumber") %>%
      rename(GameNumber = gameNumber, ShotType = variable, Shots = value)
    
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
    y_goals_axis <- master_player_ev_data %>%
      filter(name == player_name) %>%
      select(gameNumber, GF, GA) %>%
      mutate(GA2 = GA * -1) %>%
      select(-c(GA)) %>%
      rename(GA = GA2) %>%
      mutate(GA_abs = abs(GA))
    
    table_goals <- master_player_ev_data %>%
      filter(name == player_name) %>%
      select(gameNumber, GF, GA) %>%
      mutate(GA2 = GA * -1) %>%
      select(-c(GA)) %>%
      rename(GA = GA2)
    
    table_goals_2 <- melt(table_goals, id.var="gameNumber") %>%
      rename(GameNumber = gameNumber, GoalType = variable, Goals = value)
    
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



# -------------------------------------------------------------------------



#IGNORE ALL OF THIS
#Just individual plots for each component of full player card
#Game-by-game TOI
player_toi <- function(player_name) {
  
  y_axis <- master_player_toi_data %>%
    filter(name == player_name)
  
  table_toi <- master_player_toi_data %>%
    filter(name == player_name) %>%
    select(gameNumber, ESTOI, PPTOI, SHTOI)
  
  table_toi2 <- melt(table_toi, id.var="gameNumber") %>%
    rename(GameNumber = gameNumber, GameSituation = variable, TOI = value)
  
  levels(table_toi2$GameSituation)
  table_toi2$GameSituation <- factor(table_toi2$GameSituation, levels = rev(levels(table_toi2$GameSituation)))
  
  toi_visual <- ggplot(table_toi2[order(table_toi2$GameSituation),], 
                       aes(x = GameNumber, y = TOI, fill = GameSituation)) +
    geom_bar(stat = "identity") +
    labs(title= player_name,
         subtitle="TOI by Game Situation") +
    theme_few() +
    theme(plot.title = element_text(face = "bold")) +
    scale_fill_manual(values=c("#FF7F7F", "#FFD17F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 25), expand = c(0, 0))
  
  if(max(y_axis$TOI) > 25){
    toi_visual <- toi_visual + scale_y_continuous(limits=c(0, max(y_axis$TOI) + 1),
                                                  expand = c(0, 0))
  }
  
  return(toi_visual)
  
}


# -------------------------------------------------------------------------

#Game-by-game Points
player_points <- function(player_name) {
  
  y_axis <- master_player_points_data %>%
    filter(name == player_name)
  
  table_points <- master_player_points_data %>%
    filter(name == player_name) %>%
    select(gameNumber, G, A1, A2)
  
  table_points_2 <- melt(table_points, id.var="gameNumber") %>%
    rename(GameNumber = gameNumber, PointType = variable, Points = value)
  
  levels(table_points_2$PointType)
  table_points_2$PointType <- factor(table_points_2$PointType, levels = rev(levels(table_points_2$PointType)))
  
  points_visual <- ggplot(table_points_2[order(table_points_2$PointType),], 
                          aes(x = GameNumber, y = Points, fill = PointType)) +
    geom_bar(stat = "identity") +
    labs(subtitle="Point Production") +
    theme_few() +
    scale_fill_manual(values=c("#CCE4CC", "#B2B2FF", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0))
  
  
  if(max(y_axis$TP) > 3){
    points_visual <- points_visual + scale_y_continuous(limits=c(0, max(y_axis$TP)),
                                                        expand = c(0, 0))
  }
  
  return(points_visual)
  
}

# -------------------------------------------------------------------------

#Game-by-game Corsi
player_corsi <- function(player_name) {
  
  y_axis <- master_player_corsi_data %>%
    filter(name == player_name) %>%
    select(gameNumber, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2)
  
  table_corsi <- master_player_corsi_data %>%
    filter(name == player_name) %>%
    select(gameNumber, CF, CA) %>%
    mutate(CA2 = CA * -1) %>%
    select(-c(CA)) %>%
    rename(CA = CA2)
  
  table_corsi_2 <- melt(table_corsi, id.var="gameNumber") %>%
    rename(GameNumber = gameNumber, Type = variable, Shots = value)
  
  levels(table_corsi_2$Type)
  table_corsi_2$Type <- factor(table_corsi_2$Type, levels = rev(levels(table_corsi_2$Type)))
  
  corsi_visual <- ggplot(table_corsi_2[order(table_corsi_2$Type),], 
                         aes(x = GameNumber, y = Shots, fill = Type)) +
    geom_bar(stat = "identity") +
    labs(subtitle="Corsi") +
    theme_few() +
    #red, black
    scale_fill_manual(values=c("#FF7F7E", "#7F7F7F", "#7F7FFF")) +
    scale_x_continuous(limits = c(0,53), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(-20, 20), expand = c(0, 0)) 
  
  
  if(max(y_axis$CF) > 20){
    corsi_visual <- corsi_visual + scale_y_continuous(limits=c(max(y_axis$CF) * -1, max(y_axis$CF)),
                                                      expand = c(0, 0))
  }
  
  if(min(y_axis$CA) < -20){
    corsi_visual <- corsi_visual + scale_y_continuous(limits=c(min(y_axis$CA), min(y_axis$CA) * -1),
                                                      expand = c(0, 0))
  }
  
  return(corsi_visual)
  
}
