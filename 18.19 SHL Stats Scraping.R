#This is for all of the stats scraping from shl.se
#Easily the part of this that needs the most refactoring
#Was too scared to do a for loop that took into account multiple variables
#So instead I manually went through each for loop 14 times (one for each team)

#Player Points
league_player_points_scrape <- function(date, teamID) {
  #create the url for the 'Points' stats report on shl.se
  url <- read_html(paste('https://www.shl.se/statistik/spelare/point?season=from', date,
                         'to', date + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
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
    mutate(date) %>%
    
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
    mutate(name_date = paste(name, date, sep = "")) %>%
    
    #join in friendly team name
    left_join(shlseNameDictionary, by = "Lag") %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(shlName, date, sep = "")) %>%
    
    #join in schedule data
    #have to update this each time too, so not great
    left_join(vax1819_schedule, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date.y, homeTeam, awayTeam, combinedTeams, Venue, date2, team))
  
  return(table)
}

player_points_data <- c()
for (date in as.list(vax1819_schedule$date2[1:52]))  {
  
  temp <- league_player_points_scrape(date, teamIDLookup_1819$teamID[14])
  
  #add the new temp table from the next date to the master data frame
  player_points_data <- rbind(player_points_data, temp)
  
  print(date)
  rm(date, temp)
}

#assign the master data frame to its team
#again, manually update this 14 times
vax_player_points_data <- player_points_data

#create a master data frame for the whole league, all 14 teams bound together
#will use this when it's time to make the visual
master_player_points_data <- rbind(
  bif_player_points_data, dif_player_points_data, fhc_player_points_data,
  fbk_player_points_data, hv_player_points_data, lhc_player_points_data,
  lhf_player_points_data, mif_player_points_data, mik_player_points_data,
  ohk_player_points_data, rbk_player_points_data, ske_player_points_data,
  tik_player_points_data,vax_player_points_data)


# -------------------------------------------------------------------------

#Player TOI
league_player_toi_scrape <- function(date, teamID) {
  #url for the TOI report on shl.se
  url <- read_html(paste('https://www.shl.se/statistik/spelare/timeonice?season=from', date,
                         'to', date + 1, '&gameType=regular&count=-1&date%5Bfrom%5D=2018-09-15&date%5Bto%5D=2018-09-16&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date, sep = "")) %>%
    
    #join in friendly team name
    left_join(shlseNameDictionary, by = "Lag") %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(shlName, date, sep = "")) %>%
    
    #join in schedule data
    left_join(vax1819_schedule, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date.y, homeTeam, awayTeam, combinedTeams, Venue, date2, team)) %>%
    
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

player_toi_data <- c()
for (date in as.list(vax1819_schedule$date2[1:52]))  {
  
  temp <- league_player_toi_scrape(date, teamIDLookup_1819$teamID[14])
  
  player_toi_data <- rbind(player_toi_data, temp)
  
  print(date)
  rm(date, temp)
  
}

vax_player_toi_data <- player_toi_data

master_player_toi_data <- rbind(
  bif_player_toi_data, dif_player_toi_data, fhc_player_toi_data,
  fbk_player_toi_data, hv_player_toi_data, lhc_player_toi_data,
  lhf_player_toi_data, mif_player_toi_data, mik_player_toi_data,
  ohk_player_toi_data, rbk_player_toi_data, ske_player_toi_data,
  tik_player_toi_data,vax_player_toi_data)


# -------------------------------------------------------------------------

#Team Corsi
league_team_corsi_scrape <- function(date, teamID) {
  #create the URL for the team corsi stats table on shl.se
  #need this to calculate a player's corsi rel
  url <- read_html(paste('https://www.shl.se/statistik/lag/extendedmetrics?season=from', date,
                         'to', date + 1, '&gameType=regular&team=', teamID, '&vsTeam=All&outcome=all&location=all',
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
    mutate(date) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove space in "Lag" column
    mutate(Lag2 = gsub("\\s+", "", Lag)) %>%
    
    #join in friendly team name
    left_join(shlseNameDictionary2, by = "Lag2") %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(shlName, date, sep = "")) %>%
    
    #join in schedule data
    left_join(vax1819_schedule, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date.y, homeTeam, awayTeam, combinedTeams, Venue, date2, team))
  
  return(table)
}

team_corsi_data <- c()
for (date in as.list(vax1819_schedule$date2[1:52]))  {
  
  temp <- league_team_corsi_scrape(date, teamIDLookup_1819$teamID[14])
  
  team_corsi_data <- rbind(team_corsi_data, temp)
  
  print(date)
  rm(date, temp)
}

vax_team_corsi_data <- team_corsi_data

master_team_corsi_data <- rbind(
  bif_team_corsi_data, dif_team_corsi_data, fhc_team_corsi_data,
  fbk_team_corsi_data, hv_team_corsi_data, lhc_team_corsi_data,
  lhf_team_corsi_data, mif_team_corsi_data, mik_team_corsi_data,
  ohk_team_corsi_data, rbk_team_corsi_data, ske_team_corsi_data,
  tik_team_corsi_data,vax_team_corsi_data)


#Player Corsi
league_player_corsi_scrape <- function(date, teamID) {
  #create the URL for the corsi report on shl.se
  url <- read_html(paste('https://www.shl.se/statistik/spelare/extendedmetrics?season=from', date,
                         'to', date + 1, '&gameType=regular&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
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
    mutate(date) %>%
    
    #make columns numeric
    mutate_at(c('GP', 'CF', 'CA', 'FF', 'FA'), as.numeric) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date, sep = "")) %>%
    
    #join in friendly team name
    left_join(shlseNameDictionary, by = "Lag") %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(shlName, date, sep = "")) %>%
    
    #join in schedule data
    left_join(vax1819_schedule, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date.y, homeTeam, awayTeam, combinedTeams, Venue, date2, team,
              `CF%`, `CCF%`, `FF%`, `CFF%`, `OIG%`, `OISVS%`, PDO, `TOI/GP`)) %>%
    
    #join in team corsi data
    #need this to get relative corsi later
    left_join(master_team_corsi_data, by = "team_date") %>%
  
    #remove duplicated columns from join
    select(-c(Rank.y, Lag.y, GP.y, `CF%`, `CCF%`, `FF%`, `CFF%`,
              `OIG%`, `OISVS%`, PDO, date.x.y, Lag2, shlName.y,
              gameNumber.y)) %>%
    
    #rename columns
    rename(Lag = Lag.x, CF = CF.x, CA = CA.x, FF = FF.x, FA = FA.x,
           date2 = date.x.x, tmCF = CF.y, tmCA = CA.y, tmFF = FF.y,
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


player_corsi_data <- c()
for (date in as.list(vax1819_schedule$date2[1:52]))  {
  
  temp <- league_player_corsi_scrape(date, teamIDLookup_1819$teamID[14])
  
  player_corsi_data <- rbind(player_corsi_data, temp)
  
  print(date)
  rm(date, temp)
}

vax_player_corsi_data <- player_corsi_data

master_player_corsi_data <- rbind(
  bif_player_corsi_data, dif_player_corsi_data, fhc_player_corsi_data,
  fbk_player_corsi_data, hv_player_corsi_data, lhc_player_corsi_data,
  lhf_player_corsi_data, mif_player_corsi_data, mik_player_corsi_data,
  ohk_player_corsi_data, rbk_player_corsi_data, ske_player_corsi_data,
  tik_player_corsi_data,vax_player_corsi_data)

master_player_corsi_data <- master_player_corsi_data %>%
  rename(gameNumber = gameNumber.x)


# -------------------------------------------------------------------------

#Player Goals
league_player_ev_scrape <- function(date, teamID) {
  #create the URL for the goals tabls on shl.se stats page
  url <- read_html(paste('https://www.shl.se/statistik/spelare/equalstrength?season=from', date,
                         'to', date + 1, '&gameType=regular&count=-1&date%5Bfrom%5D=2018-09-15&date%5Bto%5D=2018-09-16&position=All&status=All&team=', teamID, '&vsTeam=All&outcome=all&location=all',
                         sep=''))
  
  table <- url %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame(fill = TRUE) %>%
    
    #add game date
    mutate(date) %>%
    
    #remove the last word from the Spelare string
    mutate(placeholder_name = gsub("\\s+\\S+$", "", Spelare)) %>%
    
    #remove the last letter from the placeholder_name string
    mutate(name = gsub('.{1}$', '', placeholder_name)) %>%
    
    #concatenate name and date for unique identifier
    mutate(name_date = paste(name, date, sep = "")) %>%
    
    #join in friendly team name
    left_join(shlseNameDictionary, by = "Lag") %>%
    
    #concatenate team and date for unique identifier
    mutate(team_date = paste(shlName, date, sep = "")) %>%
    
    #join in schedule data
    left_join(vax1819_schedule, by = "team_date") %>%
    
    #remove unneeded columns by schedule data
    select(-c(date.y, homeTeam, awayTeam, combinedTeams, Venue, date2, team)) %>%
    
    #rename columns
    rename(GF = X., GA = X..1, PlusMinus = X...) %>%
    
    mutate_at(c('GF', 'GA','PlusMinus'), as.numeric)
  
  return(table)
}

player_ev_data <- c()
for (date in as.list(vax1819_schedule$date2[1:52]))  {
  
  temp <- league_player_ev_scrape(date, teamIDLookup_1819$teamID[14])
  
  player_ev_data <- rbind(player_ev_data, temp)
  
  print(date)
  rm(date, temp)
  
}

vax_player_ev_data <- player_ev_data

master_player_ev_data <- rbind(
  bif_player_ev_data, dif_player_ev_data, fhc_player_ev_data,
  fbk_player_ev_data, hv_player_ev_data, lhc_player_ev_data,
  lhf_player_ev_data, mif_player_ev_data, mik_player_ev_data,
  ohk_player_ev_data, rbk_player_ev_data, ske_player_ev_data,
  tik_player_ev_data,vax_player_ev_data)


# -------------------------------------------------------------------------




