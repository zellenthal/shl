box_score_data_all <- read.csv("box_score_data_all.csv", stringsAsFactors = FALSE)
box_score_data_all_filtered  <- box_score_data_all %>%
  dplyr::filter(season == '18/19',
                gf_team_game_number < 26)


player_toi_data_master <- read.csv("player_toi_data_master.csv", stringsAsFactors = FALSE)
player_toi_data_master_filtered <- player_toi_data_master %>%
  dplyr::filter(season == '18/19',
                game_number < 26)

player_corsi_data_master <- read.csv("player_corsi_data_master.csv", stringsAsFactors = FALSE)
player_corsi_data_master_filtered <- player_corsi_data_master %>%
  dplyr::filter(season == '18/19',
                game_number.x < 26)

player_shots_data_master <- read.csv("player_shots_data_master.csv", stringsAsFactors = FALSE)
player_shots_data_master_filtered <- player_shots_data_master %>%
  dplyr::filter(season == '18/19',
                game_number < 26)

team_corsi_data_master <- read.csv("team_corsi_data_master.csv", stringsAsFactors = FALSE)
team_corsi_data_master_filtered <- team_corsi_data_master %>%
  dplyr::filter(season == '18/19',
                game_number < 26)


get_5v5_data_kc <- function(player) {
  
  GP <- player_toi_data_master_filtered %>%
    dplyr::filter(player_team == player) %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  
  fenwick_data <- player_corsi_data_master_filtered %>%
    dplyr::filter(player_team == player) %>%
    dplyr::filter(season == '18/19') %>%
    dplyr::select(FF, FA, CF, CA, CF_off, CA_off)
  #dplyr::select(FF, FA, CF, CA)
  
  CF <- sum(fenwick_data$CF)
  CA <- sum(fenwick_data$CA)
  
  `CF%` <- (round(CF / (CF + CA),3)) * 100
  #`CF%` <- (signif(CF / (CF + CA),3)) * 100
  
  CF_off <- sum(fenwick_data$CF_off)
  CA_off <- sum(fenwick_data$CA_off)
  `CF%_off` <- (round(CF_off / (CF_off + CA_off),3)) * 100
  
  `CF% Rel` <- round(`CF%` - `CF%_off`, 1)
  
  FF <- sum(fenwick_data$FF)
  FA <- sum(fenwick_data$FA)
  
  G <- box_score_data_all_filtered %>%
    dplyr::filter(goal_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all_filtered %>%
    dplyr::filter(primary_assist_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all_filtered %>%
    dplyr::filter(secondary_assist_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  GF1 <- box_score_data_all_filtered %>%
    dplyr::filter(GF1_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all_filtered %>%
    dplyr::filter(GF2_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all_filtered %>%
    dplyr::filter(GF3_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all_filtered %>%
    dplyr::filter(GF4_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all_filtered %>%
    dplyr::filter(GF5_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all_filtered %>%
    dplyr::filter(GF6_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all_filtered %>%
    dplyr::filter(GA1_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all_filtered %>%
    dplyr::filter(GA2_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all_filtered %>%
    dplyr::filter(GA3_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all_filtered %>%
    dplyr::filter(GA4_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all_filtered %>%
    dplyr::filter(GA5_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all_filtered %>%
    dplyr::filter(GA6_player_team == player) %>%
    dplyr::filter(game_situation == '5v5') %>%
    dplyr::filter(season == '18/19') %>%
    nrow()
  
  GA = GA1 + GA2 + GA3 + GA4 + GA5 + GA6
  
  `GF%` <- round((GF / (GF + GA)) * 100, 1)
  
  IPP <- round((P/GF) * 100, 1)
  
  FenSh <- round((GF/FF) * 100, 1)
  FenSv <- 100 - round((GA/FA) * 100, 1)
  
  # #New 5v5 TOI Estimate
  # TOI_subset <- player_toi_data_2021 %>%
  #   filter(player_team == player) 
  # 
  # TOI <- round(sum(TOI_subset$ESTOI), 1)
  # 
  # TOI_avg <- TOI/GP
  # 
  # if (TOI_avg >= 15) {
  #   toi_adj <- 0.933
  # }
  # 
  # if (TOI_avg >= 10 & TOI_avg < 15) {
  #   toi_adj <- 0.96
  # }
  # 
  # TOI <- round(TOI * toi_adj, 1)
  
  data <- tibble(player = player, situation = 'EV', manpower = '5v5', GP = GP, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1,
                 GF = GF, GA = GA, `GF%` = `GF%`, IPP = IPP, CF = CF, CA = CA, `CF%` = `CF%`,`CF% Rel` = `CF% Rel`,  FenSh = FenSh, FenSv = FenSv)
  
  # data <- merge(data, player_toi_data_master_filtered[,c("player_team", "shlse_team_name", "Pos")],
  #               by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  # 
  # #data <- left_join(data, player_toi_data_2021, by = c("player" = "player_team")) %>% unique()
  # 
  # data <- data %>%
  #   dplyr::rename(Team = shlse_team_name,
  #                 Player = player,
  #                 Situation = situation,
  #                 Manpower = manpower) %>%
  #   
  #   mutate(Season = '18/19')
  # 
  # data$Player <- str_split(data$Player, "[$]")[[1]][1]
  
  #NHL Rights
  # data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
  #   dplyr::rename(`NHL Rights` = abbrev)
  # 
  # bio <- shl_playerdb_v2 %>%
  #   dplyr::filter(swehockey == data$Player,
  #                 season == '2020-2021')
  # 
  # Age <- bio$age2[1]
  # Nationality <- bio$nationality[1]
  # 
  # data <- data %>% 
  #   add_column(Age = Age, Nationality = Nationality)
  
  # data <- data %>%
  #   .[,c("Player", "Season", "Team", "Pos", "Situation", "Manpower", "GP", "G", "A", "P", "P1", "A1", "A2", "GF", "GA", "GF%",
  #        "IPP", "CF", "CA", "CF%", "CF% Rel", "FenSh", "FenSv", "NHL Rights")] %>% dplyr::distinct()
  
  return(data)
  #return(`CF% Rel`)
  
}

get_5v5_data_kc('Kodie Curran$Rögle BK')


get_ev_data_kc <- function(player) {
  
  GP <- player_toi_data_master_filtered %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_master_filtered %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$ESTOI), 1)
  
  G <- box_score_data_all_filtered %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all_filtered %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all_filtered %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2) 
  
  SOG <- player_shots_data_master_filtered %>%
    dplyr::filter(player_team == player, season == '18/19')
  
  SOG <- sum(SOG$SOG_EV)
  
  SOG60 <- round(SOG/TOI * 60, 1)
  
  `Sh%` <- (round(G/SOG, 3) * 100)
  
  GF1 <- box_score_data_all_filtered %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all_filtered %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all_filtered %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all_filtered %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all_filtered %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all_filtered %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all_filtered %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all_filtered %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all_filtered %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all_filtered %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all_filtered %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'EV') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all_filtered %>%
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
  
  data <- tibble(player = player, situation = 'EV', manpower = 'All EV', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, SOG = SOG, SOG60 = SOG60, `Sh%` = `Sh%`, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  # data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
  #               by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  # 
  # data <- data %>%
  #   dplyr::rename(Team = shlse_team_name,
  #                 Player = player,
  #                 Situation = situation,
  #                 Manpower = manpower) %>%
  #   
  #   mutate(Season = '18/19')
  # 
  # data$Player <- str_split(data$Player, "[$]")[[1]][1]
  # 
  # #NHL Rights
  # data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
  #   dplyr::rename(`NHL Rights` = abbrev)
  # 
  # bio <- shl_playerdb_v2 %>%
  #   dplyr::filter(swehockey == data$Player,
  #                 season == '2020-2021')
  # 
  # Age <- bio$age2[1]
  # Nationality <- bio$nationality[1]
  # 
  # data <- data %>% 
  #   add_column(Age = Age, Nationality = Nationality)
  # 
  # data <- data %>%
  #   .[,c("Player", "Season", "Age", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "SOG", "SOG60", "Sh%", "GF", "GA", "GF%",
  #        "GF60", "GA60", "IPP", "NHL Rights", "Nationality")] %>% dplyr::distinct()
  
  return(data)
  #return(SOG)
}

get_ev_data_kc('Kodie Curran$Rögle BK')


get_pp_data_kc <- function(player) {
  
  GP <- player_toi_data_master_filtered %>%
    filter(player_team == player) %>%
    nrow()
  
  TOI_subset <- player_toi_data_master_filtered %>%
    filter(player_team == player) 
  
  TOI <- round(sum(TOI_subset$PPTOI), 1)
  
  G <- box_score_data_all_filtered %>%
    filter(goal_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A1 <- box_score_data_all_filtered %>%
    filter(primary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A2 <- box_score_data_all_filtered %>%
    filter(secondary_assist_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  
  A <- A1 + A2
  
  P1 <- G + A1
  
  P <- G + A
  
  P60 <- round(P/TOI * 60, 2)
  
  P160 <- round(P1/TOI * 60, 2)
  
  SOG <- player_shots_data_master_filtered %>%
    dplyr::filter(player_team == player, season == '18/19')
  
  SOG <- sum(SOG$SOG_PP)
  
  SOG60 <- round(SOG/TOI * 60, 1)
  
  `Sh%` <- (round(G/SOG, 3) * 100)
  
  GF1 <- box_score_data_all_filtered %>%
    filter(GF1_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF2 <- box_score_data_all_filtered %>%
    filter(GF2_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF3 <- box_score_data_all_filtered %>%
    filter(GF3_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF4 <- box_score_data_all_filtered %>%
    filter(GF4_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF5 <- box_score_data_all_filtered %>%
    filter(GF5_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  GF6 <- box_score_data_all_filtered %>%
    filter(GF6_player_team == player) %>%
    filter(game_situation_general == 'PP') %>%
    filter(season == '18/19') %>%
    nrow()
  
  GF = GF1 + GF2 + GF3 + GF4 + GF5 + GF6
  
  GA1 <- box_score_data_all_filtered %>%
    filter(GA1_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA2 <- box_score_data_all_filtered %>%
    filter(GA2_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA3 <- box_score_data_all_filtered %>%
    filter(GA3_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA4 <- box_score_data_all_filtered %>%
    filter(GA4_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA5 <- box_score_data_all_filtered %>%
    filter(GA5_player_team == player) %>%
    filter(game_situation_general == 'SH') %>%
    filter(season == '18/19') %>%
    nrow()
  GA6 <- box_score_data_all_filtered %>%
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
  
  data <- tibble(player = player, situation = 'PP', manpower = 'All PP', GP = GP, TOI = TOI, `TOI/GP` = `TOI/GP`, G = G, A = A, P = P, A1 = A1, A2 = A2, P1 = P1, P60 = P60, P160 = P160, SOG = SOG, SOG60 = SOG60, `Sh%` = `Sh%`, GF = GF, GA = GA, `GF%` = `GF%`,
                 GF60 = GF60, GA60 = GA60, IPP = IPP)
  
  # data <- merge(data, player_toi_data_2021[,c("player_team", "shlse_team_name", "Pos")],
  #               by.x = "player", by.y = "player_team") %>% dplyr::distinct()
  # 
  # data <- data %>%
  #   dplyr::rename(Team = shlse_team_name,
  #                 Player = player,
  #                 Situation = situation,
  #                 Manpower = manpower) %>%
  #   
  #   mutate(Season = '18/19')
  # 
  # data$Player <- str_split(data$Player, "[$]")[[1]][1]
  # 
  # #NHL Rights
  # data <- left_join(data, player_lookup_2021_v2, by = c("Player" = "swehockey")) %>%
  #   dplyr::rename(`NHL Rights` = abbrev)
  # 
  # bio <- shl_playerdb_v2 %>%
  #   dplyr::filter(swehockey == data$Player,
  #                 season == '2020-2021')
  # 
  # Age <- bio$age2[1]
  # Nationality <- bio$nationality[1]
  # 
  # data <- data %>% 
  #   add_column(Age = Age, Nationality = Nationality)
  # 
  # data <- data %>%
  #   .[,c("Player", "Season", "Age", "Team", "Pos", "Situation", "Manpower", "TOI", "TOI/GP", "GP", "G", "A", "P", "P1", "A1", "A2", "P60", "P160", "SOG", "SOG60", "Sh%", "GF", "GA", "GF%",
  #        "GF60", "GA60", "IPP", "NHL Rights", "Nationality")] %>% dplyr::distinct()
  
  return(data)
}

get_pp_data_kc('Kodie Curran$Rögle BK')
