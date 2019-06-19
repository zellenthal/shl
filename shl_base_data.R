
#vector for team IDs from SHL.se
teamIDs <- c("f7c8-f7c8isEb3", "1ab8-1ab8bfj7N", "2459-2459QTs1", "087a-087aTQv9u",
             "752c-752c12zB7Z", "3db0-3db09jXTE", "f51e-f51eM6sbL", "9541-95418PpkP",
             "41c4-41c4BiYZU", "1a71-1a71gTHKh", "8e6f-8e6fUXJvi", "110b-110bJcIAI",
             "dcba-dcbaXSrRU", "259b-259bYGVIp", "82eb-82ebmgaJ8", "ee93-ee93uy4oW",
             "50e6-50e6DYeWM", "936a-936aAY8bT", "31d1-31d1NbSlR", "fe02-fe02mf1FN")

#vector for SHL.se team names
shlTeamNames <- c("AIK", "Brynäs", "Djurgården", "Frölunda", "Färjestad", "HV71", "Karlskrona",
                  "Leksand", "Linköping", "Luleå", "Malmö", "MODO", "Mora", "Oskarshamn", "Örebro",
                  "Rögle", "Skellefteå", "Södertälje", "Timrå", "Växjö")

#vector for SweHockey team names
sweHockeyTeams <- c("AIK", "Brynäs IF", "Djurgårdens IF", "Frölunda HC", "Färjestad BK",
                    "HV 71", "Karlskrona HK", "Leksands IF", "Linköping HC", "Luleå HF",
                    "IF Malmö Redhawks", "MODO Hockey", "Mora IK", "IK Oskarshamn",
                    "Örebro HK", "Rögle BK", "Skellefteå AIK", "Södertälje SK", "Timrå IK", 
                    "Växjö Lakers HC")

#lookup table for SHL.se team names and team IDs
teamIDLookup <- tibble(shlName = shlTeamNames, teamID = teamIDs)

#lookup table for SHL team names and SweHockey team names
teamNameDictionary <- tibble(shlName = shlTeamNames, sweName = sweHockeyTeams)

#vector of all dates in the 2018/2019 season
dates1819 <- seq(as_date("2018-09-15"), as_date("2019-03-14"), by="days") %>%
  tibble()

#read in the 18/19 SHL schedule
shl1819_schedule <- read_csv("schedule1819.csv")


gameSummaryTest <- read_html("https://www.shl.se/statistik/spelare?season=from2018-09-15to2018-09-16&gameType=regular&position=All&status=All&team=2459-2459QTs1f&vsTeam=All&outcome=all&location=all")

gameSummaryTest %>%
  html_nodes("rmss_t-stat-table__header-item-span") %>%
  html_text()

gameSummaryTest %>%
  html_nodes("a.rmss_t-stat-table__header-item-link") %>%
  html_text()


tbls <- html_nodes(gameSummaryTest, "table")

tbls_ls <- gameSummaryTest %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)