############## Data Gathering
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(fuzzyjoin)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

year_vec = 2009:2016

pbp_list = list()
load(".//834 data//PbP_08_09.Rda")
pbp_list[[1]] = pbp
load(".//834 data//PbP_09_10.Rda")
pbp_list[[2]] = pbp
load(".//834 data//PbP_10_11.Rda")
pbp_list[[3]] = pbp
load(".//834 data//PbP_11_12.Rda")
pbp_list[[4]] = pbp
load(".//834 data//PbP_12_13.Rda")
pbp_list[[5]] = pbp
load(".//834 data//PbP_13_14.Rda")
pbp_list[[6]] = pbp
load(".//834 data//PbP_14_15.Rda")
pbp_list[[7]] = pbp
load(".//834 data//PbP_15_16.Rda")
pbp_list[[8]] = pbp

Box_Data_All <- read.csv(".//jgrosz99-nba-player-data-1978-2016//data//nba_season_data.csv")
All_Star_All <- read.csv(".//gmoney-nba-all-stars-2000-2016//data//nba_all_star_games.csv")
Kaggle_Box_All <- read.csv(".//kaggle//Seasons_Stats.csv")

ranges = data.frame(min = c(2,2,2,5,5,5,12,12,12), score = c(5,10,100,5,10,100,5,10,100))

for(x in 1:9)
{
  MIN_LIMIT = ranges$min[x]
  SCORE_LIMIT = ranges$score[x]
  SECS_FILTER = 0
  
  Box_Data_List <- list()
  
  for(index in 1:8)
  {
    YEAR = year_vec[index]
    pbp = pbp_list[[index]]
    
    Box_Data <- Box_Data_All %>% filter(year == YEAR)
    All_Star <- (All_Star_All %>% filter(year == YEAR))$player # list of all stars
    Kaggle_Box <- Kaggle_Box_All %>% filter(Year == YEAR)
    
    #Matching weird names
    Matching <- data.frame(player_id = pbp$PLAYER1_ID, player_name = pbp$PLAYER1_NAME)
    Matching <- rbind(data.frame(player_id = pbp$PLAYER2_ID, player_name = pbp$PLAYER2_NAME), Matching)
    Matching <- rbind(data.frame(player_id = pbp$PLAYER3_ID, player_name = pbp$PLAYER3_NAME), Matching)
    Matching <- unique(Matching)
    Matching <- Matching %>% filter(player_name != "")
    Matching$player_name <- as.character(Matching$player_name)
    
    Box_Data$player <- as.character(Box_Data$player)
    Box_Data <- Box_Data[Box_Data$player != "0",]
    Box_Data$player_id <- NULL
    
    Box_Data <- stringdist_left_join(Matching, Box_Data, 
                                     by = c(player_name = "player"),
                                     ignore_case = TRUE, 
                                     method = "osa", 
                                     max_dist = 2, 
                                     distance_col = "dist") %>%
      group_by(player_name) %>%
      top_n(1, -dist)
    
    #add on additional things from Kaggle
    
    Ktemp <- unique(data.frame(player_name_kaggle = Kaggle_Box$Player,
                               ft_pc = Kaggle_Box$FT.))
    Ktemp$player_name_kaggle <- as.character(Ktemp$player_name_kaggle)
    Ktemp <- unique(Ktemp)
    Ktemp <- Ktemp %>% group_by(player_name_kaggle) %>% summarize(ft_pc = mean(ft_pc))
    Box_Data <- stringdist_right_join(Ktemp,
                                      Box_Data, 
                                      by = c(player_name_kaggle = "player_name"),
                                      ignore_case = TRUE, 
                                      method = "osa", 
                                      max_dist = 2, 
                                      distance_col = "dist") %>% group_by(player_name) %>% top_n(1, -dist)
    
    #get list of all_stars
    
    Box_Data$all_star <- FALSE
    Box_Data$all_star[Box_Data$player_name %in% All_Star] <- TRUE
    
    #get all close foul events
    pbp$MINS <- as.data.frame(do.call(rbind, strsplit(as.character(pbp$PCTIMESTRING), ":")))[1]$V1
    pbp$SECS <- as.data.frame(do.call(rbind, strsplit(as.character(pbp$PCTIMESTRING), ":")))[2]$V2
    pbp$MINS <- pbp$MINS %>% as.character() %>% as.numeric()
    pbp$SECS <- pbp$SECS %>% as.character() %>% as.numeric()
    pbp$curr_time = pbp$MINS * 60 + pbp$SECS
    
    df <- data.frame(SCOREMARGIN = pbp$SCOREMARGIN,
                              MINS = pbp$MINS,
                              PERIOD = pbp$PERIOD,
                              GAME_ID = pbp$GAME_ID,
                              SECS = pbp$SECS,
                              DRAWN_FOUL_PLAYER_ID = pbp$DRAWN_FOUL_PLAYER_ID,
                              FOULED_BY_PLAYER_ID = pbp$FOULED_BY_PLAYER_ID,
                              BLOCK_PLAYER_ID = pbp$BLOCK_PLAYER_ID,
                              REBOUND_PLAYER_ID = pbp$REBOUND_PLAYER_ID,
                              STEAL_PLAYER_ID = pbp$STEAL_PLAYER_ID,
                              FOUL_TYPE = pbp$FOUL_TYPE,
                              FOUL_COUNT = pbp$FOUL_COUNT)
    
    ## keep only L2M of within last 2 mins
    
    df <- df[(df$MINS < MIN_LIMIT) & (df$PERIOD == 4),]
    
    # then, filter on score margin
    
    df$KEEP <- FALSE
    df$SCOREMARGIN <-df$SCOREMARGIN %>% as.character()
    df$SCOREMARGIN[df$SCOREMARGIN == "TIE"] = "0"
    df$SCOREMARGIN <-df$SCOREMARGIN %>% as.numeric()
    
    ## check score margin
    
    Box_Data$generic_secs <- 0
    
    for(i in 1:length(unique(df$GAME_ID))){
      game = unique(df$GAME_ID)[i]
      if(sum(df$SCOREMARGIN[df$GAME_ID == game] %>% abs() < SCORE_LIMIT, na.rm = T) > 0)
      {
        df$KEEP[df$GAME_ID == game] <- TRUE
        pbp_interval = (pbp$GAME_ID == game) & (pbp$MINS < MIN_LIMIT) & (pbp$PERIOD == 4)
        prior_time <- MIN_LIMIT*60
        for(j in 1:sum(pbp_interval))
        {
          if(!(pbp$SUB_ENTERED_PLAYER_ID[pbp_interval][j] %>% is.na()))
          {
            Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_1[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_1[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_2[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_2[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_3[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_3[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_4[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_4[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_5[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_5[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_1[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_1[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_2[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_2[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_3[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_3[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_4[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_4[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_5[pbp_interval][j]] = 
              Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_5[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            
            prior_time <- pbp$curr_time[pbp_interval][j]
          }
          
        }
        
        Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_1[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_1[pbp_interval][j]] + prior_time
        Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_2[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_2[pbp_interval][j]] + prior_time
        Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_3[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_3[pbp_interval][j]] + prior_time
        Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_4[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_4[pbp_interval][j]] + prior_time
        Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_5[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_5[pbp_interval][j]] + prior_time
        Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_1[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_1[pbp_interval][j]] + prior_time
        Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_2[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_2[pbp_interval][j]] + prior_time
        Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_3[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_3[pbp_interval][j]] + prior_time
        Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_4[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_4[pbp_interval][j]] + prior_time
        Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_5[pbp_interval][j]] = 
          Box_Data$generic_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_5[pbp_interval][j]] + prior_time
        
      }
      else
      {
        #do not keep
        df$KEEP[df$GAME_ID == game] <- FALSE
      }
    }
    
    
    df$SCOREMARGIN <- NULL
    df <- df %>% filter(KEEP == TRUE)
    df$KEEP <- NULL
    
    ##Merge everything
    
    Box_Data <- df %>% count(DRAWN_FOUL_PLAYER_ID) %>% rename(c("player_id" = "DRAWN_FOUL_PLAYER_ID", "fouls_drawn" = "n")) %>% full_join(Box_Data, by = "player_id")
    Box_Data <- df %>% count(FOULED_BY_PLAYER_ID) %>% rename(c("player_id" = "FOULED_BY_PLAYER_ID", "fouls_committed" = "n")) %>% full_join(Box_Data, by = "player_id")
    Box_Data <- df %>% count(REBOUND_PLAYER_ID) %>% rename(c("player_id" = "REBOUND_PLAYER_ID", "reb_per_interval" = "n")) %>% full_join(Box_Data, by = "player_id")
    Box_Data <- df %>% count(STEAL_PLAYER_ID) %>% rename(c("player_id" = "STEAL_PLAYER_ID", "stl_per_interval" = "n")) %>% full_join(Box_Data, by = "player_id")
    Box_Data <- df %>% count(BLOCK_PLAYER_ID) %>% rename(c("player_id" = "BLOCK_PLAYER_ID", "blk_per_interval" = "n")) %>% full_join(Box_Data, by = "player_id")
    
    Box_Data_List[[index]] <- Box_Data
    
    print(YEAR)
  }
  
  Box_Data <- rbindlist(Box_Data_List)
  Box_Data$bbref_pos <- round(Box_Data$bbref_pos %>% as.character() %>% as.numeric())
  
  Box_Data$reb_per_interval <- Box_Data$reb_per_interval/Box_Data$generic_secs * 60
  Box_Data$stl_per_interval <- Box_Data$stl_per_interval/Box_Data$generic_secs * 60
  Box_Data$blk_per_interval <- Box_Data$blk_per_interval/Box_Data$generic_secs * 60
  Box_Data$fouls_committed <- Box_Data$fouls_committed/Box_Data$generic_secs * 60
  Box_Data$fouls_drawn <- Box_Data$fouls_drawn/Box_Data$generic_secs * 60
  
  lm0 <- lm(fouls_drawn ~ all_star + as.factor(year) + fouls_committed +
              mp * tm_usg + ts + ws + ovorp + o_bpm + reb_per_interval + stl_per_interval + blk_per_interval + per +
              ft_pc + yrs_experience + as.factor(bbref_pos),
        data = Box_Data %>% filter(mp > 400) %>% filter(generic_secs > SECS_FILTER))
  
  print("Min Limit, Score Limit, Coefficient, Significance: ")
  print(MIN_LIMIT)
  print(SCORE_LIMIT)
  lm0$coefficients[2] %>% print()
  summary(lm0)$coefficients[2,4] %>% print()
}