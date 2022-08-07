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
  
  #get all foul events.
  
  Fouls <- data.frame(pbp$PLAYER1_ID, pbp$PLAYER1_NAME, pbp$PLAYER2_NAME,  pbp$PLAYER2_TEAM_ID,  
                      pbp$DRAWN_FOUL_PLAYER_ID, pbp$FOULED_BY_PLAYER_ID, pbp$FOUL_TYPE, pbp$FOUL_COUNT)
  
  
  #get all close foul events
  pbp$MINS <- as.data.frame(do.call(rbind, strsplit(as.character(pbp$PCTIMESTRING), ":")))[1]$V1
  pbp$SECS <- as.data.frame(do.call(rbind, strsplit(as.character(pbp$PCTIMESTRING), ":")))[2]$V2
  pbp$MINS <- pbp$MINS %>% as.character() %>% as.numeric()
  pbp$SECS <- pbp$SECS %>% as.character() %>% as.numeric()
  pbp$curr_time = pbp$MINS * 60 + pbp$SECS
  
  ## LOOK AT QUARTERS INSTEAD OF L2M
  ## Last quarter, score margin 10
  ##
  ##
  
  for(qtr in 1:4){
    LQ_Dataset <- data.frame(SCOREMARGIN = pbp$SCOREMARGIN,
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
    
    
    ## keep only LQ of within last quarter
    
    LQ_Dataset <- LQ_Dataset[(LQ_Dataset$PERIOD == qtr),]
    
    # then, filter on score margin
    
    LQ_Dataset$KEEP <- FALSE
    LQ_Dataset$SCOREMARGIN <-LQ_Dataset$SCOREMARGIN %>% as.character()
    LQ_Dataset$SCOREMARGIN[LQ_Dataset$SCOREMARGIN == "TIE"] = "0"
    LQ_Dataset$SCOREMARGIN <-LQ_Dataset$SCOREMARGIN %>% as.numeric()
    
    ## check score margin
    
    Box_Data$lq_secs <- 0
    
    pbp$HOME_PLAYER_ID_1[pbp$HOME_PLAYER_ID_1 %>% is.na()] <- 0
    pbp$HOME_PLAYER_ID_2[pbp$HOME_PLAYER_ID_2 %>% is.na()] <- 0
    pbp$HOME_PLAYER_ID_3[pbp$HOME_PLAYER_ID_3 %>% is.na()] <- 0
    pbp$HOME_PLAYER_ID_4[pbp$HOME_PLAYER_ID_4 %>% is.na()] <- 0
    pbp$HOME_PLAYER_ID_5[pbp$HOME_PLAYER_ID_5 %>% is.na()] <- 0
    pbp$AWAY_PLAYER_ID_1[pbp$AWAY_PLAYER_ID_1 %>% is.na()] <- 0
    pbp$AWAY_PLAYER_ID_2[pbp$AWAY_PLAYER_ID_2 %>% is.na()] <- 0
    pbp$AWAY_PLAYER_ID_3[pbp$AWAY_PLAYER_ID_3 %>% is.na()] <- 0
    pbp$AWAY_PLAYER_ID_4[pbp$AWAY_PLAYER_ID_4 %>% is.na()] <- 0
    pbp$AWAY_PLAYER_ID_5[pbp$AWAY_PLAYER_ID_5 %>% is.na()] <- 0
    Box_Data$player_id[Box_Data$player_id %>% is.na()] <- 0
    
    for(i in 1:length(unique(LQ_Dataset$GAME_ID))){
      game = unique(LQ_Dataset$GAME_ID)[i]
      if(TRUE) ##no cutoff
      {
        LQ_Dataset$KEEP[LQ_Dataset$GAME_ID == game] <- TRUE
        pbp_interval = (pbp$GAME_ID == game) & (pbp$PERIOD == qtr)
        pbp_interval = pbp_interval[!is.na(pbp_interval)]
        prior_time <- 720
        
        for(j in 1:sum(pbp_interval))
        {
          if(sum(pbp_interval) == 0) {break}
          if(!(pbp$SUB_ENTERED_PLAYER_ID[pbp_interval][j] %>% is.na()))
          {
            
            Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_1[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_1[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_2[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_2[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_3[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_3[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_4[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_4[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_5[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_5[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_1[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_1[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_2[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_2[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_3[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_3[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_4[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_4[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_5[pbp_interval][j]] = 
              Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_5[pbp_interval][j]] + prior_time - pbp$curr_time[pbp_interval][j]
            
            prior_time <- pbp$curr_time[pbp_interval][j]
          }
          
        }
        
        j = sum(pbp_interval)
      
        Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_1[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_1[pbp_interval][j]] + prior_time
        Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_2[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_2[pbp_interval][j]] + prior_time
        Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_3[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_3[pbp_interval][j]] + prior_time
        Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_4[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_4[pbp_interval][j]] + prior_time
        Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_5[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$HOME_PLAYER_ID_5[pbp_interval][j]] + prior_time
        Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_1[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_1[pbp_interval][j]] + prior_time
        Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_2[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_2[pbp_interval][j]] + prior_time
        Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_3[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_3[pbp_interval][j]] + prior_time
        Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_4[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_4[pbp_interval][j]] + prior_time
        Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_5[pbp_interval][j]] = 
          Box_Data$lq_secs[Box_Data$player_id == pbp$AWAY_PLAYER_ID_5[pbp_interval][j]] + prior_time
        
      }
      else
      {
        #do not keep
        LQ_Dataset$KEEP[LQ_Dataset$GAME_ID == game] <- FALSE
      }
    }
    
    
    LQ_Dataset$SCOREMARGIN <- NULL
    LQ_Dataset <- LQ_Dataset %>% filter(KEEP == TRUE)
    LQ_Dataset$KEEP <- NULL
    
    Box_Data <- LQ_Dataset %>% count(DRAWN_FOUL_PLAYER_ID) %>%
      rename(c("player_id" = "DRAWN_FOUL_PLAYER_ID", "fouls_drawn" = "n")) %>% full_join(Box_Data, by = "player_id")
    Box_Data <- LQ_Dataset %>% count(FOULED_BY_PLAYER_ID) %>%
      rename(c("player_id" = "FOULED_BY_PLAYER_ID", "fouls_committed" = "n")) %>% full_join(Box_Data, by = "player_id")
    Box_Data <- LQ_Dataset %>% count(REBOUND_PLAYER_ID) %>%
      rename(c("player_id" = "REBOUND_PLAYER_ID", "reb_qtr" = "n")) %>% full_join(Box_Data, by = "player_id")
    Box_Data <- LQ_Dataset %>% count(STEAL_PLAYER_ID) %>%
      rename(c("player_id" = "STEAL_PLAYER_ID", "stl_qtr" = "n")) %>% full_join(Box_Data, by = "player_id")
    Box_Data <- LQ_Dataset %>% count(BLOCK_PLAYER_ID) %>%
      rename(c("player_id" = "BLOCK_PLAYER_ID", "blk_qtr" = "n")) %>% full_join(Box_Data, by = "player_id")
    
    
    Box_Data$reb_qtr <- Box_Data$reb_qtr/Box_Data$lq_secs * 60
    Box_Data$blk_qtr <- Box_Data$blk_qtr/Box_Data$lq_secs * 60
    Box_Data$stl_qtr <- Box_Data$stl_qtr/Box_Data$lq_secs * 60
    Box_Data$fouls_committed <- Box_Data$fouls_committed/Box_Data$lq_secs * 60
    Box_Data$fouls_drawn <- Box_Data$fouls_drawn/Box_Data$lq_secs * 60
    
    colnames(Box_Data)[colnames(Box_Data) =="fouls_drawn"] <- paste0("fouls_drawn", qtr)
    colnames(Box_Data)[colnames(Box_Data) =="fouls_committed"] <- paste0("fouls_committed", qtr)
    colnames(Box_Data)[colnames(Box_Data) =="reb_qtr"] <- paste0("reb", qtr)
    colnames(Box_Data)[colnames(Box_Data) =="stl_qtr"] <- paste0("stl", qtr)
    colnames(Box_Data)[colnames(Box_Data) =="blk_qtr"] <- paste0("blk", qtr)
    colnames(Box_Data)[colnames(Box_Data) =="lq_secs"] <- paste0("q_secs", qtr)
    
  }
  ##Merge everything
  
  Box_Data <- Fouls %>% count(pbp.DRAWN_FOUL_PLAYER_ID) %>% rename(c("player_id" = "pbp.DRAWN_FOUL_PLAYER_ID", "fouls_drawn" = "n")) %>% full_join(Box_Data, by = "player_id")
  Box_Data <- Fouls %>% count(pbp.FOULED_BY_PLAYER_ID) %>% rename(c("player_id" = "pbp.FOULED_BY_PLAYER_ID", "fouls_committed" = "n")) %>% full_join(Box_Data, by = "player_id")
  
  
  Box_Data_List[[index]] <- Box_Data
}

Box_Data <- rbindlist(Box_Data_List)
Box_Data$bbref_pos <- round(Box_Data$bbref_pos %>% as.character() %>% as.numeric())

#Box_Data$mp[Box_Data$year == 2016 & !is.na(Box_Data$mp)] <- Box_Data$mp[Box_Data$year == 2016 & !is.na(Box_Data$mp)] * 10
saveRDS(Box_Data, file = "boxdata_qt_breakdown.rds")


