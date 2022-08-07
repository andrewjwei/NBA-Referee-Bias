############## Data Gathering
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(stargazer)
library(fuzzyjoin)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(".//834 data//PbP_15_16.Rda")
Box_Data <- read.csv(".//jgrosz99-nba-player-data-1978-2016//data//nba_season_data.csv")
Box_Data <- Box_Data %>% filter(year == 2016)
All_Star <- read.csv(".//gmoney-nba-all-stars-2000-2016//data//nba_all_star_games.csv")
All_Star <- (All_Star %>% filter(year == 2016))$player # list of all stars
Kaggle_Box <- read.csv(".//kaggle//Seasons_Stats.csv")
Kaggle_Box <- Kaggle_Box %>% filter(Year == 2016)

# Find player-id-name matching [2016]

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

# add on player position from kaggle

# there are only 2 players with two positions. just remove them
Ktemp <- unique(data.frame(Position = Kaggle_Box$Pos,
                           player_name_kaggle = Kaggle_Box$Player,
                           ft_pc = Kaggle_Box$FT.))
Ktemp$player_name_kaggle <- as.character(Ktemp$player_name_kaggle)
Ktemp$Position[Ktemp$player_name_kaggle == "Sonny Weems"] <-"SG"
Ktemp$Position[Ktemp$player_name_kaggle == "Channing Frye"] <- "PF"
Ktemp <- unique(Ktemp)
Ktemp <- Ktemp %>% group_by(player_name_kaggle, Position) %>% summarize(ft_pc = mean(ft_pc))

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
# note that if you were fouled, then player 1 is the name of the player that was fouled by,
#and player 2 is the name of the player who drew the foul
Fouls <- na.omit(Fouls)


#get all close foul events
pbp$MINS <- as.data.frame(do.call(rbind, strsplit(as.character(pbp$PCTIMESTRING), ":")))[1]
pbp$SECS <- as.data.frame(do.call(rbind, strsplit(as.character(pbp$PCTIMESTRING), ":")))[2]


FoulsL2M <- data.frame(SCOREMARGIN = pbp$SCOREMARGIN,
                       MINS = pbp$MINS,
                       PERIOD = pbp$PERIOD,
                       GAME_ID = pbp$GAME_ID,
                       SECS = pbp$SECS,
                       DRAWN_FOUL_PLAYER_ID = pbp$DRAWN_FOUL_PLAYER_ID,
                       FOULED_BY_PLAYER_ID = pbp$FOULED_BY_PLAYER_ID,
                       FOUL_TYPE = pbp$FOUL_TYPE,
                       FOUL_COUNT = pbp$FOUL_COUNT)

FoulsL2M <- FoulsL2M %>% rename(MINS = V1, SECS = V2)
FoulsL2M$MINS <- FoulsL2M$MINS %>% as.character() %>% as.numeric()
FoulsL2M$SECS <- FoulsL2M$SECS %>% as.character() %>% as.numeric()
## keep only L2M of within last 2 mins

FoulsL2M <- FoulsL2M[(FoulsL2M$MINS < 2) & (FoulsL2M$PERIOD > 3),]

# then, filter on score margin

FoulsL2M$KEEP <- FALSE
FoulsL2M$SCOREMARGIN <-FoulsL2M$SCOREMARGIN %>% as.character()
FoulsL2M$SCOREMARGIN[FoulsL2M$SCOREMARGIN == "TIE"] = "0"
FoulsL2M$SCOREMARGIN <-FoulsL2M$SCOREMARGIN %>% as.numeric()

## check score margin
for(i in 1:length(unique(FoulsL2M$GAME_ID))){
  game = unique(FoulsL2M$GAME_ID)[i]
  if(sum(FoulsL2M$SCOREMARGIN[FoulsL2M$GAME_ID == game] %>% abs() < 5, na.rm = T) > 0)
  {
    FoulsL2M$KEEP[FoulsL2M$GAME_ID == game] <- TRUE
  }
  else
  {
    #do not keep
    FoulsL2M$KEEP[FoulsL2M$GAME_ID == game] <- FALSE
  }
}

FoulsL2M$SCOREMARGIN <- NULL
FoulsL2M <- na.omit(FoulsL2M)
FoulsL2M <- FoulsL2M %>% filter(KEEP == TRUE)
FoulsL2M$KEEP <- NULL

#get all violation events.

Violations <- data.frame(pbp$VIOLATION, pbp$VIOLATION_PLAYER_ID)
Violations <- na.omit(Violations)
