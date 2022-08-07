## Analysis for H1/H2 Fouls Drawn ~ Star Status 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data_setup.R")

#tm-usg - tea usage
#mp - mins played /10
Box_Data <- Fouls %>% count(pbp.DRAWN_FOUL_PLAYER_ID) %>% rename(c("player_id" = "pbp.DRAWN_FOUL_PLAYER_ID", "fouls_drawn" = "n")) %>% full_join(Box_Data, by = "player_id")
lm(fouls_drawn ~ mp + tm_usg + ts + ftr + ows + ovorp + o_bpm + orb + stl + blk + per + ft_pc + raw_contrib + production +yrs_experience + all_star *Position,data = Box_Data) %>% summary()
Box_Data <- Fouls %>% count(pbp.FOULED_BY_PLAYER_ID) %>% rename(c("player_id" = "pbp.FOULED_BY_PLAYER_ID", "fouls_committed" = "n")) %>% full_join(Box_Data, by = "player_id")
lm(fouls_committed ~ all_star + mp + tm_usg + Position,data = Box_Data) %>% summary()


## BASIC PLOTS

ggplot(Box_Data) + geom_point(aes(y = fouls_drawn, x = fouls_committed, col = all_star))


## Analysis for H3:

Box_Data <- FoulsL2M %>% count(DRAWN_FOUL_PLAYER_ID) %>% rename(c("player_id" = "DRAWN_FOUL_PLAYER_ID", "fouls_drawn_l2m" = "n")) %>% full_join(Box_Data, by = "player_id")
lm(fouls_drawn_l2m ~ mp + tm_usg + ts + ftr + ows + ovorp + o_bpm + orb + stl + blk + per + ft_pc + raw_contrib + production +yrs_experience + all_star *Position,data = Box_Data) %>% summary()
