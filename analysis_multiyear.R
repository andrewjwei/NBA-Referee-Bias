## Analysis for H1/H2 Fouls Drawn ~ Star Status 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#source("data_setup_multiyear.R")
Box_Data <- readRDS(file = "boxdata.rds")
library(stargazer)
library(dplyr)
library(ggplot2)
library(lubridate)

## BASIC PLOTS

table((Box_Data %>% filter(mp > 400))$year, (Box_Data %>% filter(mp > 400))$all_star) %>% stargazer()
((Box_Data %>% filter(all_star))$mp/(Box_Data %>% filter(all_star))$g) %>% mean()
((Box_Data %>% filter(!all_star))$mp/(Box_Data %>% filter(!all_star))$g) %>% mean()
(Box_Data %>% filter(!all_star))$fouls_drawn %>% mean(na.rm = T)
(Box_Data %>% filter(all_star))$fouls_drawn %>% mean(na.rm = T)
(Box_Data %>% filter(!all_star))$fouls_committed %>% mean(na.rm = T)
(Box_Data %>% filter(all_star))$fouls_committed %>% mean(na.rm = T)
(Box_Data %>% filter(!all_star))$g %>% mean(na.rm = T)
(Box_Data %>% filter(all_star))$g %>% mean(na.rm = T)
((Box_Data %>% filter(all_star))$fouls_drawn/(Box_Data %>% filter(all_star))$g) %>% mean(na.rm = T)
((Box_Data %>% filter(!all_star))$fouls_drawn/(Box_Data %>% filter(!all_star))$g) %>% mean(na.rm = T)

Box_Data %>% filter(mp > 400) %>% ggplot() +
  geom_point(aes(y = fouls_drawn, x = fouls_committed, color = all_star, size = all_star)) +
  scale_color_manual(values = c("grey","red")) +
  scale_size_manual(values = c(0.2,1)) +
  theme_bw()+ theme(legend.position="bottom")

Box_Data %>% filter(mp > 400) %>% ggplot() +
  geom_point(aes(y = fouls_drawn_l2m, x = fouls_committed_l2m, color = all_star, size = all_star)) +
  scale_color_manual(values = c("grey","red")) +
  scale_size_manual(values = c(0.2,1)) +
  theme_bw()+ theme(legend.position="bottom") + xlim(c(0,1)) + ylim(c(0,1))

## MODIFICATION

Box_Data$orb <- Box_Data$orb/Box_Data$mp
Box_Data$drb <- Box_Data$drb/Box_Data$mp
Box_Data$stl <- Box_Data$stl/Box_Data$mp
Box_Data$blk <- Box_Data$blk/Box_Data$mp
Box_Data$fouls_committed <- Box_Data$fouls_committed/Box_Data$mp
Box_Data$fouls_drawn <- Box_Data$fouls_drawn/Box_Data$mp

Box_Data$reb_l2m <- Box_Data$reb_l2m/Box_Data$l2m_secs * 60
Box_Data$stl_l2m <- Box_Data$stl_l2m/Box_Data$l2m_secs * 60
Box_Data$blk_l2m <- Box_Data$blk_l2m/Box_Data$l2m_secs * 60
Box_Data$fouls_committed_l2m <- Box_Data$fouls_committed_l2m/Box_Data$l2m_secs * 60
Box_Data$fouls_drawn_l2m <- Box_Data$fouls_drawn_l2m/Box_Data$l2m_secs * 60

Box_Data$reb_lq <- Box_Data$reb_lq/Box_Data$lq_secs * 60
Box_Data$stl_lq <- Box_Data$stl_lq/Box_Data$lq_secs * 60
Box_Data$blk_lq <- Box_Data$blk_lq/Box_Data$lq_secs * 60
Box_Data$fouls_committed_lq <- Box_Data$fouls_committed_lq/Box_Data$lq_secs * 60
Box_Data$fouls_drawn_lq <- Box_Data$fouls_drawn_lq/Box_Data$lq_secs * 60

## H1 Analysis

lm1 <- lm(fouls_drawn ~ all_star + as.factor(year) + fouls_committed +
     mp * tm_usg + ts + ws + ovorp + o_bpm + orb + drb + stl + blk + per +
     ft_pc + yrs_experience + all_star + as.factor(bbref_pos),
     data = Box_Data %>% filter(mp > 400))
lm1 %>% summary()

## Analysis for H3:

lm2 <- lm(fouls_drawn_l2m ~ all_star + as.factor(year) + fouls_committed_l2m +
            mp * tm_usg + ts + ws + ovorp + o_bpm + reb_l2m + stl_l2m + blk_l2m + per +
            ft_pc + yrs_experience + all_star + as.factor(bbref_pos),
            data = Box_Data %>% filter(mp > 400) %>% filter(l2m_secs > 0))
lm2 %>% summary()


## Analysis for More

lm3 <- lm(fouls_drawn_lq ~ all_star + as.factor(year) + fouls_committed_lq +
            mp * tm_usg + ts + ws + ovorp + o_bpm + reb_lq + stl_lq + blk_lq + per +
            ft_pc + yrs_experience + all_star + as.factor(bbref_pos),
          data = Box_Data %>% filter(mp > 400) %>% filter(lq_secs > 0))
lm3 %>% summary()


summary((Box_Data %>% filter(mp > 400))$fouls_drawn)
summary((Box_Data %>% filter(mp > 400) %>% filter(l2m_secs > 0))$fouls_drawn_l2m)
summary((Box_Data %>% filter(mp > 400) %>% filter(lq_secs > 0))$fouls_drawn_lq)

### More Plots


Box_Data %>% filter(mp > 400) %>%
  ggplot(aes(x = fouls_drawn, color = all_star)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") +
  theme_bw()+ theme(legend.position="bottom")

Box_Data %>% filter(mp > 400) %>% filter(l2m_secs > 360) %>%
  ggplot(aes(x = fouls_drawn_l2m, color = all_star)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") +
  theme_bw()+ theme(legend.position="bottom")

Box_Data %>% filter(mp > 400) %>% filter(lq_secs > 1440) %>%
  ggplot(aes(x = fouls_drawn_lq, color = all_star)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") +
  theme_bw()+ theme(legend.position="bottom")

