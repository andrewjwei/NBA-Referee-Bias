setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Box_Data <- readRDS("boxdata_qt_breakdown.rds")

Box_Data1 <- Box_Data
Box_Data2 <- Box_Data
Box_Data3 <- Box_Data
Box_Data4 <- Box_Data

Box_Data1$fd <- Box_Data1$fouls_drawn1
Box_Data1$fc <- Box_Data1$fouls_committed1
Box_Data1$rebounds <- Box_Data1$reb1
Box_Data1$steals <- Box_Data1$stl1
Box_Data1$blocks <- Box_Data1$blk1
Box_Data1$Quarter <- 1

Box_Data2$fd <- Box_Data1$fouls_drawn2
Box_Data2$fc <- Box_Data1$fouls_committed2
Box_Data2$rebounds <- Box_Data1$reb2
Box_Data2$steals <- Box_Data1$stl2
Box_Data2$blocks <- Box_Data1$blk2
Box_Data2$Quarter <- 2

Box_Data3$fd <- Box_Data1$fouls_drawn3
Box_Data3$fc <- Box_Data1$fouls_committed3
Box_Data3$rebounds <- Box_Data1$reb3
Box_Data3$steals <- Box_Data1$stl3
Box_Data3$blocks <- Box_Data1$blk3
Box_Data3$Quarter <- 3

Box_Data4$fd <- Box_Data1$fouls_drawn4
Box_Data4$fc <- Box_Data1$fouls_committed4
Box_Data4$rebounds <- Box_Data1$reb4
Box_Data4$steals <- Box_Data1$stl4
Box_Data4$blocks <- Box_Data1$blk4
Box_Data4$Quarter <- 4

Box_Data <- rbindlist(list(Box_Data1, Box_Data2, Box_Data3, Box_Data4))

lm_new <- lm(fd ~ all_star * as.factor(Quarter) + as.factor(year) + fc +
     mp * tm_usg + ts + ws + ovorp + o_bpm + rebounds + blocks + steals + per +
     ft_pc + yrs_experience + as.factor(bbref_pos),
   data = Box_Data %>% filter(mp > 400) %>% filter(q_secs1 > 6000) %>%
     filter(q_secs2 > 6000) %>% filter(q_secs3 > 6000) %>% filter(q_secs4 > 6000))
lm_new %>% summary()
lm_new %>% stargazer()


