setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("analysis_multiyear.R")
library(stargazer)
library(dplyr)
library(ggplot2)
library(lubridate)



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## Estimates on the size of last-two-minute star bias, over time

lm_list = list()
year_vec = 2009:2016
for (index in 1:7)
{
   YEAR = year_vec[index]
   lm_list[[index]] <- lm(fouls_drawn ~ all_star + fouls_committed +
         mp + tm_usg + ts + ws + ovorp + o_bpm + orb + drb + stl + blk + per +
         ft_pc + yrs_experience + as.factor(bbref_pos),
       data = Box_Data %>% filter(mp > 400) %>% filter(year == YEAR) %>% na.omit())
}
lm01 <- lm_list[[1]]
lm02 <- lm_list[[2]]
lm03 <- lm_list[[3]]
lm04 <- lm_list[[4]]
lm05 <- lm_list[[5]]
lm06 <- lm_list[[6]]
lm07 <- lm_list[[7]]

stargazer(lm01,lm02,lm03,lm04,lm05,lm06,lm07, 
align = TRUE, 
no.space = FALSE,
omit.stat=c("f", "ser"),
column.sep.width = "-8pt")
############

p1 <- Box_Data %>% filter(mp > 400) %>% ggplot() +
  geom_point(aes(y = fouls_drawn, x = ovorp, color = all_star, size = all_star)) +
  scale_color_manual(values = c("grey","red")) +
  scale_size_manual(values = c(0.2,1)) +
  theme_bw()+ theme(legend.position="bottom")

p2 <- Box_Data %>% filter(mp > 400) %>% ggplot() +
  geom_point(aes(y = fouls_drawn, x = per, color = all_star, size = all_star)) +
  scale_color_manual(values = c("grey","red")) +
  scale_size_manual(values = c(0.2,1)) +
  theme_bw()+ theme(legend.position="bottom")

p3 <- Box_Data %>% filter(mp > 400) %>% ggplot() +
  geom_point(aes(y = fouls_drawn, x = orb, color = all_star, size = all_star)) +
  scale_color_manual(values = c("grey","red")) +
  scale_size_manual(values = c(0.2,1)) +
  theme_bw()+ theme(legend.position="bottom")

p4 <- Box_Data %>% filter(mp > 400) %>% ggplot() +
  geom_point(aes(y = fouls_drawn, x = ws, color = all_star, size = all_star)) +
  scale_color_manual(values = c("grey","red")) +
  scale_size_manual(values = c(0.2,1)) +
  theme_bw()+ theme(legend.position="bottom")

multiplot(p1, p2, p3, p4, cols=2)
