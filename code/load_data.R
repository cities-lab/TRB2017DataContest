if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(dplyr, readr, magrittr, stringr, readxl, tidyr, rgdal)

m0401 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_01_DDP.csv")
m0401.o <- m0401 %>% group_by(TAZ_ID=as.character(Origin_Zone)) %>% 
  summarize(trips=sum(Count)) %>% 
  ungroup()

