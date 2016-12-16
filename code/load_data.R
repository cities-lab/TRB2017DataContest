if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(dplyr, readr, magrittr, stringr, readxl, tidyr, rgdal, ggplot2)

# m0401 <- read_csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_01_DDP.csv")
# m0401.o <- m0401 %>% group_by(TAZ_ID=as.character(Origin_Zone)) %>% 
#   summarize(trips=sum(Count)) %>% 
#   ungroup()

source("code/functions.R")

# Setting 
# Data organization
trips.df <- NULL 
for (day in c(1:30)) {
  file.name <- paste0("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_", str_pad(day, 2, pad = "0"), "_DDP.csv")
  trips.d1 <- read_csv(file.name)
  if (is.null(trips.df)) trips.df <- trips.d1
  else trips.df <- bind_rows(trips.df, trips.d1)
}

trips.df  %<>%  mutate(
  Start_Date=as.Date(strptime(Start_Date, "%Y%m%d")),
  End_Date=as.Date(strptime(End_Date, "%Y%m%d")),
  Start_Day =format(Start_Date, "%a"),      #day
  Start_Weekend=ifelse(Start_Day %in% c("Sun", "Sat"), "Weekend", "Weekday"),
  Start_Day=factor(Start_Day, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
  Start_Hour = as.integer(str_sub(Time_of_Day, 2, 3)),
  peak = Start_Hour %in% c(6, 7, 8, 9, 16, 17, 18, 19),
  Origin_Zone = ifelse(Origin_Zone>4005, Origin_Zone-999999999990000, Origin_Zone),
  Destination_Zone = ifelse(Destination_Zone>4005, Destination_Zone-999999999990000, Destination_Zone),
  TAZ_ID=Origin_Zone
)

shpfile <- 'data/taz/OUATS_AirSage_TAZ.shp'
spdf <- read.shpfile(shpfile)

spdf.ff <- fortify(spdf, region="TAZ_ID")