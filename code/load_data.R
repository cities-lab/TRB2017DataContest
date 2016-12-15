if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(dplyr, readr, magrittr, stringr, readxl, tidyr, rgdal, ggplot2)

m0401 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_01_DDP.csv")
m0401.o <- m0401 %>% group_by(TAZ_ID=as.character(Origin_Zone)) %>% 
  summarize(trips=sum(Count)) %>% 
  ungroup()


# Setting 
# Data organization 
m0401 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_01_DDP.csv")
m0402 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_02_DDP.csv")
m0403 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_03_DDP.csv")
m0404 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_04_DDP.csv")
m0405 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_05_DDP.csv")
m0406 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_06_DDP.csv")
m0407 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_07_DDP.csv")
m0408 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_08_DDP.csv")
m0409 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_09_DDP.csv")
m0410 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_10_DDP.csv")
m0411 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_11_DDP.csv")
m0412 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_12_DDP.csv")
m0413 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_13_DDP.csv")
m0414 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_14_DDP.csv")
m0415 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_15_DDP.csv")
m0416 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_16_DDP.csv")
m0417 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_17_DDP.csv")
m0418 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_18_DDP.csv")
m0419 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_19_DDP.csv")
m0420 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_20_DDP.csv")
m0421 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_21_DDP.csv")
m0422 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_22_DDP.csv")
m0423 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_23_DDP.csv")
m0424 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_24_DDP.csv")
m0425 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_25_DDP.csv")
m0426 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_26_DDP.csv")
m0427 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_27_DDP.csv")
m0428 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_28_DDP.csv")
m0429 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_29_DDP.csv")
m0430 <- read.csv("data/TRBContest_Deliverables/trip_leg_matrix_2014_04_30_DDP.csv")

trip.combine <- rbind(m0401, m0402, m0403, m0404, m0405, m0406, m0407, m0408, m0409, m0410,
                      m0411, m0412, m0413, m0414, m0415, m0416, m0417, m0418, m0419, m0420,
                      m0421, m0422, m0423, m0424, m0425, m0426, m0427, m0428, m0429, m0430) 

week.df <- data.frame(date=c(1:30), week=c(rep(1, 5), rep(2, 7), rep(3, 7), rep(4, 7), rep(5, 4)))
time.df <- data.frame(Time_of_Day=c("H00:H01", "H01:H02", "H02:H03", "H03:H04", "H04:H05", "H05:H06", 
                                    "H06:H07", "H07:H08", "H08:H09", "H09:H10", "H10:H11", "H11:H12",
                                    "H12:H13", "H13:H14", "H14:H15", "H15:H16", "H16:H17", "H17:H18",
                                    "H18:H19", "H19:H20", "H20:H21", "H21:H22", "H22:H23", "H23:H24"),
                      time=c(0:23)
                      )

trip.dataset <- trip.combine %>%
                mutate(date=as.numeric(substr(Start_Date, 7, 8))) %>%
                left_join(week.df) %>%
                left_join(time.df) %>%
                mutate(peak=ifelse(Time_of_Day %in% c("H06:H07", "H07:H08", "H08:H09", "H09:H10", # https://en.wikipedia.org/wiki/Rush_hour
                                                      "H16:H17", "H17:H18", "H18:H19", "H19:H20"), "peak", "offpeak"),
                       weekend=ifelse(Aggregation %in% c("Sat", "Sun"), "Weekend", "Weekday"),
                       time=factor(time),
                       peak=factor(peak),
                       date=factor(date),
                       weekday=factor(Aggregation),
                       weekend=factor(weekend),
                       week=factor(week),
                       Subscriber_Class=factor(Subscriber_Class),
                       TAZ_ID=ifelse(Origin_Zone>4005, Origin_Zone-999999999990000, Origin_Zone)
                       ) 











