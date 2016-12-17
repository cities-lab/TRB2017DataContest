do_animation <- function(trips.df, Weekend) {
  trips.ad <- trips.df %>% 
    #filter(Start_Date=="2014-04-01", Start_Hour==8)
    group_by(Origin_Zone, Destination_Zone, Start_Weekend, Start_Hour) %>% 
    summarize(trips.sum=sum(Count, na.rm=TRUE),
              days.sum=n_distinct(Start_Date, na.rm=TRUE),
              trips=trips.sum/days.sum)
  #filter(Start_Date=="2014-04-01")
  
  taz.x.start_hour <- expand.grid(TAZ_ID=unique(c(trips.df$Origin_Zone, 
                                                trips.df$Destination_Zone)),
                                Start_Hour=unique(trips.df$Start_Hour)) 

  cumtrips.out <- taz.x.start_hour %>% 
    left_join(trips.ad %>% filter(Start_Weekend==Weekend), by=c("TAZ_ID"="Origin_Zone", "Start_Hour"="Start_Hour")) %>%
    mutate(trips=ifelse(is.na(trips), 0, trips)) %>% 
    group_by(TAZ_ID, Start_Hour) %>% 
    summarize(trips=sum(trips)) %>% 
    mutate(cumtrips.out=cumsum(trips)) %>% 
    select(TAZ_ID, Start_Hour, cumtrips.out)
  
  cumtrips.in <- taz.x.start_hour %>% 
    left_join(trips.ad  %>% filter(Start_Weekend==Weekend), by=c("TAZ_ID"="Destination_Zone", "Start_Hour"="Start_Hour")) %>%
    mutate(trips=ifelse(is.na(trips), 0, trips)) %>%
    group_by(TAZ_ID, Start_Hour) %>% 
    summarize(trips=sum(trips)) %>% 
    mutate(cumtrips.in=cumsum(trips)) %>% 
    select(TAZ_ID, Start_Hour, cumtrips.in)
  
  cumtrips <- cumtrips.in %>% 
    left_join(cumtrips.out) %>% 
    mutate(cumtrips=cumtrips.in - cumtrips.out,
           cumtrips.sign=ifelse(cumtrips>0, 1, -1),
           ln.cumtrips=ifelse(cumtrips!=0, log10(abs(cumtrips)) * cumtrips.sign, 0),
           #cumtrips=ifelse(cumtrips<0, 0, cumtrips),
           id=as.character(TAZ_ID))
  
  #cumtrips %<>% filter(Start_Hour==8)
  
  v.min <- min(cumtrips$ln.cumtrips)
  v.max <- max(cumtrips$ln.cumtrips)
  
  spdf.cumtrips <- spdf.ff %>% 
    left_join(cumtrips, by=c("id"="id"))
  
  (map <- ggplot() + geom_polygon(aes(x = long, y = lat, group=group, fill=ln.cumtrips, frame=Start_Hour),
                                  data = spdf.cumtrips, color ="black", 
                                  size = 0) +
      coord_map()+
      scale_fill_gradient2("log10(trips)", limits=c(v.min, v.max), low = "darkgreen", mid = "white", high = "darkred", na.value="white") +
      #scale_fill_distiller(name="trips arrived", palette = "RdBu", breaks = pretty_breaks(n = 5))+
      theme() + ggtitle("Net Trips by Hour "))
  gganimate(map, paste0("docs/map_animation.", Weekend, ".gif"))
}

do_map <- function(cumtrips, Weekend, Hour) {
  trips.ad <- trips.df %>% 
    #filter(Start_Date=="2014-04-01", Start_Hour==8)
    group_by(Origin_Zone, Destination_Zone, Start_Weekend, Start_Hour) %>% 
    summarize(trips.sum=sum(Count, na.rm=TRUE),
              days.sum=n_distinct(Start_Date, na.rm=TRUE),
              trips=trips.sum/days.sum)
  #filter(Start_Date=="2014-04-01")
  
  taz.x.start_hour <- expand.grid(TAZ_ID=unique(c(trips.df$Origin_Zone, 
                                                  trips.df$Destination_Zone)),
                                  Start_Hour=unique(trips.df$Start_Hour)) 
  
  cumtrips.out <- taz.x.start_hour %>% 
    left_join(trips.ad %>% filter(Start_Weekend==Weekend), by=c("TAZ_ID"="Origin_Zone", "Start_Hour"="Start_Hour")) %>%
    mutate(trips=ifelse(is.na(trips), 0, trips)) %>% 
    group_by(TAZ_ID, Start_Hour) %>% 
    summarize(trips=sum(trips)) %>% 
    mutate(cumtrips.out=cumsum(trips)) %>% 
    select(TAZ_ID, Start_Hour, cumtrips.out)
  
  cumtrips.in <- taz.x.start_hour %>% 
    left_join(trips.ad  %>% filter(Start_Weekend==Weekend), by=c("TAZ_ID"="Destination_Zone", "Start_Hour"="Start_Hour")) %>%
    mutate(trips=ifelse(is.na(trips), 0, trips)) %>%
    group_by(TAZ_ID, Start_Hour) %>% 
    summarize(trips=sum(trips)) %>% 
    mutate(cumtrips.in=cumsum(trips)) %>% 
    select(TAZ_ID, Start_Hour, cumtrips.in)
  
  cumtrips <- cumtrips.in %>% 
    left_join(cumtrips.out) %>% 
    mutate(cumtrips=cumtrips.in - cumtrips.out,
           cumtrips.sign=ifelse(cumtrips>0, 1, -1),
           ln.cumtrips=ifelse(cumtrips!=0, log10(abs(cumtrips)) * cumtrips.sign, 0),
           #cumtrips=ifelse(cumtrips<0, 0, cumtrips),
           id=as.character(TAZ_ID))
  
  cumtrips.h <- cumtrips %>% filter(Start_Hour==Hour)
  
  v.min <- min(cumtrips$ln.cumtrips)
  v.max <- max(cumtrips$ln.cumtrips)
  
  spdf.cumtrips <- spdf.ff %>% 
    left_join(cumtrips.h, by=c("id"="id"))
  
  (map.h <- ggplot() + geom_polygon(aes(x = long, y = lat, group=group, fill=ln.cumtrips),
                                    data = spdf.cumtrips, color ="black", 
                                    size = 0) +
      coord_map()+
      scale_fill_gradient2(limits=c(v.min, v.max), low = "darkgreen", mid = "white", high = "darkred", na.value="white") +
      #scale_fill_distiller(name="trips arrived", palette = "RdBu", breaks = pretty_breaks(n = 5))+
      theme_nothing())
  map.h
}
