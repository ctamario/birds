
########################

########################

#### GBIF it a try :)

getwd()

inorn_gbif <- read.delim("0057541-231002084531237/0057541-231002084531237.csv", row.names = NULL)



inorn_geo<- st_as_sf(inorn_gbif, coords = c("decimalLongitude", "decimalLatitude"), crs=4326)


inorn_geo <- inorn_geo %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

head(inorn_geo)

inorn_geo$date <- as.Date(paste(sep="-",inorn_geo$year,inorn_geo$month,inorn_geo$day))
inorn_geo$date <- as.Date(substr(inorn_geo$eventDate,1,10), format="%Y-%m-%d")
inorn_geo$jdate <- as.numeric(format(inorn_geo$date, "%j"))

inorn_geo <- inorn_geo %>% filter(jdate > 229)

inorn_geo1 <- inorn_geo
inorn_geo1$alpha <- 0.03

inorn_geo2 <- inorn_geo
inorn_geo2$jdate <- inorn_geo1$jdate+1
inorn_geo2$alpha <- 0.015

inorn_geo <- rbind(inorn_geo1, inorn_geo2)

for(i in 230:350){
  p <- ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(-11.201912, 26.767909), ylim = c(35.352533, 69.242867), expand = FALSE)+
    geom_point(data=inorn_geo %>%filter(jdate == i), aes(y=lat, x=lon, alpha=alpha), alpha=0.05, size=4, color="mediumblue")+
    theme_classic()
  p1 <- ggplot(data = inorn_geo)+
    geom_histogram(aes(jdate, fill = jdate == i), binwidth=1)+
    scale_x_continuous(breaks = seq(230, 360, 20))+
    scale_fill_manual(values=c("lightgrey", "black"))+
    xlab("Julianskt datum")+
    theme_classic()+
    theme(legend.position = "false")
  p_comb <- grid.arrange(p, p1, heights=c(2,0.50))
  ggsave(file=paste0("C:/jobb/KFS/out/first",i,".png"), plot=p_comb, width=4, height=6)
}



list.files(path='C:/jobb/KFS/out', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=20) %>% # animates, can opt for number of loops
  image_write("inornatus_west_europe2.gif")


#########################
#########################
#########################

getwd()

inorn_gbif <- read.delim("inornatus_eurasia/0059073-231002084531237.csv", row.names = NULL)

table(!is.na(inorn_gbif$decimalLongitude))

inorn_gbif <- inorn_gbif[!is.na(inorn_gbif$decimalLatitude),]
inorn_gbif <- inorn_gbif[!is.na(inorn_gbif$decimalLongitude),]

inorn_geo<- st_as_sf(inorn_gbif, coords = c("decimalLongitude", "decimalLatitude"), crs=4326)

inorn_geo <- inorn_geo %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

head(inorn_geo)

inorn_geo$date <- as.Date(paste(sep="-",inorn_geo$year,inorn_geo$month,inorn_geo$day))
inorn_geo$date <- as.Date(substr(inorn_geo$eventDate,1,10), format="%Y-%m-%d")
inorn_geo$jdate <- as.numeric(format(inorn_geo$date, "%j"))

ortho <- paste0('+proj=ortho +lat_0=50 +lon_0=80 +x_0=0 +k_0=1000 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')

inorn_geo <- inorn_geo %>% st_transform(crs = ortho)
inorn_geo <- inorn_geo %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

inorn_geo <- inorn_geo %>% select(lon, lat, date, jdate, individualCount)

inorn_geo$alpha1 <- 0.10

inorn_geo1 <- inorn_geo
inorn_geo2 <- inorn_geo
inorn_geo3 <- inorn_geo
inorn_geo4 <- inorn_geo

inorn_geo1$alpha1 <- 0.10/2
inorn_geo1$jdate <- inorn_geo$jdate+1
inorn_geo2$alpha1 <- 0.10/3
inorn_geo2$jdate <- inorn_geo$jdate+2
inorn_geo3$alpha1 <- 0.10/4
inorn_geo3$jdate <- inorn_geo$jdate+3
inorn_geo4$alpha1 <- 0.10/5
inorn_geo4$jdate <- inorn_geo$jdate+4

inorn_geo10 <- rbind(inorn_geo,inorn_geo1,inorn_geo2,inorn_geo3,inorn_geo4)

inorn_geo10$size <- (log(inorn_geo$individualCount)+1)/2

for(i in 1:365){
  plot1 <- ggplot(data = world) +
    geom_sf() +
    coord_sf(crs=ortho, xlim=c(-5500000, 5500000), ylim=c(-4500000, 3500000))+
    geom_point(data=inorn_geo10 %>%filter(jdate == i), aes(y=lat, x=lon, alpha=alpha, size=size), alpha=0.05, color="mediumblue")+
    ggtitle(paste0("Tajgasångare: ", format(as.Date(as.character(i), "%j"),"%d %b")))+
    scale_alpha(limits=c(0,1))+
    scale_size(limits=c(0.5,4.16))+
    theme_classic()+
    theme(legend.position="none")
  ggsave(file=paste0("C:/jobb/KFS/inornatus_eurasia/first",str_pad(i, 3, pad="0"),".png"), plot=plot1, width=6, height=5)
}



list.files(path='C:/jobb/KFS/inornatus_eurasia', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=20, optimize=T) %>% # animates, can opt for number of loops
  image_write("inornatus_eurasia2.gif")

table(inorn_geo$size)

split_pop <- st_read("C:/jobb/KFS/Two_ways.shp")

split_pop <- split_pop %>% st_transform(crs=ortho)

inorn_geo_split <- inorn_geo %>% st_join(split_pop)

inorn_geo_traj <- inorn_geo_split %>% dplyr::group_by(jdate, direction) %>% dplyr::summarise(lat_mean = mean(lat), lon_mean = mean(lon), lat_sd=sd(lat), lon_sd=sd(lon), n=n())

inorn_geo_traj2 <- inorn_geo_traj %>% filter(!is.na(direction))


inorn_geo_traj2_west <- inorn_geo_traj2 %>% filter(direction == "west")
inorn_geo_traj2_east <- inorn_geo_traj2 %>% filter(direction == "east")

inorn_lines_west <- data.frame(jdate = 1:365)
inorn_lines_east <- data.frame(jdate = 1:365)

inorn_lines_west <- inorn_lines_west %>% dplyr::left_join(inorn_geo_traj2_west, by = c("jdate"))
inorn_lines_east <- inorn_lines_east %>% dplyr::left_join(inorn_geo_traj2_east, by = c("jdate"))

inorn_lines_west$lat_mean2 <- inorn_lines_west$lat_mean
inorn_lines_west$lat_mean2[5:(nrow(inorn_lines_west))] <- rollapply(inorn_lines_west$lat_mean, 5, FUN=function(x) mean(x, na.rm=T), align = "right")
inorn_lines_west$lon_mean2 <- inorn_lines_west$lon_mean
inorn_lines_west$lon_mean2[5:(nrow(inorn_lines_west))] <- rollapply(inorn_lines_west$lon_mean, 5, FUN=function(x) mean(x, na.rm=T), align = "right")

inorn_lines_east$lat_mean2 <- inorn_lines_east$lat_mean
inorn_lines_east$lat_mean2[5:(nrow(inorn_lines_east))] <- rollapply(inorn_lines_east$lat_mean, 5, FUN=function(x) mean(x, na.rm=T), align = "right")
inorn_lines_east$lon_mean2 <- inorn_lines_east$lon_mean
inorn_lines_east$lon_mean2[5:(nrow(inorn_lines_east))] <- rollapply(inorn_lines_east$lon_mean, 5, FUN=function(x) mean(x, na.rm=T), align = "right")

inorn_lines_west$lat_sd2 <- inorn_lines_west$lat_sd
inorn_lines_west$lat_sd2[5:(nrow(inorn_lines_west))] <- rollapply(inorn_lines_west$lat_sd, 5, FUN=function(x) mean(x, na.rm=T), align = "right")
inorn_lines_west$lon_sd2 <- inorn_lines_west$lon_sd
inorn_lines_west$lon_sd2[5:(nrow(inorn_lines_west))] <- rollapply(inorn_lines_west$lon_sd, 5, FUN=function(x) mean(x, na.rm=T), align = "right")

inorn_lines_east$lat_sd2 <- inorn_lines_east$lat_sd
inorn_lines_east$lat_sd2[5:(nrow(inorn_lines_east))] <- rollapply(inorn_lines_east$lat_sd, 5, FUN=function(x) mean(x, na.rm=T), align = "right")
inorn_lines_east$lon_sd2 <- inorn_lines_east$lon_sd
inorn_lines_east$lon_sd2[5:(nrow(inorn_lines_east))] <- rollapply(inorn_lines_east$lon_sd, 5, FUN=function(x) mean(x, na.rm=T), align = "right")

inorn_lines_east$n[is.na(inorn_lines_east$n)] <- 0 
inorn_lines_west$n[is.na(inorn_lines_west$n)] <- 0 

for(i in 220:240){
  plot1 <- ggplot(data = world) +
    geom_sf() +
    coord_sf(crs=ortho, xlim=c(-5500000, 5500000), ylim=c(-4500000, 3500000))+
    geom_point(data=df210 %>%filter(jdate == i), aes(y=lat, x=lon, alpha=alpha1), alpha=0.05, color="mediumblue")+
    geom_path(data=inorn_lines_west %>% dplyr::filter(jdate > 230 & jdate < i), aes(x=lon_mean2, y=lat_mean2, alpha=n), color="red", size=2)+
    geom_path(data=inorn_lines_east %>% dplyr::filter(jdate > 230 & jdate < i), aes(x=lon_mean2, y=lat_mean2, alpha=n), color="green", size=2)+
    ggtitle(paste0("Tajgasångare: ", format(as.Date(as.character(i), "%j"),"%d %b")))+
    scale_alpha(limits=c(0,100))+
    scale_size(limits=c(0.5,4.16))+
    theme_classic()+
    theme(legend.position="none")
  ggsave(file=paste0("C:/jobb/KFS/inornatus_eurasia/first",str_pad(i, 3, pad="0"),".png"), plot=plot1, width=6, height=5)
}

list.files(path='C:/jobb/KFS/inornatus_eurasia', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=20, optimize=T) %>% # animates, can opt for number of loops
  image_write("inornatus_eurasia4.gif")

plot(inorn_geo_split)

#########################
######################### Nedan är potentiellt skräp


p <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-30.201912, 180.767909), ylim = c(0.352533, 80.242867), expand = FALSE)+
  geom_point(data=inorn_geo %>%filter(jdate == 100), aes(y=lat, x=lon, alpha=alpha), alpha=0.05, size=4, color="mediumblue")+
  theme_classic()
p1 <- ggplot(data = inorn_geo)+
  geom_histogram(aes(jdate, fill = jdate == 100), binwidth=1)+
  scale_x_continuous(breaks = seq(1, 365, 20))+
  #scale_fill_manual(values=c("lightgrey", "black"))+
  geom_vline(xintercept=100)+
  xlab("Day of the year")+
  theme_classic()+
  theme(legend.position = "false")
p_comb <- arrangeGrob(p, p1, heights=c(2,0.50))

for(i in 1:365){
  p <- ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(-30.201912, 180.767909), ylim = c(0.352533, 80.242867), expand = FALSE)+
    geom_point(data=inorn_geo %>%filter(jdate == i), aes(y=lat, x=lon, alpha=alpha), alpha=0.05, size=2, color="mediumblue")+
    theme_classic()
  p1 <- ggplot(data = inorn_geo)+
    geom_histogram(aes(jdate, fill = jdate == i), binwidth=1)+
    scale_x_continuous(breaks = seq(1, 365, 20))+
    #scale_fill_manual(values=c("lightgrey", "black"))+
    geom_vline(xintercept=i)+
    xlab("Day of the year")+
    theme_classic()+
    theme(legend.position = "false")
  p_comb <- arrangeGrob(p, p1, heights=c(2.5,0.50))
  ggsave(file=paste0("C:/jobb/KFS/inornatus_eurasia/first",i,".png"), plot=p_comb, width=6, height=5)
}

list.files(path='C:/jobb/KFS/inornatus_eurasia', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=20) %>% # animates, can opt for number of loops
  image_write("inornatus_eurasia.gif")





ortho <- paste0('+proj=ortho +lat_0=50 +lon_0=80 +x_0=0 +k_0=1000 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = ortho, xlim=c(-5500000, 5500000), ylim=c(-4500000, 3500000))+
  geom_point(data=inorn_test, aes(x=lon, y=lat))

#coord_sf(xlim = c(-0.201912, 30.767909), ylim = c(52.352533, 73.242867), expand = FALSE)
