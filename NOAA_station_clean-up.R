library(tidyverse)

lp.clim<-read.csv("lp.clim.csv")

stations<-as.vector(unique(lp.clim$station_id))

#have to make a new empty dataset first

#for (i in 1:nrow(lp.all)) {
#  latitude<-lp.all$latitude[i]
#  longitude<-lp.all$longitude[i]
#  if(latitude>=0){
#    lp.all$lat[i]<-substr(lp.all$latitude[i],1,4)}
#  else {lp.all$lat[i]<-substr(lp.all$latitude[i],1,5)
#  }
#}

#import data from each csv into a list
NOAA.clim.data <- list()
for(x in unique(stations)){
  NOAA.clim.data[[x]] <- read.csv(~/NOAA_climate_data_1/x.csv)
}

#split the list into individual dataframes
NOAA.clim.data <- NOAA.clim.data %>% set_names(stations)
invisible(list2env(NOAA.clim.data,.GlobalEnv))
ls()



stations.temp<-as.dataframe()

for(i in 1:nrow(stations)){
i<-read.csv("~/NOAA_climate_data/i.csv")
if((is.na(i$temperature))/nrow(i$temperature)<=0.1){
stations.temp[i]<-i}












































^G Get Help      ^O Write Out     ^W Where Is      ^K Cut Text      ^J Justify       ^C Cur Pos       M-U Undo         M-A Mark Text    M-] To Bracket   M-▲ Previous
^X Exit          ^R Read File     ^\ Replace       ^U Uncut Text    ^T To Spell      ^_ Go To Line    M-E Redo         M-6 Copy Text    M-W WhereIs Next M-▼ Next

