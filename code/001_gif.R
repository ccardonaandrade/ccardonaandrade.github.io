library(haven)
library(data.table)
library(foreign)
library(countrycode)
library(plyr)
library(operators)
library(dplyr)
library(tidyverse)
library(sp) 
library(rgeos)
library(rgdal)

library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library("ggspatial")
library(spdep)
library(vroom)
library(states)
library(ncdf4) # package for netcdf manipulation

library(raster)
library(patchwork)

#remotes::install_github("afrimapr/afrilearndata")
library(afrilearndata)

library(devtools)
#devtools::install_github("sboysel/murdock")
library(murdock)
library(lwgeom)
library(gganimate)
library(magick)
library(transformr)


setwd("C:/Users/ccard/Dropbox/AfricaWeather")

#WORLD MAP
world <- ne_countries(scale = "medium", returnclass = "sf")

#Calling African Cells
load(file = "clean/africa_cells.RData")
CELL<-st_as_sf(as.data.table(cell))
CELLFINAL<-CELL
rm(cell)

ORIGINAL<-data.table(read_dta("clean/cells_weather.dta"))
ORIGINAL<-ORIGINAL[,c(1:4,9)]

ORIGINAL<-st_as_sf(left_join(ORIGINAL,CELL))

for (i in 2000:2015){
  
DATA<-filter(ORIGINAL,year==i)


DATA  %>%
  ggplot(aes()) + geom_sf(data = DATA, aes(fill=DATA[[5]]),color = NA)  +  labs(title = paste0("Precipitation (mm) -"," ",i) ) + 
  geom_sf(data = world, fill = "transparent") + theme_bw() +  
  coord_sf(xlim = c(-30, 60), ylim = c(-40, 45), expand = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        title =element_text(size=20, face='bold'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_text(size=14),
        legend.key = element_rect( fill = "gray92"),
        legend.position = c(0.2, 0.3),
        legend.key.size = unit(0.6, 'cm'),
        legend.text=element_text(size=14)) +
  scale_fill_continuous(low="powderblue", high="darkorchid4", 
                        guide="colorbar",na.value="white",
                        name="",limits = c(0,300))

ggsave(paste0("C:/Users/ccard/Documents/git/ccardonaandrade.github.io/img/maps/precip","_",i,".png"), width = 6, height = 8, dpi = 900)
}


## list file names and read in
imgs <- list.files("C:/Users/ccard/Documents/git/ccardonaandrade.github.io/img/maps", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated


setwd("C:/Users/ccard/Documents/git/ccardonaandrade.github.io/img")

## save to disk
image_write(image = img_animated,
            path = "precipitation.gif")


map <- ORIGINAL  %>%
  ggplot(aes()) + geom_sf(data = ORIGINAL, aes(fill=ORIGINAL[[5]]),color = NA)  + 
  geom_sf(data = world, fill = "transparent") + theme_bw() +  
  coord_sf(xlim = c(-30, 60), ylim = c(-40, 45), expand = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        title =element_text(size=20, face='bold'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_text(size=14),
        legend.key = element_rect( fill = "gray92"),
        legend.position = c(0.2, 0.3),
        legend.key.size = unit(0.6, 'cm'),
        legend.text=element_text(size=14)) +
  scale_fill_continuous(low="powderblue", high="darkorchid4", 
                        guide="colorbar",na.value="white",
                        name="",limits = c(0,300))

anim <- map + 
  transition_states(ORIGINAL$year,transition_length = 1, state_length = 7, wrap = TRUE) + labs(title = "Precipitation (mm) - {closest_state}")

image <- animate(anim)

setwd("C:/Users/ccard/Documents/git/ccardonaandrade.github.io/img")
anim_save("africa_rain.gif")

