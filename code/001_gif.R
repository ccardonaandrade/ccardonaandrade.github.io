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
library(patchwork)
library(animation)

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

# For Delaware Data as well
ORIGINAL2<-ORIGINAL[,c(1:4,9,11)]

ORIGINAL2<-st_as_sf(left_join(ORIGINAL2,CELL))

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

# For Delaware
map <- ORIGINAL2  %>%
  ggplot(aes()) + geom_sf(data = ORIGINAL2, aes(fill=ORIGINAL2[[5]]),color = NA)  + labs(subtitle = "Climatic Research Unit") +
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

map2 <- ORIGINAL2  %>%
  ggplot(aes()) + geom_sf(data = ORIGINAL2, aes(fill=ORIGINAL2[[6]]),color = NA)  + labs(subtitle = "University of Delaware") +
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
                        name="",limits = c(0,250))

both<- map + map2
anim <- both + 
  transition_states(ORIGINAL2$year,transition_length = 1, state_length = 7, wrap = TRUE) +  plot_annotation(title = "Precipitation (mm) - {closest_state}",
                                                                                                            theme = theme(plot.title=element_text(hjust=0.5))) &  theme(text = element_text('mono'))

image <- animate(anim)

setwd("C:/Users/ccard/Documents/git/ccardonaandrade.github.io/img")
anim_save("africa_rain_comp.gif")


#### Lets use this code
# https://mikeyharper.uk/animated-plots-with-r/

invisible(saveGIF({
  for (i in 2000:2015){
    DATA<-filter(ORIGINAL2, year==i)

p1 <- DATA  %>%
  ggplot(aes()) + geom_sf(data =  DATA, aes(fill=DATA$pre_cru),color = NA)  + labs(subtitle ="Climatic Research Unit") +
  geom_sf(data = world, fill = "transparent") + theme_bw() +  
  coord_sf(xlim = c(-30, 60), ylim = c(-40, 45), expand = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        title =element_text(size=14),
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

p2 <- DATA  %>%
  ggplot(aes()) + geom_sf(data =  DATA, aes(fill=DATA$pre_del),color = NA)  + labs(subtitle ="University of Delaware") +
  geom_sf(data = world, fill = "transparent") + theme_bw() +  
  coord_sf(xlim = c(-30, 60), ylim = c(-40, 45), expand = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        title =element_text(size=14),
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
                        name="",limits = c(0,250))

model <- lm(pre_del ~ pre_cru, data = DATA)
rsq <- round(summary(model)$r.squared,2)

p3<-ggplot(DATA, aes(x = pre_cru, y = pre_del)) +
  stat_summary_bin(fun.y = 'mean', bins = 20, fill = 'gold', size = 3, shape=21,geom = 'point') +
  labs(title=paste0("Binscatter between datasets - R2: ",rsq) , subtitle = "University of Delaware", y = "", x = "Climatic Research Unit") +
  theme_classic(base_size = 12) +
  theme(panel.border = element_blank(), # removes border around plot
        axis.line.x = element_line(color = "black", size = 0.5), # sets x-axis line color and thickness
        axis.line.y = element_line(color = "black", size = 0.5), # sets y-axis line color and thickness
        axis.text.x = element_text(size = 10), # sets x-axis text size
        axis.text.y = element_text(size = 10), # sets y-axis text size
        axis.title = element_text(size = 12), # sets axis title size
        plot.title = element_text(hjust = 0.5, size = 16), # centers plot title and sets font size
        plot.subtitle = element_text( size = 12), # centers subtitle and sets font size
        legend.position = "bottom", # moves legend to bottom
        legend.text = element_text(size = 10), # sets legend text size
        legend.title = element_blank()) + # removes legend title
  scale_y_continuous(limits = c(0, 150), expand = c(0, 0)) + # sets y-axis range to 0-150 and removes space between axis and data
  scale_x_continuous(limits = c(0, 250), expand = c(0, 0)) +
  guides(fill = guide_legend(override.aes = list(size = 2))) # sets legend point size


print((p1 + p2) / p3 + plot_layout(widths = c(8, 8, 16), heights = c(1, 1))+ plot_annotation(title = paste0("Precipitation (mm) - ",i),
                              theme = theme(plot.title = element_text(size = 18, hjust=0.5, face='bold'))) & 
        theme(text = element_text('mono')))
  }
  
  
}, movie.name = "windDevelopment.gif", interval = 1, ani.width = 700, ani.height = 700))


