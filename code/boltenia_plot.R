#Load libraries
library(raster)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(stars)
library(viridis)
library(rnaturalearth)

sf_use_s2 = FALSE #this is an sf line of code I use that helps with basic plotting (turns off the spherical geometry part of some of the functions)

#projections --------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0" #there is not coordinate reference system embedded in the geotiff so I assume it is close to this. 

#read in the raster
bolt_ras <- raster("data/Boltenia_full model.tif") #note you will have to paste this into the data folder since it is far too large to track with github

bolt_extent <- extent(bolt_ras)%>%
               as(., 'SpatialPolygons')%>%
               st_as_sf()%>%
               st_set_crs(latlong)

#read in a shape file for the coastline at at a reasonable resolution 
coastline <- read_sf("data/Land_AtlCanada_ESeaboardUS.shp")%>%
            st_transform(latlong)%>%
            st_intersection(.,bolt_extent)

#bolt stars 
bolt_stars <- st_as_stars(bolt_ras)%>%
               st_set_crs(latlong) 

bolt_plot_extent <- st_bbox(bolt_stars)

#note that stars is a pig on ram and with the resolution of your raster, it actually crashes (on my computer anyway)
# ggplot()+
#   geom_stars(data=bolt_stars)+
#   geom_sf(data=coastline)+
#   coord_sf(expand=0)+
#   theme_bw()+
#   annotation_scale()


#here is a plot using base r - it's not perfect by any stretch but it at least shows you how to add land and how to manipulate how the raster values
#to make the plot more interpretable, which TBH is the hardest thing to do when you have such a highly screwed predicted distribution that is generally sparse (meaning very high values aren't clustered together). 

#there are alot of very low values so to make the range more interpetable we will 'clamp' the values between 0.01 and 600 meaning anything above or below those values is assigned to the range limits
r <- clamp(bolt_ras, 0.001, 600)

#Because of the very large range and the fact that there are so few large values we can log10 transform the plot to again try to make it more interpretable
values(r) <- log10(values(r))

#Here we set some plotting conditions again with the aim to make the raster easier to visualize
rasbreaks <- c(0.001,0.01,0.1,1,5,10,25,50,100,150,250,500,600)
raslabs <- c("0","0.01","0.1","1","5","10","25","50","100","150","250","500","600+")
rasbreaks_log <- c(-3,log10(rasbreaks[2:length(rasbreaks)])) #-3 here is because the values are 'clamped' to 0.001 (or 10^-3 in log10 space). 

#make the plot and save it
png('output/boltenia_logged.png')
    plot(r,breaks=rasbreaks_log,lab.breaks = raslabs,col=inferno(length(rasbreaks)),asp = "", xpd = NA)
    plot(coastline%>%as_Spatial(),add=T,col="grey50",asp = "", xpd = NA)
dev.off()



