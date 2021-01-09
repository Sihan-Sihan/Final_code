library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(mapview)

# MAP

## Get the London LSOA Boundaries
LD_LSOA <- st_read(
  'D:/F_SCUA-UCL/CASA05/workshop_1/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp')
qtm(LD_LSOA)

## Calculate area
LD_LSOA_area <- st_area(LD_LSOA)
LD_LSOA_area <- LD_LSOA_area %>% as.numeric(as.character(LD_LSOA_area))

## Add colomn of area
LD_LSOA$area<- LD_LSOA_area
head(LD_LSOA)

## Remove useless data 
LD_LSOA <- LD_LSOA %>%
  select('LSOA11CD', 'LSOA11NM','POPDEN', 'geometry', 'area')



# 1. CRIME DATA

## Read csv of crime
Crime_MtroP <- read.csv('D:/F_SCUA-UCL/Final/GIS_Final/data/2019-09/2019-09-metropolitan-street.csv')
Crime_CityofLD <- read.csv('D:/F_SCUA-UCL/Final/GIS_Final/data/2019-09/2019-09-city-of-london-street.csv')
head(Crime_CityofLD)

## Rbind two dataset into the greater London dataset
Crime_LD <- rbind(Crime_MtroP, Crime_CityofLD)

## Data cleaning
Crime_LD_Cleaning <- select(Crime_LD, 8:10)

## London Crime - total
LSOA_Crime <-  Crime_LD_Cleaning %>%
  group_by(LSOA.code) %>%
  summarise(n=n())

LSOA_Crime <-left_join(LD_LSOA, LSOA_Crime, by=c('LSOA11CD'= 'LSOA.code'))

LSOA_Crime$denpop <- LSOA_Crime$n / LSOA_Crime$POPDEN * LSOA_Crime$area

## plot
tmap_mode('plot')  
qtm(LSOA_Crime_cleaning, 
    fill = 'denpop')

## plot
tm_shape(LSOA_Crime_cleaning)+
  ## boundries
  tm_borders(col = "grey",alpha = 0.1, lwd = 0.01)+
  ## fill color
  tm_fill(col = 'denpop',n = 5,style = 'quantile', palette = "YlGnBu",colorNA = "gray",
          legend.show = TRUE,legend.hist = FALSE,
          title = 'Crime_index')+
  tm_compass(size = 2.5, text.size = 0.7, type = "arrow", position=c("right", "bottom"))+ #compass
  tm_scale_bar(width = 0.15, position=c("right", "bottom"), text.size = 0.5)   ## bar
# tm_xlab("Longitude") + tm_ylab("Latitude")  # coordinate

## save plot
tmap_save(filename =  'Crime_index.png')





# 2. POI DATA

## Read data
poi <- st_read('D:/F_SCUA-UCL/Final/GIS_Final/data/POI/POI.shp')
# Data Copyright Note
# Ordnance Survey Digimap: Licensed Data: © Crown copyright and database rights 2020 Ordnance Survey (100025252). 
# This material includes data licensed from PointX© Database Right/Copyright 2020.

## Check POI data
sample_n(poi, 10)

## Remove useless data 
poi_LD <- poi %>%
  select('ref_no', 'name','groupname', 'geographic', 'categoryna', 'classname', 'geometry')

## 2.1 Point heat map
poi_LD <- poi_LD %>%
  st_transform(4326) %>% # tranform to same CRS as stations and lines
  cbind(st_coordinates(.))
### Add boundaries of borough (LSOA is too small)
Londonborough <- st_read(
  'D:/F_SCUA-UCL/CASA05/workshop_1/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp')
### plot
ggplot() +
  geom_bin2d(data = poi_LD, aes(X, Y), binwidth = c(0.01, 0.01)) + # heatmap of 2d bin counts
  geom_sf(data = Londonborough, col="white", size=0.001, alpha = 0.01) +
  theme_tufte() +
  scale_fill_viridis_c(option = "plasma") +
  labs(x="", y="")

### save plot
ggsave('poi_heatmap.png', plot = last_plot())


## 2.2 Point density visualisation

### Join LOSA
poi_LD <- poi_LD %>%
  filter(geographic == 'Greater London')
poi_LD <- poi_LD %>%
  st_join(., LD_LSOA)

### Sort out variables
poi_LD %>%
  st_drop_geometry() %>%
  select(groupname, categoryna, classname) %>%
  mutate(n_groupname = n_distinct(groupname),
         n_categoryname = n_distinct(categoryna),
         n_classname = n_distinct(classname)) %>%
  select(n_groupname, n_categoryname, n_classname) %>%
  unique()

### View how many categories in groupname
levels(as.factor(poi_LD$groupname))

### how many observations for 'groupname'
poi_LD %>%
  st_drop_geometry() %>%
  count(groupname, sort = TRUE)

poi_LD %>%
  st_drop_geometry() %>% 
  select(groupname, categoryna) %>% 
  unique() %>%
  count(groupname)

# to 4326
LD_LSOA <-  LD_LSOA %>%
  st_transform(4326)
LD_LSOA

# count points of each LOSA
poi_count <- poi_LD %>%
  group_by(LSOA11CD) %>%
  summarise(n=n())
poi_count <- poi_count %>% 
  st_drop_geometry()

# add the numbers of points to LOSA (then can calculate density and plot)
LSOA_POI_account <- left_join(LD_LSOA, poi_count, by=c('LSOA11CD'= 'LSOA11CD'))

LSOA_POI_account$density <- LSOA_POI_account$n / LSOA_POI_account$area

tmap_mode('plot')  

## plot
tm_shape(LSOA_POI_account)+
  ## boundries
  tm_borders(col = "white",alpha = 0.01, lwd = 0.01)+
  ## fill color
  tm_fill(col = 'density',n = 5,style = 'quantile', palette = "YlGnBu",colorNA = "gray",
          legend.show = TRUE,legend.hist = FALSE,
          title = 'LSOA_POI_account')+
  tm_compass(size = 2.5, text.size = 0.7, type = "arrow", position=c("left", "bottom"))+ #compass
  tm_scale_bar(width = 0.15, position=c("left", "bottom"), text.size = 0.5)+   ## bar
  # tm_xlab("Longitude") + tm_ylab("Latitude")  # coordinate
  tm_layout(title = "b.Density of Points of Interest (per m2)", 
            main.title = "", title.size = 0.77) +
  tm_credits(size = 3, 
             text = "Ordnance Survey Digimap: Licensed Data: © Crown copyright and database rights 2020 Ordnance Survey (100025252). This material includes data licensed from PointX© Database Right/Copyright 2020", 
             position = c("right", "BOTTOM"))

## save plot
tmap_save(filename =  'poi_density.png')


## 2.3 Diversity

# Create summary of obs. by station #############################################
pps <- poi_LD %>%
  st_drop_geometry() %>%
  count(id) %>%
  rename(station_count = n)

# Create a 'Richness' metric by 'Group', "Category' and 'Class'
div_tab <- poi_LD %>%
  st_drop_geometry() %>%
  group_by(LSOA11CD) %>%
  summarise(rich_g = n_distinct(groupname),
            rich_cat = n_distinct(categoryna),
            rich_cla = n_distinct(classname))

# Merge
diversity <- LSOA_POI_account %>%
  merge(., div_tab)

diversity$rich_cla_den <- diversity$rich_cla / diversity$area

# PLOT
tmap_mode('plot')  
## plot
tm_shape(diversity)+
  ## boundries
  tm_borders(col = "white",alpha = 0.01, lwd = 0.01)+
  ## fill color
  tm_fill(col = 'rich_cla_den',n = 5,style = 'quantile', palette = "YlGnBu",colorNA = "gray",
          legend.show = TRUE,legend.hist = FALSE,
          title = 'POI_Diversity')+
  tm_compass(size = 2.5, text.size = 0.7, type = "arrow", position=c("left", "bottom"))+ #compass
  tm_scale_bar(width = 0.15, position=c("left", "bottom"), text.size = 0.5)+   ## bar
  # tm_xlab("Longitude") + tm_ylab("Latitude")  # coordinate
  tm_layout(title = "c.Diversity of Points of Interest(per m2)", 
            main.title = "", title.size = 0.77) +
  tm_credits(size = 3, 
             text = "Ordnance Survey Digimap: Licensed Data: © Crown copyright and database rights 2020 Ordnance Survey (100025252). This material includes data licensed from PointX© Database Right/Copyright 2020", 
             position = c("right", "BOTTOM"))

## save plot
tmap_save(filename =  'poi_diversity.png')



# 3. TRAFFIC FLOW

############street density##################

# Read data
LD_road <- st_read('D:/F_SCUA-UCL/Final/GIS_Final/data/street/gis_osm_roads_free_1.shp')
qtm(LD_road)

# Intersect the road and boundaries
road_intersect <- LD_road %>%
  st_join(., LD_LSOA)

# Calculate the length of road in each region
length <- st_length(road_intersect)
length <- as.numeric(length)
road_intersect$length <- length


# Calculate the account length of each region
road_intersect <- road_intersect %>%
  group_by(LSOA11CD, ) %>%
  summarise(.,sum(length))

# Remove NA
road_intersect <- road_intersect%>%
  filter(LSOA11CD != '')%>%
  st_drop_geometry()

# Add the numbers of points to LOSA (then can calculate density and plot)
LSOA_road_account <- left_join(LD_LSOA, road_intersect, by=c('LSOA11CD'= 'LSOA11CD'))

LSOA_road_account$density <- LSOA_road_account$`sum(length)` / LSOA_road_account$area

# PLOT
tmap_mode('plot')  
qtm(LSOA_road_account, 
    fill = 'density')
## plot
tm_shape(LSOA_road_account)+
  ## boundries
  tm_borders(col = "white",alpha = 0.01, lwd = 0.01)+
  ## fill color
  tm_fill(col = 'density',n = 5,style = 'quantile', palette = "YlGnBu",colorNA = "gray",
          legend.show = TRUE,legend.hist = FALSE,
          title = 'Road Density')+
  tm_compass(size = 2.5, text.size = 0.7, type = "arrow", position=c("left", "bottom"))+ #compass
  tm_scale_bar(width = 0.15, position=c("left", "bottom"), text.size = 0.5)+   ## bar
  # tm_xlab("Longitude") + tm_ylab("Latitude")  # coordinate
  tm_layout(title = "a.Density of Road Network(per m2)", 
            main.title = "", title.size = 0.77)

## save plot
tmap_save(filename =  'a.Density of Road Network.png')



# 4. popden
tmap_mode('plot')  
qtm(LD_LSOA, 
    fill = 'density')
## plot
tm_shape(LSOA_road_account)+
  ## boundries
  tm_borders(col = "white",alpha = 0.01, lwd = 0.01)+
  ## fill color
  tm_fill(col = 'POPDEN',n = 5,style = 'quantile', palette = "YlGnBu",colorNA = "gray",
          legend.show = TRUE,legend.hist = FALSE,
          title = 'Road Density')+
  tm_compass(size = 2.5, text.size = 0.7, type = "arrow", position=c("left", "bottom"))+ #compass
  tm_scale_bar(width = 0.15, position=c("left", "bottom"), text.size = 0.5)+   ## bar
  # tm_xlab("Longitude") + tm_ylab("Latitude")  # coordinate
  tm_layout(title = "d. Population Density", 
            main.title = "", title.size = 0.77) 

## save plot
tmap_save(filename =  'popden.png')






#5. DATA CLEANING

###remove outliers by z-score
LSOA_Crime$Zscore <- scale(LSOA_Crime$denpop,center = TRUE, scale = TRUE)

LSOA_Crime_cleaning <- LSOA_Crime %>%
  filter(Zscore < 3) %>%
  filter(Zscore > -3)

### view the data (whether it is normalize)
boxplot(LSOA_Crime_cleaning$denpop, main="Crime")
histplot <- ggplot(LSOA_Crime, aes(x=denpop))
histplot +geom_histogram()

###make it normalize by log
LSOA_Crime_cleaning$denpoplog <- log(LSOA_Crime_cleaning$denpop)

boxplot(LSOA_Crime_cleaning$denpoplog, main="Crime")
histplot <- ggplot(LSOA_Crime_cleaning, aes(x=denpoplog))
histplot +geom_histogram()

###remove outliers by z-score again
LSOA_Crime_cleaning$Zscorelog <- scale(LSOA_Crime_cleaning$denpoplog,center = TRUE, scale = TRUE)

LSOA_Crime_cleaning <- LSOA_Crime_cleaning %>%
  filter(Zscorelog < 3) %>%
  filter(Zscorelog > -3)

## plot on map
tmap_mode('plot')  
qtm(LSOA_Crime_cleaning, 
    fill = 'denpoplog')

## plot
tm_shape(LSOA_Crime_cleaning)+
  ## boundries
  tm_borders(col = "grey",alpha = 0.1, lwd = 0.01)+
  ## fill color
  tm_fill(col = 'denpoplog',n = 5,style = 'quantile', palette = "YlGnBu",colorNA = "gray",
          legend.show = TRUE,legend.hist = FALSE,
          title = 'Crime')+
  tm_compass(size = 2.5, text.size = 0.7, type = "arrow", position=c("right", "bottom"))+ #compass
  tm_scale_bar(width = 0.15, position=c("right", "bottom"), text.size = 0.5)   ## bar
# tm_xlab("Longitude") + tm_ylab("Latitude")  # coordinate

## save plot
tmap_save(filename =  'Crime_indexourfliter2.png')




# 6. Review data distribution 

# Create a new data frame just containing the three variables we are interested in
# Select POI_density
Independentdata <- select(LSOA_POI_account, LSOA11CD, density, POPDEN) %>%
  rename(.,POI_density = density)
# Select POI_diversity
POI_diversity <- select(diversity,LSOA11CD,rich_cla_den) %>%
  st_drop_geometry()
Independentdata <- left_join(Independentdata, POI_diversity, by=c('LSOA11CD'= 'LSOA11CD'))
Independentdata <- rename(Independentdata,POI_diversity = rich_cla_den)
# Select Road_density
Road_density <- select(LSOA_road_account,LSOA11CD,density) %>%
  st_drop_geometry()
Independentdata <- left_join(Independentdata, Road_density, by=c('LSOA11CD'= 'LSOA11CD'))
Independentdata <-rename(Independentdata, Road_density = density)
# drop geo inf
Independentdata <- Independentdata %>%
  st_drop_geometry()

#log
Independentdata$log_POI_density <- log(Independentdata$POI_density)
Independentdata$log_POI_diversity <- log(Independentdata$POI_diversity)
Independentdata$log_Road_density <- log(Independentdata$Road_density)
Independentdata$log_popden <- log(Independentdata$POPDEN)

Independentdata <- Independentdata %>%
  select(.,-POPDEN)

Variables <- left_join(LSOA_Crime_cleaning, Independentdata, by = c('LSOA11CD'= 'LSOA11CD')) 
Variables <- Variables %>%
  rename(crime =denpop ,log_crime = denpoplog)


#– check variable distributions first

#save plot

jpeg(file="saving_plot1.jpeg")

par(mfrow=c(2, 5))  # divide graph area in 2 columns
boxplot(LSOA_Crime_cleaning$denpop, main="Crime",col = 'orange')
boxplot(Independentdata$POI_density, main="POI_density", col = '#b0c7f0')
boxplot(Independentdata$POI_diversity, main="POI_diversity",col = '#b0c7f0')
boxplot(Independentdata$Road_density, main="Road_density", col = '#b0c7f0')
boxplot(Independentdata$Road_density, main="Poplation_density", col = '#b0c7f0')

boxplot(Variables$log_crime, main="Variables", col = 'orange')
boxplot(Variables$log_POI_density, main="log_POI_density",col = '#b0c7f0')
boxplot(Variables$log_POI_diversity, main="log_POI_diversity", col = '#b0c7f0')
boxplot(Variables$log_Road_density, main="log_Road_density", col = '#b0c7f0')
boxplot(Variables$log_popden, main="log_Poplation_density",col = '#b0c7f0')

dev.off()

crime_Dist <- Variables %>%
  ggplot(aes(x=log_crime)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="orange", col="black")+
  geom_vline(aes(xintercept=mean(log_crime)),
             color="darkblue",
             linetype="dashed")+
  labs(title="Crime",
       x="log_Crime",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(crime_Dist,filename = "crime_Dist.png",width = 12,height = 9)

POIden_Dist <- Variables %>%
  ggplot(aes(x=log_POI_density)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="#b0c7f0", col="black")+
  geom_vline(aes(xintercept=mean(log_POI_density)),
             color="darkblue",
             linetype="dashed")+
  labs(title="POI_Density",
       x="log_POI_Density",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(POIden_Dist,filename = "POIden_Dist.png",width = 12,height = 9)

POIdiv_Dist <- Variables %>%
  ggplot(aes(x=log_POI_diversity)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="#b0c7f0", col="black")+
  geom_vline(aes(xintercept=mean(log_POI_diversity)),
             color="darkblue",
             linetype="dashed")+
  labs(title="POI_Diversity",
       x="log_POI_Diversity",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(POIdiv_Dist,filename = "POIdiv_Dist.png",width = 12,height = 9)

Roadden_Dist <- Variables %>%
  ggplot(aes(x=log_Road_density)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="#b0c7f0", col="black")+
  geom_vline(aes(xintercept=mean(log_Road_density)),
             color="darkblue",
             linetype="dashed")+
  labs(title="Road_density",
       x="log_Road_density",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(Roadden_Dist,filename = "Roadden_Dist.png",width = 12,height = 9)

popden_Dist <- Variables %>%
  ggplot(aes(x=log_popden)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="#b0c7f0", col="black")+
  geom_vline(aes(xintercept=mean(log_popden)),
             color="darkblue",
             linetype="dashed")+
  labs(title="Poplation_density",
       x="log_Poplation_density",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 5))

ggsave(popden_Dist,filename = "popden_Dist.png",width = 12,height = 9)

# 7. urban vitality calculation

minPd <- min(Variables$log_POI_density)
Pd <- max(Variables$log_POI_density) - min(Variables$log_POI_density)
pd_norm <-  Variables$log_POI_density - minPd
Variables$pd_norm <- pd_norm / Pd

minPdiv <- min(Variables$log_POI_diversity)
Pdiv <- max(Variables$log_POI_diversity) - min(Variables$log_POI_diversity)
pdiv_norm <-  Variables$log_POI_diversity - minPdiv
Variables$pdiv_norm <- pdiv_norm / Pdiv

minR <- min(Variables$log_Road_density)
R <- max(Variables$log_Road_density) - min(Variables$log_Road_density)
r_norm <- Variables$log_Road_density - minR
Variables$r_norm <- r_norm / R

minPp <- min(Variables$log_popden)
Pp <- max(Variables$log_popden) - min(Variables$log_popden)
Pp_norm <- Variables$log_popden - minPp
Variables$Pp_norm <- Pp_norm / Pp

Variables$vitality <- Variables$pd_norm + Variables$pdiv_norm + Variables$r_norm + Variables$Pp_norm
Variables$vitality <- Variables$vitality / 4 * 100

## plot on map
tmap_mode('plot')  
qtm(Variables, 
    fill = 'vitality')

tm_shape(Variables)+
  ## boundries
  tm_borders(col = "grey",alpha = 0.1, lwd = 0.01)+
  ## fill color
  tm_fill(col = 'vitality',n = 5,style = 'quantile', palette = "YlGnBu",colorNA = "gray",
          legend.show = TRUE,legend.hist = FALSE,
          title = 'Urban Vitality')+
  tm_compass(size = 2.5, text.size = 0.7, type = "arrow", position=c("right", "bottom"))+ #compass
  tm_scale_bar(width = 0.15, position=c("right", "bottom"), text.size = 0.5)   ## bar

## save plot
tmap_save(filename =  'vitality.png')

Vitality_Dist <- Variables %>%
  ggplot(aes(x=vitality)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="#b0c7f0", col="black")+
  geom_vline(aes(xintercept=mean(vitality)),
             color="darkblue",
             linetype="dashed")+
  labs(title="Vitality",
       x="log_vitality",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(Vitality_Dist,filename = "Vitality_Dist.png",width = 12,height = 9)


# 7. correlation analysis


corre_variable <- Variables %>%
  select(., log_crime, vitality, log_POI_density,log_POI_diversity,log_Road_density) %>%
  st_drop_geometry()

library(corrplot)
cormat <- cor(corre_variable, use="complete.obs", method="pearson")
corrplot(cormat)

library(Hmisc)#加载包
res2 <- corre_variable 

res2 <- dplyr::mutate_all(res2,as.integer)
res <- rcorr(as.matrix(res2))
res

#####



# 8. Moran'sI
#calculate the centroids of all Wards in London
coordsLSOA <- Variables %>%
  st_centroid()%>%
  st_geometry()

plot(coordsLSOA)

#Now we need to generate a spatial weights matrix 
#(remember from the lecture a couple of weeks ago). We'll start with a simple binary matrix of queen's case neighbours
library(spdep)

#or nearest neighbours
knn_LSOA <-coordsLSOA %>%
  knearneigh(., k=4)

LSOA_knn <- knn_LSOA %>%
  knn2nb()

LSOA.knn_4_weight <- LSOA_knn %>%
  nb2listw(., style="C")

Nearest_neighbour <- Variables %>%
  st_drop_geometry()%>%
  dplyr::select(.resid)%>%
  pull()%>%
  moran.test(., LSOA.knn_4_weight)%>%
  tidy()

Nearest_neighbour

# crime moranI
crimemoran <-  corre_variable %>%
  dplyr::select(log_crime)%>%
  pull()%>%
  moran.test(., LSOA.knn_4_weight)%>%
  tidy()

crimemoran






# 9. run a spatially-lagged regression model
slag_dv_model_knn4 <- lagsarlm(log_crime ~ vitality, 
                               data = corre_variable, 
                               nb2listw(LSOA_knn, 
                                        style="C"), 
                               method = "eigen")



#what do the outputs show?
tidy(slag_dv_model_knn4)

#glance() gives model stats but this need something produced from a linear model
#here we have used lagsarlm()
library(broom)
glance(slag_dv_model_knn4)

#write out the residuals

modelresiduals <- corre_variable %>%
  mutate(slag_dv_model_knn_resids = residuals(slag_dv_model_knn4))

KNN4Moran <- modelresiduals %>%
  dplyr::select(slag_dv_model_knn_resids)%>%
  pull()%>%
  moran.test(., LSOA.knn_4_weight)%>%
  tidy()

KNN4Moran




# 10. cluster
Cluster_data <- corre_variable %>%
  select(log_crime, vitality)

Cluster_data <- dplyr::mutate_all(Cluster_data,as.integer)

fit <- Cluster_data %>%
  kmeans(., 3, nstart=75)

# get cluster means

library(tidymodels)

centroid <- tidy(fit)%>%
  #print the results of the cluster groupings
  print()%>%
  dplyr::select(log_crime, vitality)

# as we only have variable two dimensions we can plot the clusters on a graph
p <- ggplot(Cluster_data,aes(log_crime, vitality))+
  geom_point(aes(colour=factor(fit$cluster)))+
  geom_point(data=centroid,aes(denpoplog, vitality), size=7, shape=18)+ theme(legend.position="none")

LD_LSOA_dep <- fit %>% 
  # 
  augment(., LSOA_Crime_cleaning)%>%
  dplyr::select(LSOA11CD, .cluster)%>%
  #make sure the .cluster column is numeric
  mutate(across(.cluster, as.numeric))%>%
  # join the .cluster to our sf layer
  left_join(LSOA_Crime_cleaning, 
            .,
            by = c("LSOA11CD" = "LSOA11CD"))

tmap_mode('plot')  
qtm(LD_LSOA_dep,
    fill = '.cluster')
## plot
tm_shape(LD_LSOA_dep)+
  ## boundries
  tm_borders(col = "white",alpha = 0.01, lwd = 0.01)+
  ## fill color
  tm_fill(col = '.cluster',n = 5,breaks=c(1,2,3,4,5,6), palette = "YlGnBu",colorNA = "gray",
          legend.show = TRUE,legend.hist = FALSE,
          title = 'Cluster')+
  tm_compass(size = 2.5, text.size = 0.7, type = "arrow", position=c("right", "bottom"))+ #compass
  tm_scale_bar(width = 0.15, position=c("right", "bottom"), text.size = 0.5)+   ## bar
  # tm_xlab("Longitude") + tm_ylab("Latitude")  # coordinate
  tm_layout(title = "Cluster", 
            main.title = "", title.size = 0.77) 
## save plot
tmap_save(filename =  'Cluster.png')



## make the scatter
Variables$cluster <- LD_LSOA_dep$.cluster
cluster_scatter2 <- ggplot(Variables, aes(x=vitality,y=log_crime,color = cluster))+
  geom_point(aes(colour = cluster))
cluster_scatter
ggsave(cluster_scatter, filename = 'cluster_scatter.png')


cluster1 <- Variables %>%
  filter(.,cluster ==1)%>%
  select(.,log_crime,vitality.y)%>%
  st_drop_geometry()%>%
  summary()
cluster2 <- Variables %>%
  filter(.,cluster ==2)%>%
  select(.,log_crime,vitality.y)%>%
  st_drop_geometry()%>%
  summary()
cluster3 <- Variables %>%
  filter(.,cluster ==3)%>%
  select(.,log_crime,vitality.y)%>%
  st_drop_geometry()%>%
  summary()


cluster1
cluster2
cluster3

