######################################
## ENEC 395: Avian community similarity: script 2
# Ellie Kremer
# 12/3/2019
######################################

# Library
library(raster)
library(dplyr) 
library(sf)
library(lwgeom)
library(rgdal)
library(ggplot2)
library(spData)
library(tmap)

## Read in Breeding Bird Census (BBC) data
# https://github.com/weecology/bbc-data-rescue

bbc_censuses = read.csv("bbc-data/bbc_censuses.csv", header = TRUE, sep = ",")
bbc_counts = read.csv("bbc-data/bbc_counts.csv", header = TRUE, sep = ",")
bbc_sites = read.csv("bbc-data/bbc_sites.csv", header = TRUE, sep = ",")

bbsWeather = read.csv("bbs_weather.csv")
bbsRoutes = read.csv("bbs_routes.csv") %>% mutate(stateroute = (statenum * 1000) + route)

# Species name conversion - Code via Di Cecco
# Match species common names to BBS species list

fix_spp <- list(new_species = c("Sage Sparrow" = "Sagebrush Sparrow", 
                                "Western Scrub Jay" = "California Scrub Jay", 
                                "Sharp-tailed Sparrow" = "Nelson's Sparrow", 
                                "Common Crackle" = "Common Grackle", 
                                "Three-toed Woodpecker" = "American Three-toed Woodpecker", 
                                "Yellow-rumped Warbler" = "(unid. Myrtle/Audubon's) Yellow-rumped Warbler", 
                                "Rock Dove" = "Rock Pigeon", 
                                "Northern Oriole" = "Baltimore Oriole", 
                                "Plain Titmouse" = "unid. Oak Titmouse / Juniper Titmouse", 
                                "Scrub Jay" = "California Scrub Jay", 
                                "Northern Flicker" = "(unid. Red/Yellow Shafted) Northern Flicker", 
                                "Western Flycatcher" = "unid. Cordilleran / Pacific-slope Flycatcher", 
                                "Solitary Vireo" = "unid. Cassin's Vireo / Blue-headed Vireo", 
                                "Dark-eyed Junco" = "(unid. race) Dark-eyed Junco", 
                                "Brown Towhee" = "California Towhee", 
                                "Rufous-sided Towhee" = "unid. Spotted Towhee / Eastern Towhee"))

new_spp_names <- as.data.frame(fix_spp)
new_spp_names$common_names <- row.names(new_spp_names)

# Below bbc$species is referring to whatever you've named bbc_counts.csv

new_species <- c()
for(spp in bbc_counts$species) {
  if (spp %in% new_spp_names$common_names) {
    new_species <- c(new_species, as.character(new_spp_names[new_spp_names$common_names == spp, 1]))
  } else {
    new_species <- c(new_species, spp)
  }
}

## Filter to BBC surveys at the same site >= 2 census years, >= 10 years apart

# Step 1: Calculate total number of surveys performed at each site and maximum time between two surveys

bbcCountTemp <- data.frame(siteID_uni = unique(bbc_censuses$siteID), 
                             count = NA, time_range = NA)

for (s in 1: length(bbcCountTemp$siteID_uni)) {
  tmpCount = filter(bbc_censuses, siteID == bbcCountTemp$siteID_uni[s])
  tmpTime = max(tmpCount$year) - min(tmpCount$year)
  
  bbcCountTemp$count[s] = nrow(tmpCount)
  bbcCountTemp$time_range[s] = tmpTime
  }

bbcCountTemp = bbcCountTemp %>% filter(count>= 2, time_range>=10) 
bbcCensusFin = bbc_censuses %>%
  filter(siteID %in% bbcCountTemp$siteID_uni)

# Step 2: Filter to BBC sites that meet criteria

bbcSitesTemp = bbc_sites %>%
  dplyr::select(siteID:longitude) %>%
  filter(siteID %in% bbcCensusFin$siteID) %>%
  distinct() 
  
# Adding state identifiers equivalent to BBS statenum classification
state_conver = read.csv("state_convers.csv", header = TRUE, sep = ",")
bbc_states = c("Connecticut", "Connecticut", "New York", "California", "California",
               "Connecticut", "District of Columbia", "Connecticut", "South Carolina",
               "Connecticut", "Ontario","California", "New York", "Ontario",
               "South Carolina","Tennessee", "Ontario", "California")
bbc_statenum = c("18", "18", "61", "14", "14", "18","22", "18", 
                 "80","18", "68","14", "61", "68", "80", "82", "68", "14")

# Categorizing BBC landcover types based on site name
bbcSiteFinNLCD = c("forest", "forest", "forest, agriculture", "shrubland", "shrubland",
                   "shrubland", "forest", "forest", "forest", "forest", "grassland",
                   "wetland", "forest", "grassland", "forest", "forest", "grassland", "shrubland")

bbcSitesFin = bbcSitesTemp %>%
  mutate(state = bbc_states) %>%
  mutate(statenum = bbc_statenum) %>%
  mutate(landcover = bbcSiteFinNLCD) %>%
  # removing siteid 247 because no comparable elevation found in BBC sites
  filter(siteID != 247) 

for (s in 1:nrow(bbcSitesFin)) {
  bbcCensusTemp= filter(bbcCensusFin, siteID == bbcSitesFin$siteID[s])
  bbcSitesFin$y1[s] = min(bbcCensusTemp$year)
  bbcSitesFin$y2[s] = max(bbcCensusTemp$year)
}
# Change to negative longitude values
bbcSitesFin$longitude = -(bbcSitesFin$longitude)

####

## Assigning coordinates for Point Reyes station sites based on coordinates of the Palomarin Field Station
## available at https://github.com/weecology/bbc-data-rescue/blob/master/BBC_pdfs/BBC1973.pdf
## coordinates based on Google Maps 
for (n in 1:nrow(bbcSitesFin)) {
  if(bbcSitesFin$state[n] == "California" & bbcSitesFin$landcover[n] == "shrubland") {
    bbcSitesFin$latitude[n] = 37.92993
    bbcSitesFin$longitude[n] = -122.73526
  }
}

## Read in elevation data and assign to each survey site for BBC
# https://www.sciencebase.gov/catalog/item/4fb5495ee4b04cb937751d6d

# Step 1: Read in raw data

elev <- raster("Elevation_GRID/NA_Elevation/data/NA_Elevation/na_elevation")
proj4string(elev) = CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

# Step 2: Transform coordinates to match the map projection used for elevation data
latlong = data.frame(long = bbcSitesFin$longitude, lat = bbcSitesFin$latitude)
sp::coordinates(latlong) = c("long", "lat")
proj4string(latlong) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
latlong2 = spTransform(latlong, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))

# Step 3: Extract and save elevation values for each BBC site
bbcElev = extract(elev, latlong2) 
bbcSitesFin2 = mutate(bbcSitesFin, elev_m = bbcElev)

####
## Filtering BBS sites based on proximity to BBC sites and elevation

# Step 1: Create spatial data frame of BBS sites and assign projection
sf_bbsRoutes = st_as_sf(bbsRoutes, 
                   coords = c("longitude", "latitude"),
                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sf_bbsRoutes = mutate(sf_bbsRoutes, longitude = bbsRoutes$longitude, latitude = bbsRoutes$latitude)
sf_bbsRoutes$stateroute = (sf_bbsRoutes$statenum * 1000)+ sf_bbsRoutes$route
sf_bbcSites = st_as_sf(bbcSitesFin2,
                       coords = c("longitude", "latitude"),
                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Step 2: Create area specific BBS Datasets to streamline processing
{
cali_bbs = st_as_sf(filter(sf_bbsRoutes, countrynum == "840" & 
                             statenum == "14"))
tenn_bbs = st_as_sf(filter(sf_bbsRoutes, countrynum == "840" & statenum == "82"|statenum== "63"|
                    statenum=="80"| statenum =="27"))
sc_bbs = st_as_sf(filter(sf_bbsRoutes, countrynum == "840" & 
                           statenum == "80" | statenum == "27"))
dc_bbs = st_as_sf(filter(sf_bbsRoutes, countrynum == "840" & statenum == "22" | statenum == "46" |
                  statenum == "59"|statenum == "72"))
conn_bbs = st_as_sf(filter(sf_bbsRoutes, countrynum == "840" & statenum == "18"| statenum == "77"|
                    statenum == "47"|statenum == "61"))
ny_bbs = st_as_sf(filter(sf_bbsRoutes, statenum == "61"|statenum == "72" |
                  statenum == "59"|statenum == "18"|statenum == "47"|
                  statenum == "87"|statenum == "68"))
ont_bbs = st_as_sf(filter(sf_bbsRoutes, statenum == "68" | 
                            statenum == "61"|statenum == "49"))
}

# Step 3: Calculate distances from BBC to BBS site and filter to BBS sites within 100000m of each BBC site

dist_list = list()
for (n in 1: nrow(sf_bbcSites)) {
  state = sf_bbcSites$statenum[n]
  if (state == "18") {df = conn_bbs}
  if (state == "61") {df = ny_bbs}
  if (state == "14") {df = cali_bbs}
  if (state == "22") {df = dc_bbs}
  if (state == "80") {df = sc_bbs}
  if (state == "68") {df = ont_bbs}
  if (state == "82") {df = tenn_bbs}
  
  site = df
  for (s in 1:nrow(df)) {
   site$dist[s] = st_distance(sf_bbcSites$geometry[n], 
                               df$geometry[s], "Great Circle")
   site$siteid[s] = sf_bbcSites$siteID[n]
  }
  
  site1 = site %>% filter(dist < 100000)
  
  site1$y1 = sf_bbcSites$y1[n]
  site1$y2 = sf_bbcSites$y2[n]
  
  dist_list[[n]] = site1
}

# Step 4: Calculate elevation of each BBS route then filter to +/- 100m of each BBC site
dist_list_elev = dist_list
for(n in 1: length(dist_list)) {
  latlong = st_coordinates(dist_list[[n]])
  latlong2 = SpatialPoints(latlong, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  latlong3 = spTransform(latlong2, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
  bbsElev = extract(elev, latlong3)
  dist_list[[n]] = mutate(dist_list[[n]], elev_m = bbsElev)
}

for (n in 1:length(dist_list)) {
  dist_list_elev[[n]] = filter(dist_list[[n]], (elev_m <= sf_bbcSites$elev_m[n] + 100) &
                                 (elev_m >= sf_bbcSites$elev_m[n] - 100))
}

## Filter BBS candidates based on whether they were surveyed within a 5 year window of year 1 and year 2 of BBC surveys

bbsRepeated_list = list()
bbsRoutes_fin = list()
for (n in 1: length(dist_list_elev)) {
  df = bbsWeather %>%
    filter(stateroute %in% dist_list_elev[[n]]$stateroute) %>%
    mutate(early.app = (year >= dist_list_elev[[n]]$y1[1]-2 & year <= dist_list_elev[[n]]$y1[1]+2)) %>%
    mutate(late.app = (year >= dist_list_elev[[n]]$y2[1]-2 & year <= dist_list_elev[[n]]$y2[1]+2))
  
    df1 = df %>% dplyr::select(stateroute, early.app) %>%
    filter(early.app == "TRUE")
    
    df2 = df %>% dplyr::select(stateroute, late.app) %>%
    filter(late.app == "TRUE")
    
    repeats = df1 %>% filter(stateroute %in% df2$stateroute)
    df = df %>% filter(stateroute %in% repeats$stateroute)
    
  bbsRepeated_list[[n]] = df
  routes_list = unique(df$stateroute)
  bbsRoutes_fin[[n]] = data_frame(bbc_site = n, stateroute = routes_list) 
  
}

## Filter BBS count data to relevant surveys based on selected sites and survey years

bbsCount = read.csv("bbs_count_filter.csv")

counts_list = list()
for (n in 1:length(dist_list_elev)) {
  counts_list[[n]] = bbsCount %>% 
    dplyr::select(-X) %>%
    filter(stateroute %in%  bbsRepeated_list[[n]]$stateroute) %>%
    filter(rpid == 101) %>%
    filter(staterouteyear %in% bbsWeather$staterouteyear) %>%
    mutate(siteid = dist_list[[n]]$siteid[1]) %>%
    mutate(aou.route = ((aou*100000)+stateroute)) %>%
    mutate(site = n) %>%
    filter((year >= dist_list[[n]]$y1[1]-2 & year <= dist_list[[n]]$y1[1]+2) |
             (year >= dist_list[[n]]$y2[1]-2 & year <= dist_list[[n]]$y2[1]+2))
}

## Filter out transient species, (e.g. present in 33% or fewer surveys)

counts_list_final = list()
for (n in 1: length(counts_list)) {
  print(n)
  bird = counts_list[[n]] %>%
    distinct(stateroute, aou) %>%
    mutate(aou.route = ((aou*100000)+stateroute)) %>%
    mutate(bbc.site = n)
  bird$presence = NA
  for (l in 1: nrow(bird)) {
    df = filter(bbsWeather, bbsWeather$stateroute == bird$stateroute[l])
    tot_survey = nrow(df)
    
    df2 = bbsCount
    df2 = filter(df2, df2$stateroute == bird$stateroute[l], df2$aou == bird$aou[l])
    
    tot_present = nrow(df2)
    bird$tot_survey[l] = tot_survey
    bird$presence[l] = (tot_present/tot_survey)
    
  }
  bird = filter(bird, presence >= .33)
  counts_list_final[[n]]= filter(counts_list[[n]],
                           counts_list[[n]]$aou.route %in% bird$aou.route) 
}

## Generate tables for final routes and counts to be used in analysis

counts_df = bind_rows(counts_list)
subset_bbsCounts = write.csv(counts_df, "subset_bbsCounts.csv")

routes_df = bind_rows(bbsRoutes_fin)
routes_df = left_join(routes_df, bbsRoutes, by = "stateroute")
subset_bbsRoutes = write.csv(routes_df, "subset_bbsRoutes.csv")

write.csv(bbcSitesFin2, "bbcSitesFin2.csv")

# Generate a histogram of surveys at each BBS stateroute in year 1 and year 2

pdf(file = "bbs_hist_byroute.pdf")
for(i in 1:length(counts_list)) {
  
  par(mfrow = c(1,2))
  
  occurrence1 = bbsWeather %>% 
    filter(stateroute %in% counts_list[[i]]$stateroute, year >= bbcSitesFin$y1[i] - 2 & year <= bbcSitesFin$y1[i] +2)  
  
  occurrence2 = bbsWeather %>% 
    filter(stateroute %in% counts_list[[i]]$stateroute, year >= bbcSitesFin$y2[i] - 2 & year <= bbcSitesFin$y2[i] +2) 
  
  barplot(table(occurrence1$stateroute), xlab = "Y1 state route surveys", main = paste("Site ", i, ":", bbcSitesFin$State[i]))
  barplot(table(occurrence2$stateroute), xlab = "Y2 state route surveys")
  
}
dev.off()

par(mfrow = c(1, 1))

# Generate map of BBC sites to be used in analysis and all potential BBS survey routes

pdf(file = "map routes and surveys.pdf")
us_states2163 = st_transform(us_states, 2163)
us_map = tm_shape(us_states2163) + 
  tm_polygons() + 
  tm_layout(frame = FALSE) 

us_map = us_map + 
  tm_shape(sf_bbcSites)+
  tm_dots(col = "red", size = .07, shape = 8)

for( n in 1: length(dist_list)) {
  us_map = us_map + 
    tm_shape(dist_list[[n]]) +
    tm_dots()
}

print(us_map)

dev.off()
