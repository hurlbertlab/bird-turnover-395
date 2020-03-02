## ENEC 395: Climate and avian turnover script 2

# Final Products
# bbcCensusFin - all censuses from selected BBC sites with at least 2 surveys 
# 10 or more years apart
# counts_list - each list space holds surveys taken at early/late BBC site years 
# from 10 closest BBS sites

# Library
library(elevatr)
library(raster)
library(dplyr) 
library(sf)
library(lwgeom)


# Read in Breeding Bird Census (BBC) data

bbc_censuses = read.csv("bbc data/bbc_censuses.csv", header = TRUE, sep = ",")
bbc_counts = read.csv("bbc data/bbc_counts.csv", header = TRUE, sep = ",")
bbc_sites = read.csv("bbc data/bbc_sites.csv", header = TRUE, sep = ",")

# Read in elevation data
elev <- raster("Elevation_GRID/NA_Elevation/data/NA_Elevation/na_elevation")

# Species name conversion - Code via Di Cecco
# Match species common names to BBS species list
{
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


# Adding state identifiers based on BBS statenum 
state_conver = read.csv("state_convers.csv", header = TRUE, sep = ",")
bbc_states = c("Connecticut", "Connecticut", "New York", "California", "California", "Connecticut", "District of Columbia", "Connecticut", "South Carolina", "Connecticut", "Ontario","California", "New York", "Ontario", "South Carolina", "Tennessee", "Ontario", "California")
bbc_statenum = c("18", "18", "61", "14", "14", "18","22", "18", 
                 "80","18", "68","14", "61", "68", "80", "82", "68", "14")
}
# Filter BBC censuses and sites ( >= 2 census years, >= 10 years apart)
{
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
}

bbcSitesFin = bbc_sites %>%
  dplyr::select(siteID:longitude) %>%
  filter(siteID %in% bbcCensusFin$siteID) %>%
  distinct() %>%
  # removing siteid 177 because gap in bbs for y2 year
  #filter(siteID != 177) %>%
  mutate(State = bbc_states) %>%
  mutate(StateNum = bbc_statenum)

for (s in 1:nrow(bbcSitesFin)) {
  bbcCensusTemp= filter(bbcCensusFin, siteID == bbcSitesFin$siteID[s])
  bbcSitesFin$y1[s] = min(bbcCensusTemp$year)
  bbcSitesFin$y2[s] = max(bbcCensusTemp$year)
}
####
crs.new = CRS("+proj=laea +lat_0=-100 +lon_0=6370997 +x_0=45 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

latlong = data.frame(long = -bbcSitesFin$longitude, lat = bbcSitesFin$latitude)
sp::coordinates(latlong) = c(latlong$long, latlong$lat)
proj4string(latlong) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
latlong = spTransform(latlong, crs.new)
df = raster::extract(elev, latlong)

# Change to neg long values
bbcSitesFin$longitude = -(bbcSitesFin$longitude)

# Create spatial data frame
{
sf_bbsRoutes = st_as_sf(bbs$routes, 
                   coords = c("longitude", "latitude"),
                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sf_bbsRoutes$stateroute = (sf_bbsRoutes$statenum * 1000)+ sf_bbsRoutes$route


sf_bbcSites = st_as_sf(bbcSitesFin,
                       coords = c("longitude", "latitude"),
                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

longlat = data.frame(long = bbcSitesFin$longitude, lat = bbcSitesFin$latitude)
sp_longlat = SpatialPoints(longlat, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox = NULL)
sp_longlat = spTransform(sp_longlat, CRSobj = crs.new)
df= extract(elev, sp_longlat)
#df = sf_bbcSites %>% select(geometry) %>% get_elev_point()
#sf_bbcSites$elev_m = df$elevation
#######
df = raster::extract(elev, latlong)
#sf_bbcSites$elev_m = df$elevation

}
# Area specific BBS Datasets
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
# Dist calculations 
# list of dataframes for each site with dist calc for each route in surrounding area
 
dist_list = list()
for (n in 1: nrow(sf_bbcSites)) {
  state = sf_bbcSites$StateNum[n]
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

  #site1 = top_n(site, 10, site$dist)
  #if(sf_bbcSites[n,]$siteID == 177) {site1 = top_n(site, 20, site$dist)}

  #site1$dist = -site1$dist
  
  site1 = site %>% filter(dist < 100000)
  
  site1$y1 = sf_bbcSites$y1[n]
  site1$y2 = sf_bbcSites$y2[n]
  
  dist_list[[n]] = site1
}
## calculate elevation for bbs routes and filter to within +/- 100 m of bbc elev
#for (n in 1:length(dist_list)) {
 # df = dist_list[[n]] %>% select(geometry) %>% get_elev_point()
  #dist_list[[n]]$elev_m = df$elevation
#}
#dist_list_elev = dist_list
#for(n in 1: length(dist_list)) {
 # dist_list_elev[[n]] = filter(dist_list[[n]], (elev_m <= sf_bbcSites$elev_m[n] + 100) &
  #         (elev_m >= sf_bbcSites$elev_m[n] - 100))
#}




for (n in 1:length(dist_list)) {
 df = extract(elev, dist_list[[n]]$geometry)
 dist_list[[n]]$elev_m = df$elevation
}
dist_list_elev = dist_list
for(n in 1: length(dist_list)) {
 dist_list_elev[[n]] = filter(dist_list[[n]], (elev_m <= sf_bbcSites$elev_m[n] + 100) &
         (elev_m >= sf_bbcSites$elev_m[n] - 100))
}


bbsRepeated_list = list()
for (n in 1: length(dist_list)) {
  df = bbsWeather %>%
    filter(stateroute %in% dist_list[[n]]$stateroute) %>%
    mutate(early.app = (year >= dist_list[[n]]$y1[1]-2 & year <= dist_list[[n]]$y1[1]+2)) %>%
    mutate(late.app = (year >= dist_list[[n]]$y2[1]-2 & year <= dist_list[[n]]$y2[1]+2))
  
    df1 = df %>% dplyr::select(stateroute, early.app) %>%
    filter(early.app == "TRUE")
    
    df2 = df %>% dplyr::select(stateroute, late.app) %>%
    filter(late.app == "TRUE")
    
    repeats = df1 %>% filter(stateroute %in% df2$stateroute)
    df = df %>% filter(stateroute %in% repeats$stateroute)
  bbsRepeated_list[[n]] = df
}

## filter bbs by site and year, match counts to early/late bbc years
# 5 year window (2 on either side of bbc)

bbsCount = read.csv("bbs_count_filter.csv")


counts_list = list()
for (n in 1:length(dist_list)) {
  counts_list[[n]] = bbsCount %>% 
    dplyr::select(-X) %>%
    filter(stateroute %in%  bbsRepeated_list[[n]]$stateroute) %>%
    filter(rpid == 101) %>%
    filter(staterouteyear %in% bbsWeather$staterouteyear) %>%
    mutate(siteid = dist_list[[n]]$siteid[1]) %>%
    mutate(aou.route = ((aou*100000)+stateroute)) %>%
    filter((year >= dist_list[[n]]$y1[1]-2 & year <= dist_list[[n]]$y1[1]+2) |
             (year >= dist_list[[n]]$y2[1]-2 & year <= dist_list[[n]]$y2[1]+2))
}

##### filter for transience, (e.g. present in 33% or more surveys)

for (n in 1: length(counts_list)) {
  bird = counts_list[[n]] %>%
    distinct(stateroute, aou) %>%
    mutate(aou.route = ((aou*100000)+stateroute))
  bird$presence = NA
  for (l in 1: nrow(bird)) {
    df = filter(bbsWeather, bbsWeather$stateroute == bird$stateroute[l])
    tot_survey = nrow(df)
    
    df2 = bbsCount
    df2 = filter(df2, df2$stateroute == bird$stateroute[l], df2$aou == bird$aou[l])
    tot_present = nrow(df2)
    bird$tot_survey = tot_survey
    bird$presence[l] = (tot_present/tot_survey)
  }
  bird = filter(bird, presence >= .33)
  counts_list[[n]]= filter(counts_list[[n]],
                           counts_list[[n]]$aou.route %in% bird$aou.route) 
}
#list of bbs routes for each bbc site (counts list filt by dist list)
# bbcid column in counts
# subset to max and min bbc census years, add y1 y2 and bbc id columns into bbs
