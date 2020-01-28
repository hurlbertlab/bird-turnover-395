## ENEC 395: Climate and avian turnover

# Library
library(dplyr)
library(sf)

# Read in BBS data
library(reticulate)
use_condaenv("r-reticulate", require =TRUE)
library(rdataretriever)
bbs = rdataretriever::fetch('breed-bird-survey')

bbsRoutes = bbs$routes

# Read in Breeding Bird Census (BBC) data
bbc_censuses = read.csv("bbc_censuses.csv", header = TRUE, sep = ",")
bbc_counts = read.csv("bbc_counts.csv", header = TRUE, sep = ",")
bbc_sites = read.csv("bbc_sites.csv", header = TRUE, sep = ",")

# Adding state identifiers -- streamline w/ function?
state_conver = read.csv("state_convers.csv", header = TRUE, sep = ",")
bbc_states = c("Connecticut", "Connecticut", "New York", "California", "California", "Connecticut", "District of Columbia", "Connecticut", "South Carolina", "Connecticut", "Ontario", "California", "New York", "Ontario", "South Carolina", "Tennessee", "Ontario", "California")
bbc_statenum = c("18", "18", "61", "14", "14", "18","22", "18", 
                 "80","18", "68","14", "61", "68", "80", "82", "68", "14")

# Filter sites ( >= 2 census years, >= 10 years apart)
bbcCensusCount <- data.frame(siteID_uni = unique(bbc_censuses$siteID), 
                             count = NA, time_range = NA)

for (s in 1: length(bbcCensusCount$siteID_uni)) {
  tmpCount = filter(bbc_censuses, siteID, siteID == bbcCensusCount$siteID_uni[s])
  tmpTime = max(tmpCount$year) - min(tmpCount$year)
  
  bbcCensusCount$count[s] = nrow(tmpCount)
  bbcCensusCount$time_range[s] = tmpTime
  }

bbcCountTemp = bbcCensusCount %>% filter(count>= 2, time_range>=10) 
bbcCensusFinal = filter(bbc_censuses, siteID, siteID %in% bbcCountTemp$siteID_uni)

bbcSitesTemp = filter(bbc_sites, siteID, siteID %in% bbcCensusFinal$siteID)
bbcSitesFinal = bbcSitesTemp %>% 
  select(siteID:longitude) %>% 
  distinct() %>%
  mutate(State = bbc_states) %>%
  mutate(StateNum = bbc_statenum)

# Change to neg long values
bbcSitesFinal$longitude = -(bbcSitesFinal$longitude)

# Create spatial data frame
sf_bbsRoutes = st_as_sf(bbsRoutes, 
                   coords = c("longitude", "latitude"),
                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sf_bbcSites = st_as_sf(bbcSitesFinal,
                       coords = c("longitude", "latitude"),
                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Area specific BBS Datasets
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

#Dist calculations 
## list containing dataframe for each site with dist calc 
# for each route in surrounding area
 
dist_list = list()

for (n in 1: length(sf_bbcSites$siteID)) {
  state = sf_bbcSites$StateNum[n]
  if (state == "18") {df = conn_bbs}
  if(state == "61") {df = ny_bbs}
  if(state == "14") {df = cali_bbs}
  if(state == "22") {df = dc_bbs}
  if(state == "80") {df = sc_bbs}
  if(state == "68") {df = ont_bbs}
  if(state == "82") {df = tenn_bbs}
  
  site = df
  for (s in 1:length(df)) {
   site$dist[s] = st_distance(sf_bbcSites[n,], df[s,], "Great Circle")
  }
  dist_list[[n]] = site
}
