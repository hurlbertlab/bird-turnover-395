## ENEC 395: Climate and avian turnover

# Library
library(dplyr)

# Read in Breeding Bird Census (BBC) data
bbc_censuses = read.csv("bbc_censuses.csv", header = TRUE, sep = ",")
bbc_counts = read.csv("bbc_counts.csv", header = TRUE, sep = ",")
bbc_sites = read.csv("bbc_sites.csv", header = TRUE, sep = ",")

# Filter sites ( >= 2 census years, >= 10 years apart)
bbcCensusCount <- data.frame(siteID_uni = unique(bbc_censuses$siteID), count = NA, time_range = NA)

for (s in 1: length(bbcCensusCount$siteID_uni)) {
  tmpCount = filter(bbc_censuses, siteID, siteID == bbcCensusCount$siteID_uni[s])
  tmpTime = max(tmpCount$year) - min(tmpCount$year)
  
  bbcCensusCount$count[s] = nrow(tmpCount)
  bbcCensusCount$time_range[s] = tmpTime
  }

bbcCountTemp = bbcCensusCount %>%
  filter(count, count>= 2) %>%
  filter(time_range, time_range>= 10)
bbcCensusFinal = filter(bbc_censuses, siteID, siteID %in% bbcCountTemp$siteID_uni)

bbcSitesTemp = filter(bbc_sites, siteID, siteID %in% bbcCensusFinal$siteID)
bbcSitesFinal = bbcSitesTemp %>% 
  select(siteID:longitude) %>% 
  distinct()

# Read in BBS data
library(reticulate)
use_condaenv("r-reticulate", require =TRUE)
library(rdataretriever)
bbs = rdataretriever::fetch('breed-bird-survey')

bbsRoutes = bbs$routes

# Change to pos long values to match BBC
bbcSitesFinal$longitude = -(bbcSitesFinal$longitude)

# Create spatial data frame
sf_bbsRoutes = st_as_sf(bbsRoutes, 
                   coords = c("longitude", "latitude"),
                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sf_bbcSites = st_as_sf(bbcSitesFinal,
                       coords = c("longitude", "latitude"),
                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## filter to states, great circle 
## for loop of closest routes to sites(by great circle dist)
## save for loop output for each site

