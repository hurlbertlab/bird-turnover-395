
######################################
## ENEC 395: Avian community similarity: script 1
# Ellie Kremer
# 12/3/2019
######################################


# Read in raw data files

library(dplyr)
library(sf)

# rdataretriever may not work, in this case download data files directly from USGS BBS website
#https://www.pwrc.usgs.gov/bbs/results/

#library(reticulate)
#use_condaenv("r-reticulate", require =TRUE)
#library(rdataretriever)
#bbs = rdataretriever::fetch('breed-bird-survey')

## Direct download of data
bbsWeather_1 = read.csv("bbs-2017/bbs_weather_20170712.csv")
bbsCounts_1 = read.csv("bbs-2017/bbs_counts_20170712.csv")
bbsRoutes_1 = read.csv("bbs-2017/bbs_routes_20170712.csv")

#bbsWeather = bbs$weather %>% 
## alternate code, if using rdataretriever comment out following line 
bbsWeather = bbsWeather_1 %>%
  mutate(stateroute = (statenum * 1000)+ route) %>% 
  mutate(staterouteyear = (stateroute* 10000) + year) %>%
  filter(runtype == 1)

# Filter to remove survey years before BBC data is available, adding identifiers, isolating land birds 

#bbsCounts_filter = bbs$counts %>%
## alternate code, if using rdataretriever comment out following line 
bbsCounts_filter = bbsCounts_1 %>%
  filter(year >= 1985) %>%
  mutate(stateroute = (statenum * 1000)+ route) %>% 
  mutate(staterouteyear = (stateroute* 10000) + year) %>%
  filter (staterouteyear %in% bbsWeather$staterouteyear) %>%
  filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010)

#bbsRoutes = bbs$routes
## alternate code, if using rdataretriever comment out following line
bbsRoutes = bbsRoutes_1

write.csv(bbsRoutes, file = "bbs_routes.csv")
write.csv(bbsCounts_filter, file = "bbs_count_filter.csv")
write.csv(bbsWeather, file = "bbs_weather.csv")
 