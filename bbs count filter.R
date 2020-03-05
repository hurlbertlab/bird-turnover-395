## ENEC 395: Climate and avian turnover script 1

library(dplyr)
library(sf)

# rdataretriever may not work, in this case download data files directly from USGS BBS website
library(reticulate)
use_condaenv("r-reticulate", require =TRUE)
library(rdataretriever)
bbs = rdataretriever::fetch('breed-bird-survey')

bbsWeather = bbs$weather %>% 
  mutate(stateroute = (statenum * 1000)+ route) %>% 
  mutate(staterouteyear = (stateroute* 10000) + year) %>%
  filter(runtype == 1)


bbc_statenum_total = c("18", "18", "61", "14", "14", "18","22", "18", 
                 "80","18", "68","14", "61", "68", "80", "82", "68", "14", "63", "27",
                  "46", "59", "72", "77", "47", "72", "87", "49") 

# remove years before bbc, adding identifiers, isolating land birds 
bbsCounts_filter = bbs$counts %>% 
  filter(year >= 1985) %>%
  filter(statenum %in% bbc_statenum_total) %>%
  mutate(stateroute = (statenum * 1000)+ route) %>% 
  mutate(staterouteyear = (stateroute* 10000) + year) %>%
  filter (routedataid %in% bbsWeather$routedataid) %>%
  filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010)

bbsRoutes = bbs$routes

write.csv(bbsRoutes, file = "bbs_routes.csv")
write.csv(bbsCounts_filter, file = "bbs_count_filter.csv")
write.csv(bbsWeather, file = "bbs_weather.csv")
