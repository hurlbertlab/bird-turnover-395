## ENEC 395: Climate and avian turnover script 3
# Final products: histogram pdf, bbcSpeciesCount: table of bbc aou counts 
# filtered by relevant sites and breeders, counts_list: list of tables of bbs data 
# for each bbc site, removed transient

library(dplyr)
library(sf)
library(ggplot2)
# Checking temporal distribution of surveys
# must run filtering scripts first and save objects for counts_list


pdf(file = "bbs_hist.pdf")
for(i in 1:length(counts_list)) {
  hist(counts_list[[i]]$year, breaks = 20, xlab = "year", 
       main = paste("Site ", i))
}
dev.off()
#####



#####
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
# next steps
# species list filter, 
# bbc- only keep breeders
# bbs - remove transient (any observed only 1 time out of 5 year window)

bbcSpeciesCount = bbc_counts %>%
  filter(siteID %in% bbcSitesFin) %>%
  filter(status == "breeder")

# bbs - remove transient (any observed only 1 time out of 5 year window)
# counts list, group by aou in early/late survey, filter by present >1 time 

for (n in 1: length(counts_list)) {
  occurrence1 = counts_list[[n]] %>% 
    filter(year >= bbcSitesFinal$y1[n] - 2 & year <= bbcSitesFinal$y1[n] +2) %>%
    count(aou) 
  occurrence1 = filter(occurrence1, n, n >1)
  
  occurrence2 = counts_list[[n]] %>% 
    filter(year >= bbcSitesFinal$y2[n] - 2 & year <= bbcSitesFinal$y2[n] +2) %>%
    count(aou)
  occurrence2 = filter(occurrence2, n, n >1)
  
  counts_list[[n]] = filter(counts_list[[n]], aou %in% occurrence1$aou |aou %in% occurrence2$aou)
}


subset_bbsCounts = read.csv("subset_bbsCounts.csv")
subset_bbsRoutes = read.csv("subset_bbsRoutes.csv")
landcover_US = read.csv("fragmentation_indices_nlcd_simplified.csv")
newcode <- data.frame(code = seq(1,9), 
                                   legend = c("Open water", "Urban", "Barren", "Forest", "Shrubland", 
                                              "Agricultural", "Grasslands", "Wetlands", "Perennial ice, snow"))
landcover_US_2001 = landcover_US %>%
  select(file:prop.landscape) %>%
  filter(year == "2001") %>%
  filter(stateroute %in% subset_bbsRoutes)

  summarise(max(prop.landscape))

for(n in 1: nrow(subset_bbsRoutes)) {
  df = filter(landcover_US_2001, stateroute == subset_bbsRoutes[,n])
  max_land.prop = max(df$prop.landscape)
  
  for(l in 1: nrow(df)) {
    df2 = filter(df, prop.landscape == max_land.prop)
    subset_bbsRoutes[n]$landcover = df2[1]$class
  }
  
  subset_bbsRoutes[,n]$landcover = 
}