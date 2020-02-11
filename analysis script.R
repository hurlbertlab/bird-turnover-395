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

pdf(file = "bbs_hist_byroute.pdf")
for(i in 1:length(counts_list)) {
  
  par(mfrow = c(1,2))
  
  occurrence1 = counts_list[[i]] %>% 
    filter(year >= bbcSitesFinal$y1[i] - 2 & year <= bbcSitesFinal$y1[i] +2)  
  
  occurrence2 = counts_list[[i]] %>% 
    filter(year >= bbcSitesFinal$y2[i] - 2 & year <= bbcSitesFinal$y2[i] +2) 

  barplot(table(occurrence1$stateroute), xlab = "Y1 surveys")
  barplot(table(occurrence2$stateroute), xlab = "Y2 surveys")
  # try ggplot for better main title/label format
}
dev.off()
par(mfrow = c(1, 1))

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

