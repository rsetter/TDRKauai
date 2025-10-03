library(data.table)
library(foreign)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)


source('config_dir.R')

# Disable scientific notation
options(scipen = 999)





# identify parcels in setback

#open pre-processed files
write.csv(assessors_ce,"assessors_ce.csv")
rate_parid <- unique(assessors_rate$PARID)
depth_parid <- unique(assessors_depth$PARID)
setback_parid <- unique(assessors_setback$PARID)
slrxa_parid <- unique(assessors_slrxa$PARID)
inland_parid <- unique(assessors_inland$PARID)



#parcels in CE
assessors_ce

# total value of parcels in CE
sum(assessors_ce$MKTTOT25,na.rm=T)
# total number of parcels in CE
length(unique(assessors_ce$COTMK,na.rm=T))
# total number of CPR's in CE
length(unique(assessors_ce$PARID,na.rm=T))
assessors_ce %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow()  
# total number of parcels CPR'd in CE
length(unique(assessorscpr$COTMK,na.rm=T))
assessors_ce %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()





#parcels in depth setback
assessors_depth

# total value of parcels in depth-based setback
sum(assessors_depth$MKTTOT25)
# total number of parcels in depth-based setback
length(unique(assessors_depth$COTMK,na.rm=T))
# total number of CPR's in depth-based setback
length(unique(assessors_depth$PARID,na.rm=T))
assessors_depth %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd in depth-based setback
assessors_depth %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()


#parcels in depth+rate setback
assessors_setback

# total value of parcels in setback
sum(assessors_setback$MKTTOT25)
# total number of parcels in setback
length(unique(assessors_setback$COTMK,na.rm=T))
# total number of CPR's in setback
length(unique(assessors_setback$PARID,na.rm=T))
assessors_setback %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd in setback
assessors_setback %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()




#parcels in slrxa
assessors_slrxa

# total value of parcels in slrxa
sum(assessors_slrxa$MKTTOT25)
# total number of parcels in slrxa
length(unique(assessors_slrxa$COTMK,na.rm=T))
# total number of CPR's in slrxa
length(unique(assessors_slrxa$PARID,na.rm=T))
assessors_slrxa %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd in slrxa
assessors_slrxa %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()




#parcels non-coastal
assessors_inland

# total value of parcels inland
sum(assessors_inland$MKTTOT25)
# total number of parcels inland
length(unique(assessors_inland$COTMK,na.rm=T))
# total number of CPR's inland
length(unique(assessors_inland$PARID,na.rm=T))
assessors_inland %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd inland
assessors_inland %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()





# blackknight

CE_parid <- unique(assessors_ce$PARID)
rate_parid <- unique(assessors_rate$PARID)
depth_parid <- unique(assessors_depth$PARID)
setback_parid <- unique(assessors_setback$PARID)
slrxa_parid <- unique(assessors_slrxa$PARID)
inland_parid <- unique(assessors_inland$PARID)


bk_CE <- bk_assessors[bk_assessors$PARID %in% CE_parid, ]
bk_rate <- bk_assessors[bk_assessors$PARID %in% rate_parid, ]
bk_depth <- bk_assessors[bk_assessors$PARID %in% depth_parid, ]
bk_setback <- bk_assessors[bk_assessors$PARID %in% setback_parid, ]
bk_slrxa <- bk_assessors[bk_assessors$PARID %in% slrxa_parid, ]
bk_inland <- bk_assessors[bk_assessors$PARID %in% inland_parid, ]


#number of sales
nrow(bk_CE)
nrow(bk_depth)
nrow(bk_setback)
nrow(bk_slrxa)
nrow(bk_inland)

#number of unique parcels sold
length(unique(bk_CE$PARID))
length(unique(bk_depth$PARID))
length(unique(bk_setback$PARID))
length(unique(bk_slrxa$PARID))
length(unique(bk_inland$PARID))
