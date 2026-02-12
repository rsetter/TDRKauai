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
assessors_ce <- read.csv("assessors_ce.csv")
assessors_rate<-read.csv("assessors_rate.csv")
assessors_depth<-read.csv("assessors_depth.csv")
assessors_setback<-read.csv("assessors_setback.csv")
assessors_slrxa<-read.csv("assessors_slrxa.csv")
assessors_inland<-read.csv("assessors_inland.csv")



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
assessors_ce %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()


#parcels in rate setback
assessors_rate

# total value of parcels in rate-based setback
sum(assessors_rate$MKTTOT25)
# total number of parcels in rate-based setback
length(unique(assessors_rate$COTMK,na.rm=T))
# total number of CPR's in rate-based setback
length(unique(assessors_rate$PARID,na.rm=T))
assessors_rate %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd in rate-based setback
assessors_rate %>%
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





#parcels in depth+rate setback - residential
residential <- c("Owner-Occupied", "Vacation Rental","Non-Owner-Occupied Residential","Owner-Occupied Mixed Use")  
assessors_setback_res <- assessors_setback %>%
  filter(TAXCLASS25 %in% residential)

# total value of parcels in setback
sum(assessors_setback_res$MKTTOT25)
# total number of parcels in setback
length(unique(assessors_setback_res$COTMK,na.rm=T))
# total number of CPR's in setback
length(unique(assessors_setback_res$PARID,na.rm=T))
assessors_setback_res %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd in setback
assessors_setback_res %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()



#parcels ag

# total value of parcels in setback
sum(assessors_ag$MKTTOT25)
# total number of parcels in setback
length(unique(assessors_ag$COTMK,na.rm=T))
# total number of CPR's in setback
length(unique(assessors_ag$PARID,na.rm=T))
assessors_ag %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd in setback
assessors_ag %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()



#ag CPR

# total value of parcels in setback
sum(assessors_agCPR$MKTTOT25)
# total number of parcels in setback
length(unique(assessors_agCPR$COTMK,na.rm=T))
# total number of CPR's in setback
length(unique(assessors_agCPR$PARID,na.rm=T))
assessors_agCPR %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd in setback
assessors_agCPR %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()




# blackknight

CE_parid <- unique(assessors_ce$PARID)
rate_parid <- unique(assessors_rate$PARID)
depth_parid <- unique(assessors_depth$PARID)
setback_parid <- unique(assessors_setback$PARID)
setback_parid_res <- unique(assessors_setback_res$PARID)
#slrxa_parid <- unique(assessors_slrxa$PARID)
#inland_parid <- unique(assessors_inland$PARID)
ag_parid <- unique(assessors_ag$PARID[assessors_ag$IAL == 0]) #not IAL
agCPR_parid <- unique(assessors_agCPR$PARID[assessors_agCPR$IAL == 0])


bk_CE <- bk_assessors[bk_assessors$PARID %in% CE_parid, ]
bk_rate <- bk_assessors[bk_assessors$PARID %in% rate_parid, ]
bk_depth <- bk_assessors[bk_assessors$PARID %in% depth_parid, ]
bk_setback <- bk_assessors[bk_assessors$PARID %in% setback_parid, ]
bk_setback_res <- bk_assessors[bk_assessors$PARID %in% setback_parid_res, ]
#bk_slrxa <- bk_assessors[bk_assessors$PARID %in% slrxa_parid, ]
#bk_inland <- bk_assessors[bk_assessors$PARID %in% inland_parid, ]
bk_ag <- bk_assessors[bk_assessors$PARID %in% ag_parid, ]
bk_agCPR <- bk_assessors[bk_assessors$PARID %in% agCPR_parid, ]


#number of sales
nrow(bk_CE)
nrow(bk_rate)
nrow(bk_depth)
nrow(bk_setback)
nrow(bk_setback_res)
#nrow(bk_slrxa)
#nrow(bk_inland)
nrow(bk_ag)
nrow(bk_agCPR)

#number of unique parcels sold
length(unique(bk_CE$PARID))
length(unique(bk_rate$PARID))
length(unique(bk_depth$PARID))
length(unique(bk_setback$PARID))
length(unique(bk_setback_res$PARID))
#length(unique(bk_slrxa$PARID))
#length(unique(bk_inland$PARID))
length(unique(bk_ag$PARID))
length(unique(bk_agCPR$PARID))





bk_setback_res <- bk_setback_res %>%
  left_join(
    assessors_setback_res %>% 
      st_drop_geometry() %>%  
      select(PARID, buildable_area, buildable_depth, bldg_area_lost_pct,PARCEL_SQFT),
    by = "PARID"
  )



