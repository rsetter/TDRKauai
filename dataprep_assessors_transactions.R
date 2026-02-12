library(data.table)
library(foreign)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


source('config_dir.R')

# Disable scientific notation
options(scipen = 999)


#prep datasets






# open lot-based setback dataset
setbackdepthshp <- st_read(setback_folder,layer=setbackdepth_layer)
setbacklot <- as.data.frame(setbackdepthshp)

#important columns
# TMK 
# LD_SETBK_F = lot depth setback distance in feet for each parcel
# AVE_DEP_FT = average lot depth in feet

setbacklot <- setbacklot[, c("PARID","LD_SETBK_F","AVE_DEP_FT")]
setbacklot <- setbacklot[setbacklot$LD_SETBK_F > 0,]

#remove duplicates 
setbacklot <- setbacklot %>% distinct(PARID, .keep_all = TRUE)





#open rate-based setback dataset
setbackrateshp <- st_read(setback_folder,layer=setbackrate_layer)


#create cumulative setback shapefile
setbackdepthshp <- st_make_valid(setbackdepthshp)
setbackrateshp <- st_make_valid(setbackrateshp)
setbackshp <- st_union(setbackdepthshp, setbackrateshp)




#open ce dataset
slrceshp <- st_read(slrce_folder)




#open building footprints dataset 2025
bldgftprntshp <- st_read(bldgftprnt_folder,fid_column_name = "bldgid")
#calculate building footprint cover by setback / CE
setback_shps <- list(setback = st_transform(setbackshp, st_crs(bldgftprntshp)), 
                      setback_depth = st_transform(setbackdepthshp, st_crs(bldgftprntshp)),
                      setback_rate = st_transform(setbackrateshp, st_crs(bldgftprntshp)),
                      slrce = st_transform(slrceshp, st_crs(bldgftprntshp)))
bldg_setback_cover <- lapply(names(setback_shps), function(name) {
  overlaps <- st_intersects(bldgftprntshp, setback_shps[[name]]) #filter out only buildings within any setback designation
  has_overlap <- lengths(overlaps) > 0
  if(sum(has_overlap) == 0) return(NULL)
  st_intersection(bldgftprntshp[has_overlap, ], setback_shps[[name]]) %>%
    mutate(overlap_sqft = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    group_by(bldgid) %>%
    summarise(!!paste0(name, "_sqft") := sum(overlap_sqft))
})
for (result in bldg_setback_cover) {
  bldgftprntshp <- left_join(bldgftprntshp, result, by = "bldgid")
}
bldgftprntshp <- bldgftprntshp %>%
  mutate(across(ends_with("_sqft"), ~replace_na(., 0)))
bldgftprntshp$BLDG_SQFT <- as.numeric(st_area(bldgftprntshp)) * 10.7639
bldgftprntshp <- bldgftprntshp %>%
  mutate(across(c(setback_sqft, setback_depth_sqft, setback_rate_sqft, slrce_sqft), 
                ~ . * 10.7639))




#open parcel + assessors dataset 2025
parcelshp <- st_read(kauaiparcel_folder,layer=kauaiparcel_layer)
#calculate parcel cover by setback / CE
parcelshp$PARCEL_SQFT <- as.numeric(st_area(parcelshp)) * 10.7639
setback_shps <- list(setback = st_transform(setbackshp, st_crs(parcelshp)),
  setback_depth = st_transform(setbackdepthshp, st_crs(parcelshp)),
  setback_rate = st_transform(setbackrateshp, st_crs(parcelshp)),
  slrce = st_transform(slrceshp, st_crs(parcelshp)))
parcel_setback_cover <- lapply(names(setback_shps), function(name) {
  overlaps <- st_intersects(parcelshp, setback_shps[[name]])
  has_overlap <- lengths(overlaps) > 0
  if(sum(has_overlap) == 0) return(NULL)
  st_intersection(parcelshp[has_overlap, ], setback_shps[[name]]) %>%
    mutate(overlap_sqft = as.numeric(st_area(.)) * 10.7639) %>%  # Convert to sqft
    st_drop_geometry() %>%
    group_by(PARID) %>%
    summarise(!!paste0(name, "_sqft") := sum(overlap_sqft))
})
for (result in parcel_setback_cover) {
  if(!is.null(result)) {
    parcelshp <- left_join(parcelshp, result, by = "PARID")
  }
}
parcelshp <- parcelshp %>%
  mutate(across(ends_with("_sqft") & !PARCEL_SQFT, ~replace_na(., 0)))





#join building footprint data with parcels 
# assign each building to parcel with most overlap
bldgftprntshp <- st_transform(bldgftprntshp, st_crs(parcelshp))
#bldgftprntshp <- st_join(bldgftprntshp, parcelshp[c("PARID","COTMK","TMK")], largest = TRUE) # no need this with the new 2025 building dataset
# column indicating >50% cover by setback
bldgftprntshp <- bldgftprntshp %>%
  mutate(
    setback_over50 = (setback_sqft / BLDG_SQFT) > 0.5,
    setback_depth_over50 = (setback_depth_sqft / BLDG_SQFT) > 0.5,
    setback_rate_over50 = (setback_rate_sqft / BLDG_SQFT) > 0.5,
    slrce_over50 = (slrce_sqft / BLDG_SQFT) > 0.5
  )
# building summary by parcel
parcel_summary <- bldgftprntshp %>%
  st_drop_geometry() %>%
  group_by(PARID) %>% 
  summarise(
    n_buildings = n(),
    n_setback_over50 = sum(setback_over50, na.rm = TRUE),
    n_setback_depth_over50 = sum(setback_depth_over50, na.rm = TRUE),
    n_setback_rate_over50 = sum(setback_rate_over50, na.rm = TRUE),
    n_slrces_over50 = sum(slrce_over50, na.rm = TRUE),
    largest_bldg_sqft = max(BLDG_SQFT, na.rm = TRUE),
    largest_setback_sqft = setback_sqft[which.max(BLDG_SQFT)],
    largest_setback_depth_sqft = setback_depth_sqft[which.max(BLDG_SQFT)],
    largest_setback_rate_sqft = setback_rate_sqft[which.max(BLDG_SQFT)],
    largest_slrces_sqft = slrce_sqft[which.max(BLDG_SQFT)]
  ) %>%
  mutate(
    TMK = as.numeric(paste0("4", substr(PARID, 1, 8)))
  )
# add building summary to parcels shapefile
parcelshp <- parcelshp %>%
  left_join(parcel_summary %>% select(-TMK), by = "PARID")
#add data to cpr's too
#if gisacres == 0.00180303 (it's a CPR) then use the same values as the TMK8
parcel_summary_cpr <- parcelshp %>%
  st_drop_geometry() %>%
  filter(GISACRES == 0.00180303) %>%
  select(PARID, TMK) %>%
  left_join(parcel_summary %>% select(-PARID), by = "TMK") %>%
  select(PARID, n_buildings:largest_slrces_sqft)
parcelshp <- parcelshp %>%
  left_join(parcel_summary_cpr, by = "PARID", suffix = c("", "_tmk")) %>%
  mutate(
    n_buildings = coalesce(n_buildings_tmk, n_buildings),
    n_setback_over50 = coalesce(n_setback_over50_tmk, n_setback_over50),
    n_setback_depth_over50 = coalesce(n_setback_depth_over50_tmk, n_setback_depth_over50),
    n_setback_rate_over50 = coalesce(n_setback_rate_over50_tmk, n_setback_rate_over50),
    n_slrces_over50 = coalesce(n_slrces_over50_tmk, n_slrces_over50),
    largest_bldg_sqft = coalesce(largest_bldg_sqft_tmk, largest_bldg_sqft),
    largest_setback_sqft = coalesce(largest_setback_sqft_tmk, largest_setback_sqft),
    largest_setback_depth_sqft = coalesce(largest_setback_depth_sqft_tmk, largest_setback_depth_sqft),
    largest_setback_rate_sqft = coalesce(largest_setback_rate_sqft_tmk, largest_setback_rate_sqft),
    largest_slrces_sqft = coalesce(largest_slrces_sqft_tmk, largest_slrces_sqft)
  ) %>%
  select(-ends_with("_tmk"))



# all parcels in depth-based and/or rate-based setback 
parcelshp <- parcelshp %>%
  left_join(setbacklot, by = "PARID")

#parcels within setback: n_setback_over50 >= 1 OR n_buildings==0 & LD_SETBK_F > 0
assessors_setback <- parcelshp %>% 
  filter(n_setback_over50 >= 1 | 
           (n_buildings == 0 & LD_SETBK_F > 0) | 
           (is.na(n_buildings) & LD_SETBK_F > 0))
setback_parid <- unique(assessors_setback$PARID)
setback_tmk <- unique(assessors_setback$COTMK)
assessors_setback <- assessors_setback[!duplicated(assessors_setback$PARID), ]



#calculate buildable area

#average width
# assessors_setback <- assessors_setback %>%
#   left_join(setbacklot, by = "PARID")
assessors_setback$AVE_WID_FT <- assessors_setback$PARCEL_SQFT / assessors_setback$AVE_DEP_FT
#buildable depth: AVE_DEP_FT - LD_SETBK_F - 10' 
assessors_setback$buildable_depth <- pmax(0, assessors_setback$AVE_DEP_FT - assessors_setback$LD_SETBK_F - 10)
#buildable width: AVE_WID_FT-2*5
#buildable area: buildable_depth * buildable_width
assessors_setback$buildable_area <- assessors_setback$buildable_depth * pmax(0,assessors_setback$AVE_WID_FT-2*5)
#building area lost: largest_bldg_sqft - buildable_area
assessors_setback$bldg_area_lost <- assessors_setback$largest_bldg_sqft - assessors_setback$buildable_area
#building area % lost: (largest_bldg_sqft - buildable_area)/largest_bldg_sqft
assessors_setback$bldg_area_lost_pct <- (assessors_setback$largest_bldg_sqft - assessors_setback$buildable_area)/assessors_setback$largest_bldg_sqft



#parcels within rate-based setback
assessors_rate <- assessors_setback %>% 
  filter(n_setback_rate_over50 >= 1 | 
           (n_buildings == 0 & setback_rate_sqft > 0) | 
           (is.na(n_buildings) & setback_rate_sqft > 0))
rate_parid <- unique(assessors_rate$PARID)
rate_tmk <- unique(assessors_rate$COTMK)


# parcels within lot depth-based setback
assessors_depth <- assessors_setback %>% 
  filter(n_setback_depth_over50 >= 1 | 
           (n_buildings == 0 & setback_depth_sqft > 0) | 
           (is.na(n_buildings) & setback_depth_sqft > 0))
depth_parid <- unique(assessors_depth$PARID)
depth_tmk <- unique(assessors_depth$COTMK)







# all parcels in CE
assessors_ce <- parcelshp %>% 
  filter(n_slrces_over50 > 0 |
           (n_buildings == 0 & slrce_sqft > 0) |
           (is.na(n_buildings) & slrce_sqft > 0))
CE_parid <- unique(assessors_ce$PARID)
CE_tmk <- unique(assessors_ce$COTMK)




# non coastal / inland parcels (not in setback or CE)
assessors_inland <- parcelshp %>%
  filter(!COTMK %in% union(CE_tmk, setback_tmk))
inland_tmk <- unique(assessors_inland$COTMK)
inland_parid<- unique(assessors_inland$PARID)


#all ag parcels 
assessors_ag <- parcelshp %>%
  filter(TAXCLASS25 == "Agricultural")
ag_tmk <- unique(assessors_ag$COTMK)
ag_parid<- unique(assessors_ag$PARID)

IALag_shp <- st_read(IALag_folder)
IALag_shp <- st_transform(IALag_shp, st_crs(assessors_ag))
assessors_ag$IAL <- as.integer(lengths(st_intersects(assessors_ag, IALag_shp)) > 0)



#ag CPR's
assessors_agCPR <- assessors_ag %>%
  filter(!is.na(CPR_UNIT))
agCPR_tmk <- unique(assessors_agCPR$COTMK)
agCPR_parid<- unique(assessors_agCPR$PARID)








# all parcels on kauai

# total value of parcels 
sum(parcelshp$MKTTOT25)
# total number of parcels 
length(unique(parcelshp$COTMK,na.rm=T))
#total unique tmk12's
length(unique(parcelshp$PARID,na.rm=T))
# total number of CPR's 
parcelshp %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd 
parcelshp %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()






#open kauai assessors data 2025
#pre-processed to include SLRXA data (by Kammie)

# Import Assessor's data and Assign data frame.  make sure TMK column cell is not in scientific notation
assessorsshp <- st_read(assessors_file)
assessors_ceshp <- st_read(noncprshpfolder,layer=noncprshplayer) 
assessorsshpcpr <- st_read(cprshpfolder,layer=cprshplayer)
assessorscpr <- as.data.frame(assessorsshpcpr)
assessors_hazard <- as.data.frame(assessors_ceshp)
assessors <- as.data.frame(assessorsshp)

#column definitions:
#COTMK = TMK8
#CPR_UNIT = CPR number. blank if no CPR
#PARID = TMK12 (alternatively use PARTXT which is a text character version of this column)
#APRTOTMKT = MKTTOT25 = Appraised Total Market Value (use for total value)
#ASMTTOT = ASSDTOT25 = Total Property Assessed Value (use for tax loss: (Assessed Value-Exemptions) x Tax Rate = Taxes)
#TOTEXEMPT = TOTEXEMPT_ = Total Property Exemption
#NETTAXABLE = NETTAXAB_3 = Total Net Taxable Value (MKTTOT25 - TOTEXEMPT)
#APRLANDMKT = MKTLAND25 = Appraised Land Market Value
#APRBLDGMKT = MKTBLDG25 = Appraised Building Market Value
#ASMTLAND = ASSDLAND25 = Land Assessed Value
#ASMTBLDG = ASSDBLDG25 = Building Assessed Value
#CLASS = use for identifying study site
#TAXCLASS = TAXCLASS25 = use for calculating taxes

#join the dataframes. in ArcGIS, use the building-TMK-join method1 "centroid within" for non-CPR, method2 "intersect" for CPR units. 
#if there are < 4 CPR for a COTMK, these are false CPR and should treat them as non-CPR
common_cols <- intersect(names(assessorscpr), names(assessors_hazard)) # Keep only common columns in both datasets
assessorscpr <- assessorscpr[, common_cols]
assessors_hazard <- assessors_hazard[, common_cols]
assessorscpr <- assessorscpr[!is.na(assessorscpr$CPR_UNIT),] #remove nonCPR from CPR df
falseCPR <- assessorscpr %>% #find the false CPRs that are basically really just split parcels and not apartments
  group_by(COTMK) %>%
  filter(n_distinct(PARID) < 4) %>%
  pull(PARID) %>%
  unique()
assessors_falseCPR <- assessors_hazard %>% #create a df to store the false CPR
  filter(PARID %in% falseCPR) 
assessors_falseCPR <- assessors_falseCPR %>% mutate(CPR_UNIT = NA)
assessorscpr <- assessorscpr %>% # remove false CPRs from CPR df 
  filter(!PARID %in% falseCPR)
assessors_hazard <- assessors_hazard[is.na(assessors_hazard$CPR_UNIT) & !assessors_hazard$PARID %in% falseCPR & 
                               !assessors_hazard$PARID %in% assessorscpr,] #remove CPR and falseCPR from nonCPR df
assessors_hazard <- rbind(assessors_hazard,assessors_falseCPR) #join nonCPR and falseCPR df together


# if there are >1 buildings on a single non-CPR'd parcel, keep only the row with the most makai building that is >300sqft
assessors_hazard <- assessors_hazard %>%
  group_by(PARID) %>%
  mutate(buildings = n()) %>%
  filter(
    buildings == 1 | 
      (buildings > 1 & 
         if(any(GIS_SQFT > 300)) {
           NEAR_VEG == min(NEAR_VEG[GIS_SQFT > 300])
         } else {
           NEAR_VEG == min(NEAR_VEG)
         })
  ) %>%
  slice(1) %>%  # keep only the first row if there are ties
  ungroup()

# add excluded CPR's back into assessorscpr / assessors_hazard
cpr_tmk <- unique(assessorscpr$COTMK)
missing_parcels <- assessors %>%
  filter(COTMK %in% cpr_tmk) %>%  # Only COTMKs that exist in assessorscpr
  filter(!PARID %in% assessorscpr$PARID)  # Only PARIDs not already in assessorscpr
assessorscpr <- bind_rows(assessorscpr, missing_parcels)

ncpr_tmk <- unique(assessors_hazard$COTMK)
missing_parcels <- assessors %>%
  filter(COTMK %in% ncpr_tmk) %>%  # Only COTMKs that exist in assessors_hazard
  filter(!PARID %in% assessors_hazard$PARID)  # Only PARIDs not already in assessors_hazard
missing_parcels <- missing_parcels %>% select(-geometry)
assessors_hazard <- bind_rows(assessors_hazard, missing_parcels)
assessors_hazard$CPR_units_total <- 0 

#join with setback data
assessors_hazard <- left_join(assessors_hazard, setbacklot, by = c("PARID"))
assessorscpr <- left_join(assessorscpr, setbacklot, by = c("PARID"))

# add summary columns to COTMK groups
# Apply to assessorscpr dataset only
assessorscpr <- assessorscpr %>%
  group_by(COTMK) %>%
  mutate(
    CPR_units_total = n(), # Count of CPR units in this COTMK group
    buildings = n_distinct(TARGET_FID, na.rm = TRUE), # Count unique building IDs in this COTMK group
    # Find most common BuildingID at COTMK level
    most_common_fid = {
      valid_fids <- TARGET_FID[!is.na(TARGET_FID)]
      if(length(valid_fids) == 0) {
        NA_real_
      } else if(length(valid_fids) == 1) {
        valid_fids[1]
      } else {
        # Count occurrences at COTMK level
        fid_counts <- table(valid_fids)
        max_count <- max(fid_counts,na.rm=T)
        most_common_fids <- as.numeric(names(fid_counts)[fid_counts == max_count])
        if(length(most_common_fids) == 1) {
          most_common_fids[1]
        } else {
          # Among tied buildings, keep the most makai (minimum NEAR_VEG)
          current_data <- data.frame(TARGET_FID = TARGET_FID, NEAR_VEG = NEAR_VEG)
          tied_subset <- current_data[current_data$TARGET_FID %in% most_common_fids & !is.na(current_data$TARGET_FID), ]
          if(nrow(tied_subset) > 0) {
            tied_subset$TARGET_FID[which.min(tied_subset$NEAR_VEG)]
          } else {
            most_common_fids[1]
          }
        }
      }
    }
  ) %>%
  ungroup() %>%
  # Now group by PARID to keep best building for each PARID
  group_by(PARID) %>%
  filter(
    # keep the most common building if this PARID has it
    if(any(TARGET_FID == first(most_common_fid[!is.na(most_common_fid)]), na.rm = TRUE)) {
      TARGET_FID == first(most_common_fid[!is.na(most_common_fid)])
    } else {
      # If this PARID doesn't have the most common building, keep the most makai building
      # Handle case where all NEAR_VEG values are NA
      if(all(is.na(NEAR_VEG))) {
        row_number() == 1  # Just keep first row if no NEAR_VEG data
      } else {
        NEAR_VEG == min(NEAR_VEG, na.rm = TRUE)
      }
    }
  ) %>%
  slice(1) %>%  # Safety net to ensure one row per PARID
  ungroup() %>%
  # Remove the temporary column
  select(-most_common_fid)

keep_cols <- c("PARID","NEAR_VEG",
               "NEAR_CE05","NEAR_CE11","NEAR_CE20","NEAR_CE32",
               "NEAR_PF05","NEAR_PF11","NEAR_PF20","NEAR_PF32",
               "NEAR_WF05","NEAR_WF11","NEAR_WF20","NEAR_WF32",
               "NEAR_XA05","NEAR_XA11","NEAR_XA20","NEAR_XA32",
               'area_og','SA_CE05','SA_CE11','SA_CE20','SA_CE32',
               'SA_WF05','SA_WF11','SA_WF20','SA_WF32',
               'SA_XA05','SA_XA11','SA_XA20','SA_XA32',
               'SA_PF05','SA_PF11','SA_PF20','SA_PF32',
               'CPR_units_total')
assessors_hazard <- assessors_hazard[, keep_cols]
assessorscpr <- assessorscpr[, keep_cols]

#join all dataframes together - falseCPR, nonCPR, and CPR
assessors_hazard <- assessors_hazard[!assessors_hazard$PARID %in% assessorscpr$PARID, ] #remove duplicates before joining
assessors_hazard <- rbind(assessors_hazard,assessorscpr) 


assessors_hazard <- assessors_hazard %>%
  mutate(
    pct_ce05 = SA_CE05 / area_og,
    pct_ce11 = SA_CE11 / area_og,
    pct_ce20 = SA_CE20 / area_og,
    pct_ce32 = SA_CE32 / area_og,  
    pct_wf05 = SA_WF05 / area_og,
    pct_wf11 = SA_WF11 / area_og,
    pct_wf20 = SA_WF20 / area_og,
    pct_wf32 = SA_WF32 / area_og,
  )
















#open black knight dataset
# data covers 1994 - 9/2023

## Load Deed 
blackknight <- fread(BKdeed_file)
blackknight <- blackknight[,c(1,2,5,15,20,38,39,57,58,74, 91:92, 93, 94, 103,104, 117, 120, 106,37, 63:70, 95)]
blackknight$`BKI Distinct Property ID (DPID)` <- as.character(blackknight$`BKI Distinct Property ID (DPID)`)
#ED <- blackknight %>% filter(`Document Type Code` == "ED" | `Document Type Code` == "CD" | `Document Type Code` == "TA")
#colnames(blackknight)
blackknight <- blackknight[!is.na(blackknight$`BKI Distinct Property ID (DPID)`)] #remove rows with NA (there are 17334 NA rows - 1% of dataset)

## Load Assessment 
assmt <- fread(BKassessment_file)
#colnames(assmt)
assmt <- assmt[,c(1:3, 9, 19:21, 72:74, 78, 80,85, 151, 248)]
assmt$`BKI Distinct Property ID (DPID)` <- as.character(assmt$`BKI Distinct Property ID (DPID)`)
assmt <- assmt %>% distinct(`BKI Distinct Property ID (DPID)`, `Assessor’s Parcel Number (APN, PIN)`, .keep_all = TRUE)
assmt <- assmt %>% select(-c("FIPS Code (State/County)", "Property Zip Code", "BKFS Internal PID"))

# BKID = unique identifier for each transaction
# APN = TMK ... ranges in length from 8 to 13 characters: county prefix + TMK8 + optional 4 CPR numbers

# TMK first digit county: 1=Oahu, 2=Maui, 3=Hawaii, 4=Kauai.
#data prep: append '2' to Maui TMK's 

# Merge Data
bk <- left_join(blackknight, assmt, by = c("BKI Distinct Property ID (DPID)", "Assessor’s Parcel Number (APN, PIN)"))
rm(assmt, blackknight)

#fix column names to be more R-friendly
# Create new R-friendly column names
names(bk) <- c("fips_code", "property_address", "property_zip", "recording_date", "apn",
  "buyer_mail_state", "buyer_mail_zip", "contract_date", "sales_price", "bkfs_pid",
  "assessor_land_use", "residential_indicator", "construction_loan", "inter_family", "seller_mail_state",
  "seller_mail_zip", "mean_sale_price", "property_type", "bki_dpid", "buyer_mail_city",
  "concurrent_td_lender_name", "concurrent_td_lender_type", "concurrent_td_loan_amount", "concurrent_td_loan_type", "concurrent_td_financing_type",
  "concurrent_td_interest_rate", "concurrent_td_due_date", "concurrent_2nd_td_loan_amount", "cash_purchase", "latitude",
  "longitude", "census_tract", "county_land_use_desc", "county_land_use_code", "zoning",
  "bldg_area", "year_built","n_bedrooms", "lot_size_sqft")

#create TMK column remove dashes in APN
bk$TMK <- gsub("-", "", bk$apn)

#keep only kauai transactions
bk_kauai <- bk %>%
  filter(
    substr(TMK, 1, 1) == "4" & #starting with 4 indicates kauai 
      nchar(TMK) %in% c(9, 13) # mauai transactions are missing a digit so they are 8 or 12 digits. so we want just 9 or 13 digit transactions
  )
bk_kauai$PARID <- as.numeric(ifelse(
  nchar(substr(as.character(bk_kauai$TMK), 2, nchar(as.character(bk_kauai$TMK)))) == 8,
  paste0(substr(as.character(bk_kauai$TMK), 2, nchar(as.character(bk_kauai$TMK))), "0000"),
  substr(as.character(bk_kauai$TMK), 2, nchar(as.character(bk_kauai$TMK)))
))









#inflate sales prices to $2025
present_year <- 2025
# Create SFH price index from Kauai median home prices https://data.uhero.hawaii.edu/#/search?start=1994-01-01&id=11&data_list_id=29&view=table&geo=KAU&freq=A
sfh_price_index <- data.frame(
  year = 1994:2024,
  price_index = c(
    246.2, 264.6, 220.6, 231.7, 233.1, 236.8, 259, 286.5, 335, 367.6, 
    499.5, 632.3, 694.8, 654.1, 607.4, 489.1, 493.6, 474.1, 475.9, 520.9, 
    547.2, 631.3, 628.5, 656.2, 708.8, 668.8, 792.3, 1133.80, 1214.10, 
    1200.80, 1343.90
  ) * 1000  
)

# Estimate 2025 SFH price index (using 2024 value)
sfh_index_2025 <- 1343.90 * 1000
sfh_price_index <- rbind(sfh_price_index, 
  data.frame(year = 2025, price_index = sfh_index_2025)
)

# Adjusted Price = Nominal Price × (CPI_2025 / CPI_sale_year)
adjust_to_2025 <- function(sale_year) {
  if(is.na(sale_year)) return(NA)
  # Get SFH price index for the sale year
  sfh_index_sale <- sfh_price_index$price_index[sfh_price_index$year == sale_year]
  if(length(sfh_index_sale) == 0) return(NA)
  # Deflator = SFH_Index_targetyr / SFH_Index_sale
  deflator <- sfh_index_2025 / sfh_index_sale
  return(deflator)
}


bk_kauai <- bk_kauai %>%
  mutate(
    date_sale = as.Date(as.character(contract_date), format = "%Y%m%d"),
    year_sale = as.numeric(format(date_sale, "%Y")),
    deflator = sapply(year_sale, adjust_to_2025),
    sales_price_2025 = sales_price * deflator
  )





#join with assessors
bk_assessors <- merge(bk_kauai, parcelshp, by = "PARID", all.x = TRUE)

bk_CE <- bk_assessors[bk_assessors$PARID %in% CE_parid, ]
bk_rate <- bk_assessors[bk_assessors$PARID %in% rate_parid, ]
bk_depth <- bk_assessors[bk_assessors$PARID %in% depth_parid, ]
bk_setback <- bk_assessors[bk_assessors$PARID %in% setback_parid, ]
#bk_slrxa <- bk_assessors[bk_assessors$PARID %in% slrxa_parid, ]
bk_inland <- bk_assessors[bk_assessors$PARID %in% inland_parid, ]






write.csv(st_drop_geometry(assessors_ce), "assessors_ce.csv", row.names = FALSE)
write.csv(st_drop_geometry(assessors_rate), "assessors_rate.csv", row.names = FALSE)
write.csv(st_drop_geometry(assessors_depth), "assessors_depth.csv", row.names = FALSE)
write.csv(st_drop_geometry(assessors_setback), "assessors_setback.csv", row.names = FALSE)
write.csv(st_drop_geometry(assessors_inland), "assessors_inland.csv", row.names = FALSE)
write.csv(st_drop_geometry(assessors_ag), "assessors_ag.csv", row.names = FALSE)
write.csv(st_drop_geometry(bk_assessors), "bk_assessors.csv", row.names = FALSE)
write.csv(st_drop_geometry(bldgftprntshp), "bldgftprntshp.csv", row.names = FALSE)
write.csv(st_drop_geometry(parcelshp), "parcelshp.csv", row.names = FALSE)

