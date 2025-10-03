library(data.table)
library(foreign)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)


source('config_dir.R')

# Disable scientific notation
options(scipen = 999)


#prep datasets






# open lot-based setback dataset
setbackshp <- st_read(setback_folder,layer=setbackdepth_layer)
setbacklot <- as.data.frame(setbackshp)

#important columns
# TMK 
# LD_SETBK_F = lot depth setback distance in feet for each parcel
# AVE_DEP_FT = average lot depth in feet

setbacklot <- setbacklot[, c("PARID","LD_SETBK_F","AVE_DEP_FT")]
setbacklot <- setbacklot[setbacklot$LD_SETBK_F > 0,]

#remove duplicates 
setbacklot <- setbacklot %>% distinct(PARID, .keep_all = TRUE)









#open kauai assessors data 2025
#pre-processed to include SLRXA data

# Import Assessor's data and Assign data frame.  make sure TMK column cell is not in scientific notation
assessorsshp <- st_read(assessors_file)
assessors_ceshp <- st_read(noncprshpfolder,layer=noncprshplayer) 
assessorsshpcpr <- st_read(cprshpfolder,layer=cprshplayer)
assessorscpr <- as.data.frame(assessorsshpcpr)
assessors_ce <- as.data.frame(assessors_ceshp)
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
common_cols <- intersect(names(assessorscpr), names(assessors_ce)) # Keep only common columns in both datasets
assessorscpr <- assessorscpr[, common_cols]
assessors_ce <- assessors_ce[, common_cols]
assessorscpr <- assessorscpr[!is.na(assessorscpr$CPR_UNIT),] #remove nonCPR from CPR df
falseCPR <- assessorscpr %>% #find the false CPRs that are basically really just split parcels and not apartments
  group_by(COTMK) %>%
  filter(n_distinct(PARID) < 4) %>%
  pull(PARID) %>%
  unique()
assessors_falseCPR <- assessors_ce %>% #create a df to store the false CPR
  filter(PARID %in% falseCPR) 
assessors_falseCPR <- assessors_falseCPR %>% mutate(CPR_UNIT = NA)
assessorscpr <- assessorscpr %>% # remove false CPRs from CPR df 
  filter(!PARID %in% falseCPR)
assessors_ce <- assessors_ce[is.na(assessors_ce$CPR_UNIT) & !assessors_ce$PARID %in% falseCPR & 
                               !assessors_ce$PARID %in% assessorscpr,] #remove CPR and falseCPR from nonCPR df
assessors_ce <- rbind(assessors_ce,assessors_falseCPR) #join nonCPR and falseCPR df together


# if there are >1 buildings on a single non-CPR'd parcel, keep only the row with the most makai building that is >300sqft
assessors_ce <- assessors_ce %>%
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

# add excluded CPR's back into assessorscpr / assessors_ce
cpr_tmk <- unique(assessorscpr$COTMK)
missing_parcels <- assessors %>%
  filter(COTMK %in% cpr_tmk) %>%  # Only COTMKs that exist in assessorscpr
  filter(!PARID %in% assessorscpr$PARID)  # Only PARIDs not already in assessorscpr
assessorscpr <- bind_rows(assessorscpr, missing_parcels)

ncpr_tmk <- unique(assessors_ce$COTMK)
missing_parcels <- assessors %>%
  filter(COTMK %in% ncpr_tmk) %>%  # Only COTMKs that exist in assessors_ce
  filter(!PARID %in% assessors_ce$PARID)  # Only PARIDs not already in assessors_ce
missing_parcels <- missing_parcels %>% select(-geometry)
assessors_ce <- bind_rows(assessors_ce, missing_parcels)
assessors_ce$CPR_units_total <- 0 

#join with setback data
assessors_ce <- left_join(assessors_ce, setbacklot, by = c("PARID"))
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

keep_cols <- c("PARID","COTMK","CPR_UNIT","TAXCLASS25",
               "ahupuaa","moku","NewB",
               "MKTBLDG25","ASSDBLDG25","MKTLAND25","ASSDLAND25",
               "MKTTOT25","ASSDTOT25","TOTEXEMPT_","NETTAXAB_3",
               "TARGET_FID","GIS_SQFT","NEAR_VEG",
               "NEAR_CE05","NEAR_CE11","NEAR_CE20","NEAR_CE32",
               "NEAR_PF05","NEAR_PF11","NEAR_PF20","NEAR_PF32",
               "NEAR_WF05","NEAR_WF11","NEAR_WF20","NEAR_WF32",
               "NEAR_XA05","NEAR_XA11","NEAR_XA20","NEAR_XA32",
               'area_og','SA_CE05','SA_CE11','SA_CE20','SA_CE32',
               'SA_WF05','SA_WF11','SA_WF20','SA_WF32',
               'SA_XA05','SA_XA11','SA_XA20','SA_XA32',
               'SA_PF05','SA_PF11','SA_PF20','SA_PF32',
               'CPR_units_total','buildings',"LD_SETBK_F","AVE_DEP_FT")
assessors_ce <- assessors_ce[, keep_cols]
assessorscpr <- assessorscpr[, keep_cols]

#join all dataframes together - falseCPR, nonCPR, and CPR
assessors_ce <- assessors_ce[!assessors_ce$PARID %in% assessorscpr$PARID, ] #remove duplicates before joining
assessors_ce <- rbind(assessors_ce,assessorscpr) 

# rename columns
assessors_ce <- assessors_ce %>%
  rename(BuildingID = TARGET_FID,BLDG_SQFT = GIS_SQFT,PARCEL_SQFT = area_og)

assessors_ce <- assessors_ce[assessors_ce$PARID>0,]

#as an extra precaution, make sure there are no duplicates of TMK's that happen to have buildings equally far from veg
assessors_ce = assessors_ce[!duplicated(assessors_ce$PARID),] 

# only those within CE
assessors_ce <- assessors_ce[assessors_ce$SA_CE32 > 0 | assessors_ce$NEAR_CE32 == 0| 
                               is.na(assessors_ce$SA_CE32) | is.na(assessors_ce$NEAR_CE32),]

CE_tmk <- unique(assessors_ce$COTMK)
CE_parid <- unique(assessors_ce$PARID)







#parcels within rate-based setback
setbackrateshp <- st_read(setback_folder,layer=setbackrate_layer)
parcelshp <- st_read(kauaiparcel_folder,layer=kauaiparcel_layer)
setbackrateshp <- st_transform(setbackrateshp, st_crs(parcelshp))
parcelshp <- st_make_valid(parcelshp)
setbackrateshp <- st_make_valid(setbackrateshp)
# Calculate parcel areas
parcelshp$parcel_area <- st_area(parcelshp)
# calculate rate-based coverage on parcels
ratebased_cover <- st_intersection(parcelshp, setbackrateshp)
ratebased_cover <- ratebased_cover %>%
  mutate(ratebased_cover = st_area(geometry)) %>% 
  st_drop_geometry() %>%
  group_by(PARID) %>%
  summarise(ratebased_cover = sum(ratebased_cover))
parcelshp <- parcelshp %>%
  left_join(ratebased_cover, by = "PARID") %>%
  mutate(
    ratebased_cover = ifelse(is.na(ratebased_cover), units::set_units(0, "m^2"), ratebased_cover),
    pct_ratesetback = as.numeric(ratebased_cover / parcel_area) * 100
  )
assessors_rate <- parcelshp[parcelshp$pct_ratesetback > 0,]
rate_parid <- unique(assessors_rate$PARID)




# parcels within lot depth-based setback
assessors <- left_join(assessors, setbacklot, by = c("PARID"))
assessors <- assessors[!duplicated(assessors$PARID), ]
depth_tmk <- unique(assessors$COTMK[!is.na(assessors$AVE_DEP_FT)])
assessors_depth <- assessors[assessors$COTMK %in% depth_tmk, ]
depth_parid <- unique(assessors_depth$PARID)








# all parcels in depth-based and/or rate-based setback 
assessors_setback <- assessors %>% 
  filter(COTMK %in% union(CE_tmk,depth_tmk))
setback_parid <- unique(assessors_setback$PARID)










#open slrxa data
slrxashp <- st_read(slrxa_folder, layer=slrxa_layer)
slrxashp <- st_transform(slrxashp, st_crs(assessorsshp))
sf_use_s2(FALSE) #speed up calculations - use planar geometry rather than spherical
assessors_slrxa_shp <- st_intersection(assessorsshp, slrxashp) %>%  
  mutate(slrxa_area = as.numeric(st_area(.)))

assessors_slrxa <- as.data.frame(assessors_slrxa_shp)
assessors_slrxa <- assessors_slrxa[!duplicated(assessors_slrxa$PARID), ]

slrxa_tmk <- unique(assessors_slrxa$COTMK)
slrxa_parid <- unique(assessors_slrxa$PARID)

#add missing CPR units 
missing_parcels <- assessors %>%
  filter(COTMK %in% slrxa_tmk) %>%  # Only COTMKs that exist in assessors_slrxa
  filter(!PARID %in% assessors_slrxa$PARID)  # Only PARIDs not already in assessors_slrxa
assessors_slrxa <- bind_rows(assessors_slrxa, missing_parcels)









# non coastal parcels
assessors_inland <- assessors[!assessors$COTMK %in% slrxa_tmk & 
                                  !assessors$COTMK %in% union(CE_tmk, depth_tmk), ]
inland_tmk <- unique(assessors_inland$COTMK)
inland_parid<- unique(assessors_inland$PARID)








# all parcels on kauai

# total value of parcels 
sum(assessors$MKTTOT25)
# total number of parcels 
length(unique(assessors$COTMK,na.rm=T))
#total unique tmk12's
length(unique(assessors$PARID,na.rm=T))
# total number of CPR's 
assessors %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup() %>%
  filter(cpr == TRUE) %>%
  nrow() 
# total number of parcels CPR'd in 
assessors %>%
  group_by(COTMK) %>%
  summarise(parid_count = n(), .groups='drop') %>%
  filter(parid_count > 5) %>%
  nrow()












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
assmt <- assmt[,c(1:3, 9, 19:21, 72:74, 78, 80, 151, 248)]
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
  "bldg_area", "year_built","lot_size_sqft")

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


#join with assessors
bk_assessors <- merge(bk_kauai, assessors, by = "PARID", all.x = TRUE)

bk_CE <- bk_assessors[bk_assessors$PARID %in% CE_parid, ]
bk_depth <- bk_assessors[bk_assessors$PARID %in% depth_parid, ]
bk_setback <- bk_assessors[bk_assessors$PARID %in% setback_parid, ]
bk_slrxa <- bk_assessors[bk_assessors$PARID %in% slrxa_parid, ]
bk_inland <- bk_assessors[bk_assessors$PARID %in% inland_parid, ]














