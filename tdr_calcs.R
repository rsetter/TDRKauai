library(data.table)
library(mgcv)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)


source('config_dir.R')

# Disable scientific notation
options(scipen = 999)

#set costs
demo_cost <- 8778
construction_cost <- 499158 
agCPR_bldg_sqft <- 3781 #average agCPR building


#open datasets
assessors_setback <- fread("assessors_setback.csv")
assessors_ag <- fread("assessors_ag.csv")
assessors_agCPR <- assessors_ag %>%
  filter(!is.na(CPR_UNIT))
bk_assessors <- fread("bk_assessors.csv")


# sending area
# residential non-cpr'd parcels in setback with <1000 sqft buildable area
residential <- c("Owner-Occupied", "Vacation Rental","Non-Owner-Occupied Residential","Owner-Occupied Mixed Use")  
assessors_setback_res <- assessors_setback %>%
  filter(TAXCLASS25 %in% residential)
assessors_setback_res_noncpr_nobuild_nogov <- assessors_setback_res %>%
  filter(!is.na(buildable_area), 
         buildable_area < 1000,
         PARCEL_SQFT > 80,
         !OWN1 %in% c("STATE OF HAWAII","COUNTY OF KAUAI","HAWAIIAN HOME LANDS"),
         !RESTRICT3 %in% c("R:NON-TAXABLE RIGHT OF WAY"))
bk_assessors_setback_res_noncpr_nobuild_nogov <- bk_assessors %>%
  filter(PARID %in% assessors_setback_res_noncpr_nobuild_nogov$PARID)
bk_assessors_setback_res_noncpr_nobuild_nogov %>%
  filter(sales_price_2025 >= 50000 & sales_price_2025 <= 30000000) %>%
  summarise(
    mean = mean(sales_price_2025, na.rm = TRUE),
    median = median(sales_price_2025, na.rm = TRUE),
    sd = sd(sales_price_2025, na.rm = TRUE),
    n = n()
  )
#calibrate 
sales_calibration_send <- bk_assessors_setback_res_noncpr_nobuild_nogov %>%
  filter(!is.na(MKTTOT25) & !is.na(sales_price_2025) & 
           sales_price_2025 > 50000 & sales_price_2025 < 30000000 & 
           MKTTOT25 < 30000000)
calibration_model_send <- lm(sales_price_2025 ~ MKTTOT25, data = sales_calibration_send)
summary(calibration_model_send)
bk_assessors_setback_res_noncpr_nobuild_nogov <- bk_assessors_setback_res_noncpr_nobuild_nogov %>%
  mutate(# Linear model prediction
    MKTTOT25_calibrated = predict(calibration_model_send, newdata = .))
assessors_setback_res_noncpr_nobuild_nogov <- assessors_setback_res_noncpr_nobuild_nogov %>%
  mutate(# Linear model prediction
    MKTTOT25_calibrated = predict(calibration_model_send, newdata = .))
assessors_setback_res_noncpr_nobuild_nogov[assessors_setback_res_noncpr_nobuild_nogov$TMK == 435002029,"n_buildings"] <- 1
assessors_setback_res_noncpr_nobuild_nogov %>%
  summarise(
    mean = mean(MKTTOT25, na.rm = TRUE),
    median = median(MKTTOT25, na.rm = TRUE),
    sd = sd(MKTTOT25, na.rm = TRUE),
    n = n()
  )
n_sending_parcels <- nrow(assessors_setback_res_noncpr_nobuild_nogov)
n_sending_vacant <- sum(is.na(assessors_setback_res_noncpr_nobuild_nogov$n_buildings))
n_sending_bldg <- sum(assessors_setback_res_noncpr_nobuild_nogov$n_buildings,na.rm=T)
sending_total_value <- sum(assessors_setback_res_noncpr_nobuild_nogov$MKTTOT25_calibrated,na.rm=T)
sending_demo <- demo_cost*n_sending_bldg
sending_tdrcredit_total <- sending_total_value+sending_demo
sending_tdrcredit_per_unit <- sending_tdrcredit_total / n_sending_parcels




#receiving area
# ag non-cpr'd parcels

# Estimate 2025 SFH price index (using 2024 value)
sfh_price_index <- data.frame(
  year = 1994:2024,
  price_index = c(
    246.2, 264.6, 220.6, 231.7, 233.1, 236.8, 259, 286.5, 335, 367.6, 
    499.5, 632.3, 694.8, 654.1, 607.4, 489.1, 493.6, 474.1, 475.9, 520.9, 
    547.2, 631.3, 628.5, 656.2, 708.8, 668.8, 792.3, 1133.80, 1214.10, 
    1200.80, 1343.90
  ) * 1000  
)
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

ag_parid<- unique(assessors_ag$PARID)
agCPR_parid<- unique(assessors_agCPR$PARID)
bk_ag <- bk_assessors[bk_assessors$PARID %in% ag_parid, ]
bk_agCPR <- bk_assessors[bk_assessors$PARID %in% agCPR_parid, ]
bk_ag <- bk_ag %>%
  mutate(
    date_sale = as.Date(as.character(contract_date), format = "%Y%m%d"),
    year_sale = as.numeric(format(date_sale, "%Y")),
    deflator = sapply(year_sale, adjust_to_2025),
    sales_price_2025 = sales_price * deflator
  )
bk_agCPR <- bk_agCPR %>%
  mutate(
    date_sale = as.Date(as.character(contract_date), format = "%Y%m%d"),
    year_sale = as.numeric(format(date_sale, "%Y")),
    deflator = sapply(year_sale, adjust_to_2025),
    sales_price_2025 = sales_price * deflator
  )
bk_agCPR_non0_nonoutlier <- bk_agCPR %>%
  filter(!is.na(sales_price_2025) & sales_price_2025 > 50000 & sales_price_2025 < 50000000)

#estimate sale price per unit
mean(bk_agCPR_non0_nonoutlier$sales_price_2025) #mean sales price of a CPR
mean(assessors_agCPR$MKTTOT25) #mean sales price of a CPR
ag_bldg_sqft <- median(assessors_agCPR$largest_bldg_sqft,na.rm=T)



#exclude IAL ag parcels
assessors_ag_noIAL <- assessors_ag[assessors_ag$IAL==0,]
assessors_agCPR_noIAL <- assessors_agCPR[assessors_agCPR$IAL==0,]
assessors_ag_nonCPR_noIAL <- assessors_ag_noIAL %>% filter(!PARID %in% assessors_agCPR_noIAL$PARID)

# linear model based on acreage and dwelling sqft
ag_acredwel_mkt_model <- lm(MKTTOT25 ~ TAXACRES+largest_bldg_sqft, data = assessors_agCPR_noIAL)
summary(ag_acredwel_mkt_model)
predict_ag_acredwel <- function(acres,largest_bldg_sqft) {
  predict(ag_acredwel_mkt_model, newdata = data.frame(TAXACRES = acres,largest_bldg_sqft=largest_bldg_sqft))
}
predict_ag_acredwel(27,3781.272)

#calibrate model: actual sales price is higher than assessors data
#get predicted assessor values for parcels that actually sold
# for ag CPR parcels (parcels to be sold)
sales_calibration <- bk_agCPR %>%
  filter(!is.na(MKTTOT25) & !is.na(sales_price_2025) & PARID %in% assessors_agCPR_noIAL$PARID &
           sales_price_2025 > 50000 & sales_price_2025 < 30000000 & MKTTOT25 < 30000000)
# calibration_model <- lm(sales_price_2025 ~ MKTTOT25, data = sales_calibration)
# summary(calibration_model)
calibration_model_loglog <- lm(log(sales_price_2025) ~ log(MKTTOT25), data = sales_calibration)
summary(calibration_model_loglog)
#calibrated model
# Revenue per subdivided unit (predict multiple times)
predict_ag_acredwel_calib <- function(acres, largest_bldg_sqft) {
  # Stage 1: GAM predicts assessor value
  pred_mkt <- predict(ag_acredwel_mkt_model, 
                      newdata = data.frame(TAXACRES = acres, largest_bldg_sqft = largest_bldg_sqft))
  
  # Stage 2: calibrate that assessor value up to expected sales price
  log_sales_price <- predict(calibration_model_loglog, newdata = data.frame(MKTTOT25 = pred_mkt))
  
  # exponentiate to get back to dollar scale
  exp(log_sales_price)
}

#for ag non-CPR parcels (starting non-developed parcel)
sales_calibration_land <- bk_ag %>%
  filter(!is.na(MKTTOT25) & !is.na(sales_price_2025) & PARID %in% assessors_ag_nonCPR_noIAL$PARID &
           sales_price_2025 > 50000 & sales_price_2025 < 30000000 & MKTTOT25 < 30000000)
calibration_model_land <- lm(sales_price_2025 ~ MKTTOT25, data = sales_calibration_land)
summary(calibration_model_land)
calibration_model_loglog_land <- lm(log(sales_price_2025) ~ log(MKTTOT25), data = sales_calibration_land)
summary(calibration_model_loglog_land)
#calibrated model
# Land purchase cost (whole parcel, predict once)
predict_land_cost <- function(mkttot25) {
  predict(calibration_model_land, newdata = data.frame(MKTTOT25 = mkttot25))
}






# TDR FUNCTIONS


# Function to calculate dwellings based on parcel acreage (§8-8.2c)
calc_dwellings_per_parcel <- function(acres) {
  dwellings <- 1 + floor((acres - 1) / 3)
  min(dwellings, 5)  # Capped at 5
}

# function that returns subdivision structure and total dwellings (§8-8.3b)
calc_baseline_subdivision <- function(acres) {
  
  if (acres < 1) {
    # parcels <1 acre
    return(list(
      total_subdivisions = 0,
      total_dwellings = 1,
      subdivision_details = tibble(
        parcel_type = "<1 acre no subdivision",
        n_parcels = 1,
        acres_per_parcel = acres,
        dwellings_per_parcel = 1,
        total_dwellings = 1
      )
    ))
  }
  
  if (acres >= 1 && acres <= 10) {
    # (A) ≤10 acres: 1-acre minimum lots
    n_parcels <- floor(acres / 1)
    dwellings_per <- calc_dwellings_per_parcel(1)  # Always 1 dwelling
    
    return(list(
      total_subdivisions = n_parcels,
      total_dwellings = n_parcels * dwellings_per,
      subdivision_details = tibble(
        parcel_type = "1-acre lots",
        n_parcels = n_parcels,
        acres_per_parcel = 1,
        dwellings_per_parcel = dwellings_per,
        total_dwellings = n_parcels * dwellings_per
      )
    ))
  }
  
  if (acres > 10 && acres <= 20) {
    # (B) 10-20 acres: Up to 4 lots at 1-acre, remainder at 2-acre min
    n_1ac_lots <- 4
    remaining_acres <- acres - 4
    n_2ac_lots <- floor(remaining_acres / 2)
    
    dwellings_1ac <- calc_dwellings_per_parcel(1)  # 1 dwelling
    dwellings_2ac <- calc_dwellings_per_parcel(2)  # 1 dwelling
    
    return(list(
      total_subdivisions = n_1ac_lots + n_2ac_lots,
      total_dwellings = (n_1ac_lots * dwellings_1ac) + (n_2ac_lots * dwellings_2ac),
      subdivision_details = tibble(
        parcel_type = c("1-acre lots", "2-acre lots"),
        n_parcels = c(n_1ac_lots, n_2ac_lots),
        acres_per_parcel = c(1, 2),
        dwellings_per_parcel = c(dwellings_1ac, dwellings_2ac),
        total_dwellings = c(n_1ac_lots * dwellings_1ac, n_2ac_lots * dwellings_2ac)
      )
    ))
  }
  
  if (acres > 20 && acres <= 30) {
    # (C) 20-30 acres: Up to 4 lots at 1-acre, remainder at 3-acre min
    n_1ac_lots <- 4
    remaining_acres <- acres - 4
    n_3ac_lots <- floor(remaining_acres / 3)
    
    dwellings_1ac <- calc_dwellings_per_parcel(1)  # 1 dwelling
    dwellings_3ac <- calc_dwellings_per_parcel(3)  # 1 dwelling
    
    return(list(
      total_subdivisions = n_1ac_lots + n_3ac_lots,
      total_dwellings = (n_1ac_lots * dwellings_1ac) + (n_3ac_lots * dwellings_3ac),
      subdivision_details = tibble(
        parcel_type = c("1-acre lots", "3-acre lots"),
        n_parcels = c(n_1ac_lots, n_3ac_lots),
        acres_per_parcel = c(1, 3),
        dwellings_per_parcel = c(dwellings_1ac, dwellings_3ac),
        total_dwellings = c(n_1ac_lots * dwellings_1ac, n_3ac_lots * dwellings_3ac)
      )
    ))
  }
  
  if (acres > 30 && acres <= 50) {
    # (D) 30-50 acres: 5-acre minimum, no exceptions
    n_parcels <- floor(acres / 5)
    dwellings_per <- calc_dwellings_per_parcel(5)  # 2 dwellings
    
    return(list(
      total_subdivisions = n_parcels,
      total_dwellings = n_parcels * dwellings_per,
      subdivision_details = tibble(
        parcel_type = "5-acre lots",
        n_parcels = n_parcels,
        acres_per_parcel = 5,
        dwellings_per_parcel = dwellings_per,
        total_dwellings = n_parcels * dwellings_per
      )
    ))
  }
  
  if (acres > 50 && acres <= 300) {
    # (E) 50-300 acres: Max 10 parcels, 5-acre minimum
    n_parcels <- 10
    avg_acres <- acres / n_parcels
    dwellings_per <- calc_dwellings_per_parcel(avg_acres)
    
    return(list(
      total_subdivisions = n_parcels,
      total_dwellings = n_parcels * dwellings_per,
      subdivision_details = tibble(
        parcel_type = paste0(round(avg_acres, 1), "-acre avg lots"),
        n_parcels = n_parcels,
        acres_per_parcel = avg_acres,
        dwellings_per_parcel = dwellings_per,
        total_dwellings = n_parcels * dwellings_per
      )
    ))
  }
  
  if (acres > 300) {
    # (2) Parcels >300 acres - three distinct parts
    
    # Part A: First 75 acres → max 10 parcels (5-acre min)
    partA_n_parcels <- 10
    partA_avg_acres <- 75 / partA_n_parcels  # 7.5 acres
    partA_dwellings_per <- calc_dwellings_per_parcel(partA_avg_acres)  # 3 dwellings
    partA_total_dwellings <- partA_n_parcels * partA_dwellings_per
    
    # Part B: Additional 20% or 300 acres max → 25-acre min parcels
    partB_acres <- min(0.20 * acres, 300)
    partB_n_parcels <- floor(partB_acres / 25)
    partB_dwellings_per <- calc_dwellings_per_parcel(25)  # 5 dwellings (capped)
    partB_total_dwellings <- partB_n_parcels * partB_dwellings_per
    
    # Part C: Balance cannot be subdivided BUT still gets dwellings
    partC_acres <- acres - 75 - (partB_n_parcels * 25)  # Use actual acres used in Part B
    partC_dwellings <- calc_dwellings_per_parcel(partC_acres)  # 5 dwellings (capped)
    
    return(list(
      total_subdivisions = partA_n_parcels + partB_n_parcels + 1,  # +1 for Part C
      total_dwellings = partA_total_dwellings + partB_total_dwellings + partC_dwellings,
      subdivision_details = tibble(
        parcel_type = c("Part A (7.5-acre avg)", "Part B (25-acre)", "Part C (no subdivision)"),
        n_parcels = c(partA_n_parcels, partB_n_parcels, 1),
        acres_per_parcel = c(partA_avg_acres, 25, partC_acres),
        dwellings_per_parcel = c(partA_dwellings_per, partB_dwellings_per, partC_dwellings),
        total_dwellings = c(partA_total_dwellings, partB_total_dwellings, partC_dwellings)
      )
    ))
  }
}


## SINGLE PARCEL SCENARIO
# function for TDR ROI analysis for all possible receiving parcels, applied to single parcels
calculate_tdr_roi <- function(acres, land_cost, current_dwellings, tdr_credits_to_buy) {
  
  # Get baseline subdivision structure
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  max_allowed <- baseline_sub$total_dwellings
  
  # Calculate how many dwellings can be added
  dwellings_to_add <- max(0, max_allowed  - current_dwellings)
  baseline_dwellings <- current_dwellings + dwellings_to_add
  
  # TDR scenario
  tdr_dwellings <- current_dwellings + dwellings_to_add + tdr_credits_to_buy
  
  #extract to vectors 
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # Baseline costs & revenue
  baseline_construction <- dwellings_to_add * construction_cost 
  baseline_total_cost <- land_cost + baseline_construction
  # use  subdivision acreages
  # Each dwelling sold separately at (acres_per_parcel / dwellings_per_parcel)
  baseline_revenue <- sum(mapply(function(a, d, n) {
    n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
  }, d_acres, d_dwellings, d_nparcels))
  baseline_profit <- baseline_revenue - baseline_total_cost
  baseline_roi <- baseline_profit / baseline_total_cost
  
  # TDR costs & revenue
  # distribute extra dwellings proportionally across parcel types
  baseline_total_per_type <- d_nparcels * d_dwellings
  shares <- baseline_total_per_type / sum(baseline_total_per_type)
  extra <- round(shares * tdr_credits_to_buy)
  tdr_total_per_type <- baseline_total_per_type + extra
  tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
  tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
  tdr_revenue <- sum(mapply(function(a, d, n) {
    n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
  }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
  
  tdr_construction <- (dwellings_to_add + tdr_credits_to_buy) * construction_cost 
  tdr_credit_cost <- tdr_credits_to_buy * sending_tdrcredit_per_unit 
  tdr_total_cost <- land_cost + tdr_construction + tdr_credit_cost
  
  tdr_profit <- tdr_revenue - tdr_total_cost
  tdr_roi <- tdr_profit / tdr_total_cost
  
  tibble(
    baseline_subdivisions = baseline_sub$total_subdivisions,
    baseline_dwellings = baseline_dwellings,
    dwellings_to_add = dwellings_to_add,
    tdr_credits = tdr_credits_to_buy,
    tdr_dwellings = tdr_dwellings,
    
    # Baseline scenario
    baseline_land_cost = land_cost,
    baseline_construction_cost = baseline_construction,
    baseline_total_cost = baseline_total_cost,
    baseline_revenue = baseline_revenue,
    baseline_profit = baseline_profit,
    baseline_roi = baseline_roi,
    
    # TDR scenario
    tdr_land_cost = land_cost,
    tdr_construction_cost = tdr_construction,
    tdr_credit_cost = tdr_credit_cost,
    tdr_total_cost = tdr_total_cost,
    tdr_revenue = tdr_revenue,
    tdr_profit = tdr_profit,
    tdr_roi = tdr_roi,
    
    # Comparison
    roi_improvement = tdr_roi - baseline_roi,
    profit_improvement = tdr_profit - baseline_profit
  )
}



#SPLIT ALLOCATION SCENARIO
#split tdr credits across multiple receiving parcels
# function to Calculate marginal profit/roi for each parcel with each additional credit
calculate_marginal_profit <- function(acres, land_cost, current_dwellings, current_credits, bundle_size = 1) {
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  max_allowed <- baseline_sub$total_dwellings
  
  dwellings_to_add <- max(0, max_allowed - current_dwellings)
  
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # Current state (unchanged)
  current_total_dwellings <- current_dwellings + dwellings_to_add + current_credits
  current_construction <- (dwellings_to_add + current_credits) * construction_cost
  current_tdr_cost <- current_credits * sending_tdrcredit_per_unit
  current_total_cost <- land_cost + current_construction + current_tdr_cost
  
  current_revenue <- {
    if(current_credits == 0) {
      sum(mapply(function(a, d, n) {
        n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
      }, d_acres, d_dwellings, d_nparcels))
    } else {
      baseline_total_per_type <- d_nparcels * d_dwellings
      shares <- baseline_total_per_type / sum(baseline_total_per_type)
      extra <- round(shares * current_credits)
      current_total_per_type <- baseline_total_per_type + extra
      current_dwellings_per_parcel <- current_total_per_type / d_nparcels
      current_acres_per_dwelling <- d_acres / current_dwellings_per_parcel
      
      sum(mapply(function(a, d, n) {
        n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
      }, current_acres_per_dwelling, current_dwellings_per_parcel, d_nparcels))
    }
  }
  current_profit <- current_revenue - current_total_cost
  
  # With bundle_size more credits (CHANGED: +1 becomes +bundle_size)
  next_total_dwellings  <- current_dwellings + dwellings_to_add + current_credits + bundle_size
  next_construction <- (dwellings_to_add + current_credits + bundle_size) * construction_cost
  next_tdr_cost <- (current_credits + bundle_size) * sending_tdrcredit_per_unit
  next_total_cost <- land_cost + next_construction + next_tdr_cost
  
  # Calculate next revenue
  next_revenue <- {
    baseline_total_per_type <- d_nparcels * d_dwellings
    shares <- baseline_total_per_type / sum(baseline_total_per_type)
    extra <- round(shares * (current_credits + bundle_size))
    next_total_per_type <- baseline_total_per_type + extra
    next_dwellings_per_parcel <- next_total_per_type / d_nparcels
    next_acres_per_dwelling <- d_acres / next_dwellings_per_parcel
    
    sum(mapply(function(a, d, n) {
      n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
    }, next_acres_per_dwelling, next_dwellings_per_parcel, d_nparcels))
  }
  next_profit <- next_revenue - next_total_cost
  
  return(next_profit - current_profit)
}
calculate_marginal_roi <- function(acres, land_cost, current_dwellings, current_credits, bundle_size = 1) {
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  max_allowed <- baseline_sub$total_dwellings
  
  dwellings_to_add <- max(0, max_allowed - current_dwellings)
  
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # Current state
  current_total_dwellings <- current_dwellings + dwellings_to_add + current_credits
  current_construction <- (dwellings_to_add + current_credits) * construction_cost
  current_tdr_cost <- current_credits * sending_tdrcredit_per_unit
  current_total_cost <- land_cost + current_construction + current_tdr_cost
  
  current_revenue <- {
    if(current_credits == 0) {
      sum(mapply(function(a, d, n) {
        n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
      }, d_acres, d_dwellings, d_nparcels))
    } else {
      baseline_total_per_type <- d_nparcels * d_dwellings
      shares <- baseline_total_per_type / sum(baseline_total_per_type)
      extra <- round(shares * current_credits)
      current_total_per_type <- baseline_total_per_type + extra
      current_dwellings_per_parcel <- current_total_per_type / d_nparcels
      current_acres_per_dwelling <- d_acres / current_dwellings_per_parcel
      
      sum(mapply(function(a, d, n) {
        n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
      }, current_acres_per_dwelling, current_dwellings_per_parcel, d_nparcels))
    }
  }
  current_profit <- current_revenue - current_total_cost
  current_roi <- current_profit / current_total_cost 
  
  # With bundle_size more credits
  next_total_dwellings  <- current_dwellings + dwellings_to_add + current_credits + bundle_size
  next_construction <- (dwellings_to_add + current_credits + bundle_size) * construction_cost
  next_tdr_cost <- (current_credits + bundle_size) * sending_tdrcredit_per_unit
  next_total_cost <- land_cost + next_construction + next_tdr_cost
  
  next_revenue <- {
    baseline_total_per_type <- d_nparcels * d_dwellings
    shares <- baseline_total_per_type / sum(baseline_total_per_type)
    extra <- round(shares * (current_credits + bundle_size))
    next_total_per_type <- baseline_total_per_type + extra
    next_dwellings_per_parcel <- next_total_per_type / d_nparcels
    next_acres_per_dwelling <- d_acres / next_dwellings_per_parcel
    
    sum(mapply(function(a, d, n) {
      n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
    }, next_acres_per_dwelling, next_dwellings_per_parcel, d_nparcels))
  }
  next_profit <- next_revenue - next_total_cost
  next_roi <- next_profit / next_total_cost 
  
  # Return ROI improvement instead of profit improvement
  return(next_roi - current_roi)  
}
# Test different bundle sizes
run_allocation <- function(bundle_size = 1) {
  receiving_parcels <- assessors_ag_noIAL %>%
    filter(TAXACRES >= 1) %>%
    mutate(
      TMK = as.character(TMK),
      current_dwellings = ifelse(is.na(n_buildings), 0, n_buildings)
    )
  
  credits_by_tmk <- setNames(rep(0, nrow(receiving_parcels)), receiving_parcels$TMK)
  cost_by_tmk <- setNames(rep(0, nrow(receiving_parcels)), receiving_parcels$TMK)
  
  transactions <- list()
  
  credits_remaining <- n_sending_parcels
  i <- 0
  
  while(credits_remaining >= bundle_size) {
    i <- i + bundle_size
    
    # Calculate marginal ROI with full details for each receiving parcel
    details_list <- lapply(1:nrow(receiving_parcels), function(j) {
      tmk <- as.character(receiving_parcels$TMK[j])
      curr_credits <- credits_by_tmk[[tmk]]
      curr_cost <- cost_by_tmk[[tmk]]
      
      if(is.na(curr_credits)) curr_credits <- 0
      if(is.na(curr_cost)) curr_cost <- 0
      
      calculate_marginal_roi_with_details(
        receiving_parcels$TAXACRES[j],
        receiving_parcels$MKTTOT25[j],
        receiving_parcels$current_dwellings[j],
        curr_credits,
        curr_cost,
        sending_tdrcredit_per_unit  # Use average price
      )
    })
    
    # Extract just marginal ROI for finding best
    marginal_rois <- sapply(details_list, function(d) d$marginal_roi_gain)
    
    best_idx <- which.max(marginal_rois)
    best_details <- details_list[[best_idx]]
    best_tmk <- as.character(receiving_parcels$TMK[best_idx])
    
    # Stop if no positive marginal ROI improvement
    if(best_details$marginal_roi_gain <= 0) {
      message(paste("Bundle size", bundle_size, ": Stopped after", i - bundle_size, 
                    "credits - no more ROI improvement"))
      break
    }
    
    # Allocate bundle to best parcel
    credits_by_tmk[[best_tmk]] <- credits_by_tmk[[best_tmk]] + bundle_size
    cost_by_tmk[[best_tmk]] <- cost_by_tmk[[best_tmk]] + (bundle_size * sending_tdrcredit_per_unit)
    
    # Log transaction with FULL before/after details
    transactions[[i]] <- tibble(
      transaction_number = i,
      sending_TMK = NA_character_,
      sending_market_value = NA_real_,
      demo_cost = NA_real_,
      total_credit_cost = sending_tdrcredit_per_unit,
      receiving_TMK = best_tmk,
      receiving_acres = receiving_parcels$TAXACRES[best_idx],
      receiving_assessed_value = receiving_parcels$MKTTOT25[best_idx],
      receiving_calibrated_value = best_details$land_cost_calibrated,
      
      # Baseline/context
      baseline_max_dwellings = best_details$baseline_max_dwellings,
      n_subdivisions = best_details$n_subdivisions,
      
      # CURRENT state (before this credit)
      current_credits = credits_by_tmk[[best_tmk]] - bundle_size,
      current_dwellings_total = best_details$current_dwellings_total,
      current_cost = best_details$current_cost,
      current_revenue = best_details$current_revenue,
      current_profit = best_details$current_profit,
      current_roi = best_details$current_roi,
      
      # NEXT state (after accepting this credit)
      next_credits = credits_by_tmk[[best_tmk]],
      next_dwellings_total = best_details$next_dwellings_total,
      next_cost = best_details$next_cost,
      next_revenue = best_details$next_revenue,
      next_profit = best_details$next_profit,
      next_roi = best_details$next_roi,
      
      # Change
      marginal_roi_gain = best_details$marginal_roi_gain
    )
    
    credits_remaining <- credits_remaining - bundle_size
    
    if(i %% 10 == 0) message(paste("Allocated", i, "credits..."))
  }
  
  transactions_df <- bind_rows(transactions)
  
  receiving_summary <- receiving_parcels %>%
    mutate(
      credits_allocated = sapply(as.character(TMK), function(t) credits_by_tmk[[t]]),
      total_credit_cost_paid = sapply(as.character(TMK), function(t) cost_by_tmk[[t]])
    )
  
  return(list(
    receiving_parcels = receiving_summary,
    transactions = transactions_df,
    summary = list(
      credits_allocated = sum(credits_by_tmk),
      total_credits_available = n_sending_parcels,
      n_receiving_parcels_used = sum(credits_by_tmk > 0),
      total_value_transacted = sum(cost_by_tmk)
    )
  ))
}

#SPLIT ALLOCATION WITH DYNAMIC CREDIT PRICING
# Modified marginal ROI function that takes a specific credit price
calculate_marginal_roi_with_details <- function(acres, assessed_value, current_dwellings, current_credits, total_credit_cost_so_far,next_credit_price) {
  land_cost <- predict_land_cost(assessed_value)
  
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  max_allowed <- baseline_sub$total_dwellings
  
  dwellings_to_add <- max(0, max_allowed - current_dwellings)
  
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # CURRENT state
  current_total_dwellings <- current_dwellings + dwellings_to_add + current_credits
  current_construction <- (dwellings_to_add + current_credits) * construction_cost
  current_total_cost <- land_cost + current_construction + total_credit_cost_so_far
  
  current_revenue <- {
    if(current_credits == 0) {
      sum(mapply(function(a, d, n) {
        n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
      }, d_acres, d_dwellings, d_nparcels))
    } else {
      baseline_total_per_type <- d_nparcels * d_dwellings
      shares <- baseline_total_per_type / sum(baseline_total_per_type)
      extra <- round(shares * current_credits)
      current_total_per_type <- baseline_total_per_type + extra
      current_dwellings_per_parcel <- current_total_per_type / d_nparcels
      current_acres_per_dwelling <- d_acres / current_dwellings_per_parcel
      
      sum(mapply(function(a, d, n) {
        n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
      }, current_acres_per_dwelling, current_dwellings_per_parcel, d_nparcels))
    }
  }
  current_profit <- current_revenue - current_total_cost
  current_roi <- current_profit / current_total_cost 
  
  # NEXT state (with one more credit)
  next_total_dwellings <- current_dwellings + dwellings_to_add + current_credits + 1
  next_construction <- (dwellings_to_add + current_credits + 1) * construction_cost
  next_total_cost <- land_cost + next_construction + total_credit_cost_so_far + next_credit_price
  
  next_revenue <- {
    baseline_total_per_type <- d_nparcels * d_dwellings
    shares <- baseline_total_per_type / sum(baseline_total_per_type)
    extra <- round(shares * (current_credits + 1))
    next_total_per_type <- baseline_total_per_type + extra
    next_dwellings_per_parcel <- next_total_per_type / d_nparcels
    next_acres_per_dwelling <- d_acres / next_dwellings_per_parcel
    
    sum(mapply(function(a, d, n) {
      n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
    }, next_acres_per_dwelling, next_dwellings_per_parcel, d_nparcels))
  }
  next_profit <- next_revenue - next_total_cost
  next_roi <- next_profit / next_total_cost 
  
  return(list(
    # Current state
    current_dwellings_total = current_total_dwellings,
    current_cost = current_total_cost,
    current_revenue = current_revenue,
    current_profit = current_profit,
    current_roi = current_roi,
    
    # Next state
    next_dwellings_total = next_total_dwellings,
    next_cost = next_total_cost,
    next_revenue = next_revenue,
    next_profit = next_profit,
    next_roi = next_roi,
    
    # Change
    marginal_roi_gain = next_roi - current_roi,
    
    # Context
    baseline_max_dwellings = max_allowed,
    land_cost_calibrated = land_cost,
    n_subdivisions = baseline_sub$total_subdivisions
  ))
}
# Main allocation function using real TDR prices
run_real_tdr_allocation <- function() {
  sending_parcels <- assessors_setback_res_noncpr_nobuild_nogov %>%
    mutate(
      sending_market_value = MKTTOT25_calibrated,
      demo_cost_val = demo_cost,
      total_credit_cost = MKTTOT25_calibrated + demo_cost
    ) %>%
    arrange(total_credit_cost) %>%
    select(sending_TMK = TMK, 
           sending_market_value,
           demo_cost_val,
           total_credit_cost)
  
  receiving_parcels <- assessors_ag_noIAL %>%
    filter(TAXACRES >= 1) %>%
    mutate(
      TMK = as.character(TMK),
      current_dwellings = ifelse(is.na(n_buildings), 0, n_buildings)
    )
  
  credits_by_tmk <- setNames(rep(0, nrow(receiving_parcels)), receiving_parcels$TMK)
  cost_by_tmk <- setNames(rep(0, nrow(receiving_parcels)), receiving_parcels$TMK)
  
  transactions <- list()
  
  for(i in 1:nrow(sending_parcels)) {
    total_credit_cost <- sending_parcels$total_credit_cost[i]
    sending_tmk <- sending_parcels$sending_TMK[i]
    sending_mkt_val <- sending_parcels$sending_market_value[i]
    demo_cost_val <- sending_parcels$demo_cost_val[i]
    
    # Calculate marginal ROI with full details for each receiving parcel
    details_list <- lapply(1:nrow(receiving_parcels), function(j) {
      tmk <- as.character(receiving_parcels$TMK[j])
      curr_credits <- credits_by_tmk[[tmk]]
      curr_cost <- cost_by_tmk[[tmk]]
      
      if(is.na(curr_credits)) curr_credits <- 0
      if(is.na(curr_cost)) curr_cost <- 0
      
      calculate_marginal_roi_with_details(
        receiving_parcels$TAXACRES[j],
        receiving_parcels$MKTTOT25[j],
        receiving_parcels$current_dwellings[j],
        curr_credits,
        curr_cost,
        total_credit_cost
      )
    })
    
    # Extract just marginal ROI for finding best
    marginal_rois <- sapply(details_list, function(d) d$marginal_roi_gain)
    
    best_idx <- which.max(marginal_rois)
    best_details <- details_list[[best_idx]]
    best_tmk <- as.character(receiving_parcels$TMK[best_idx])
    
    if(best_details$marginal_roi_gain <= 0) {
      message(paste("Credit", i, "from", sending_tmk, "at $",
                    scales::comma(total_credit_cost), "- no positive ROI. Stopping auction."))
      break
    }
    
    credits_by_tmk[[best_tmk]] <- credits_by_tmk[[best_tmk]] + 1
    cost_by_tmk[[best_tmk]] <- cost_by_tmk[[best_tmk]] + total_credit_cost
    
    # Log transaction with FULL before/after details
    transactions[[i]] <- tibble(
      transaction_number = i,
      sending_TMK = sending_tmk,
      sending_market_value = sending_mkt_val,
      demo_cost = demo_cost_val,
      total_credit_cost = total_credit_cost,
      receiving_TMK = best_tmk,
      receiving_acres = receiving_parcels$TAXACRES[best_idx],
      receiving_assessed_value = receiving_parcels$MKTTOT25[best_idx],
      receiving_calibrated_value = best_details$land_cost_calibrated,
      
      # Baseline/context
      baseline_max_dwellings = best_details$baseline_max_dwellings,
      n_subdivisions = best_details$n_subdivisions,
      
      # CURRENT state (before this credit)
      current_credits = credits_by_tmk[[best_tmk]] - 1,
      current_dwellings_total = best_details$current_dwellings_total,
      current_cost = best_details$current_cost,
      current_revenue = best_details$current_revenue,
      current_profit = best_details$current_profit,
      current_roi = best_details$current_roi,
      
      # NEXT state (after accepting this credit)
      next_credits = credits_by_tmk[[best_tmk]],
      next_dwellings_total = best_details$next_dwellings_total,
      next_cost = best_details$next_cost,
      next_revenue = best_details$next_revenue,
      next_profit = best_details$next_profit,
      next_roi = best_details$next_roi,
      
      # Change
      marginal_roi_gain = best_details$marginal_roi_gain
    )
    
    if(i %% 10 == 0) message(paste("Allocated", i, "credits..."))
  }
  
  transactions_df <- bind_rows(transactions)
  
  receiving_summary <- receiving_parcels %>%
    mutate(
      credits_allocated = sapply(as.character(TMK), function(t) credits_by_tmk[[t]]),
      total_credit_cost_paid = sapply(as.character(TMK), function(t) cost_by_tmk[[t]])
    )
  
  return(list(
    receiving_parcels = receiving_summary,
    transactions = transactions_df,
    summary = list(
      credits_allocated = sum(credits_by_tmk),
      total_credits_available = nrow(sending_parcels),
      n_receiving_parcels_used = sum(credits_by_tmk > 0),
      total_value_transacted = sum(cost_by_tmk)
    )
  ))
}



# AG CPR NEIGHBORHOOD DEVELOPMENT SCENARIO
# function to calculate scenario where buy entire CPR development. add dwellings where underdeveloped
calculate_cpr_tdr_roi <- function(cpr_row, tdr_credits) {
  # Get subdivision details
  baseline_sub <- calc_baseline_subdivision(cpr_row$total_acres)
  details <- baseline_sub$subdivision_details
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # TDR scenario
  tdr_dwellings = cpr_row$total_max_dwellings + tdr_credits
  tdr_construction = (cpr_row$dwellings_to_add + tdr_credits) * construction_cost
  tdr_credit_cost = tdr_credits * sending_tdrcredit_per_unit
  tdr_total_cost = cpr_row$baseline_purchase_cost + tdr_construction + tdr_credit_cost
  
  # Calculate TDR revenue with distributed dwellings
  tdr_revenue <- predict_ag_acredwel_calib(
    cpr_row$total_acres / tdr_dwellings,
    ag_bldg_sqft
  ) * tdr_dwellings
  
  tdr_profit = tdr_revenue - tdr_total_cost
  tdr_roi = tdr_profit / tdr_total_cost
  
  tibble(
    tdr_credits = tdr_credits,
    tdr_dwellings = tdr_dwellings,
    tdr_total_cost = tdr_total_cost,
    tdr_revenue = tdr_revenue,
    tdr_profit = tdr_profit,
    tdr_roi = tdr_roi,
    
    roi_improvement = tdr_roi - cpr_row$baseline_roi,
    profit_improvement = tdr_profit - cpr_row$baseline_profit
  )
}


# BREAKEVEN CALCULATION FUNCTIONS

# function to calculate what TDR credit price makes ROI = 0 (breakeven)
calculate_breakeven_tdr_price_roi0 <- function(acres, land_cost, current_dwellings, tdr_credits = 58) {
  
  # Calculate baseline subdivision to get max allowable dwellings
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  baseline_dwellings <- baseline_sub$total_dwellings
  
  # Extract subdivision details
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # Calculate dwellings to add
  dwellings_to_add <- baseline_dwellings - current_dwellings
  
  # TDR scenario with unknown credit price
  tdr_dwellings <- baseline_dwellings + tdr_credits
  tdr_construction <- (dwellings_to_add + tdr_credits) * construction_cost
  
  # Calculate TDR revenue with distributed dwellings
  tdr_revenue <- {
    baseline_total_per_type <- d_nparcels * d_dwellings
    shares <- baseline_total_per_type / sum(baseline_total_per_type)
    extra <- round(shares * tdr_credits)
    tdr_total_per_type <- baseline_total_per_type + extra
    tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
    tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
    
    sum(mapply(function(a, d, n) {
      n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
    }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
  }
  
  # For ROI = 0, we need: revenue = total_cost
  # revenue = land_cost + construction + (tdr_credits * X)
  # Solve for X (the TDR credit price):
  # X = (revenue - land_cost - construction) / tdr_credits
  
  breakeven_credit_price <- (tdr_revenue - land_cost - tdr_construction) / tdr_credits
  
  return(breakeven_credit_price)
}

# function to calculate what TDR credit price makes TDR ROI = baseline ROI
calculate_breakeven_tdr_price <- function(acres, land_cost, current_dwellings, tdr_credits = 58) {
  
  # Calculate baseline subdivision to get max allowable dwellings
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  baseline_dwellings <- baseline_sub$total_dwellings
  
  # Extract subdivision details
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # Calculate dwellings to add
  dwellings_to_add <- baseline_dwellings - current_dwellings
  
  # Calculate baseline scenario
  baseline_construction <- dwellings_to_add * construction_cost
  baseline_total_cost <- land_cost + baseline_construction
  
  # Baseline revenue using subdivision structure
  baseline_revenue <- sum(mapply(function(a, d, n) {
    n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
  }, d_acres, d_dwellings, d_nparcels))
  
  baseline_roi <- (baseline_revenue - baseline_total_cost) / baseline_total_cost
  
  # TDR scenario with unknown credit price
  tdr_dwellings <- baseline_dwellings + tdr_credits
  tdr_construction <- (dwellings_to_add + tdr_credits) * construction_cost
  # TDR revenue with distributed dwellings
  tdr_revenue <- {
    baseline_total_per_type <- d_nparcels * d_dwellings
    shares <- baseline_total_per_type / sum(baseline_total_per_type)
    extra <- round(shares * tdr_credits)
    tdr_total_per_type <- baseline_total_per_type + extra
    tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
    tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
    
    sum(mapply(function(a, d, n) {
      n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
    }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
  }
  
  # Solve for breakeven price where TDR ROI = baseline ROI
  breakeven_credit_price <- (tdr_revenue / (1 + baseline_roi) - land_cost - tdr_construction) / tdr_credits
  
  return(breakeven_credit_price)
}

#function to calculate what TDR credit price makes TDR ROI = baseline ROI for CPR development
calculate_cpr_breakeven_tdr_price <- function(cpr_row, tdr_credits = 58) {
  
  baseline_sub <- calc_baseline_subdivision(cpr_row$total_acres)
  details <- baseline_sub$subdivision_details
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # USE PRE-CALCULATED VALUES FROM CPR DATA
  baseline_roi <- cpr_row$baseline_roi
  land_cost <- cpr_row$baseline_purchase_cost
  
  # TDR construction
  tdr_construction <- (cpr_row$dwellings_to_add + tdr_credits) * construction_cost
  
  # TDR revenue (exactly as in calculate_cpr_tdr_roi)
  tdr_revenue <- {
    baseline_total_per_type <- d_nparcels * d_dwellings
    shares <- baseline_total_per_type / sum(baseline_total_per_type)
    extra <- round(shares * tdr_credits)
    tdr_total_per_type <- baseline_total_per_type + extra
    tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
    tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
    
    sum(mapply(function(a, d, n) {
      n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
    }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
  }
  
  # Breakeven: solve for price where TDR ROI = baseline ROI
  (tdr_revenue / (1 + baseline_roi) - land_cost - tdr_construction) / tdr_credits
}

# function to calculate breakeven density ratio (how many dwellings per TDR credit needed to hit ROI = 0)
calculate_breakeven_density_ratio_roi0 <- function(acres, land_cost, baseline_dwellings, dwellings_to_add, tdr_credits = 58, tdr_credit_price = sending_tdrcredit_per_unit) {
  
  # Get subdivision details
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # find ratio R where:
  # revenue(baseline_dwellings + tdr_credits * R) = land_cost + construction(dwellings_to_add + tdr_credits * R) + tdr_credit_cost
  
  objective <- function(ratio) {
    tdr_dwellings <- baseline_dwellings + (tdr_credits * ratio)
    tdr_construction <- (dwellings_to_add + tdr_credits * ratio) * construction_cost
    tdr_credit_cost <- tdr_credits * tdr_credit_price
    tdr_total_cost <- land_cost + tdr_construction + tdr_credit_cost
    
    # Calculate TDR revenue with distributed dwellings
    tdr_revenue <- {
      baseline_total_per_type <- d_nparcels * d_dwellings
      shares <- baseline_total_per_type / sum(baseline_total_per_type)
      extra <- round(shares * (tdr_credits * ratio))
      tdr_total_per_type <- baseline_total_per_type + extra
      tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
      tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
      
      sum(mapply(function(a, d, n) {
        n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
      }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
    }
    
    # We want revenue - cost = 0
    roi <- (tdr_revenue - tdr_total_cost) / tdr_total_cost
    
    # Return absolute value (we want to minimize to find ROI = 0)
    return(abs(roi))
  }
  
  # Find ratio that makes ROI closest to 0
  result <- optimize(objective, interval = c(0.1, 20), tol = 0.001)
  
  return(result$minimum)
}

# function to calculate breakeven density ratio (dwellings per TDR credit needed for TDR ROI = baseline ROI)
calculate_breakeven_density_ratio <- function(acres, land_cost, baseline_dwellings, dwellings_to_add, tdr_credits = 58, tdr_credit_price = sending_tdrcredit_per_unit) {
  
  # Get subdivision details
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # Calculate baseline scenario
  baseline_construction <- dwellings_to_add * construction_cost
  baseline_total_cost <- land_cost + baseline_construction
  
  # Baseline revenue using subdivision structure
  baseline_revenue <- sum(mapply(function(a, d, n) {
    n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
  }, d_acres, d_dwellings, d_nparcels))
  
  baseline_roi <- (baseline_revenue - baseline_total_cost) / baseline_total_cost
  
  # find ratio R where TDR ROI = baseline ROI
  objective <- function(ratio) {
    tdr_dwellings <- baseline_dwellings + (tdr_credits * ratio)
    tdr_construction <- (dwellings_to_add + tdr_credits * ratio) * construction_cost
    tdr_credit_cost <- tdr_credits * tdr_credit_price
    tdr_total_cost <- land_cost + tdr_construction + tdr_credit_cost
    
    # Calculate TDR revenue with distributed dwellings
    tdr_revenue <- {
      baseline_total_per_type <- d_nparcels * d_dwellings
      shares <- baseline_total_per_type / sum(baseline_total_per_type)
      extra <- round(shares * (tdr_credits * ratio))
      tdr_total_per_type <- baseline_total_per_type + extra
      tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
      tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
      
      sum(mapply(function(a, d, n) {
        n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
      }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
    }
    
    # We want TDR ROI - baseline ROI = 0
    tdr_roi <- (tdr_revenue - tdr_total_cost) / tdr_total_cost
    
    # Return absolute difference (we want to minimize to find where they're equal)
    return(abs(tdr_roi - baseline_roi))
  }
  
  # Find ratio that makes TDR ROI = baseline ROI
  result <- optimize(objective, interval = c(0.1, 20), tol = 0.001)
  
  return(list(
    breakeven_density_ratio = result$minimum,
    baseline_roi = baseline_roi
  ))
}

# Calculate breakeven TDR credit price for single credits to single parcels (ROI = 0)
calculate_split_breakeven_price_roi0 <- function(parcels_data, n_credits = 1) {
  
  # If multiple credit amounts provided, calculate for each and combine
  if(length(n_credits) > 1) {
    breakeven_list <- lapply(n_credits, function(n_cred) {
      calculate_split_breakeven_price_roi0(parcels_data, n_credits = n_cred)
    })
    return(bind_rows(breakeven_list))
  }
  
  # Single credit amount calculation
  parcels_data %>%
    rowwise() %>%
    mutate(
      # Calibrate land cost
      land_cost_calibrated = predict_land_cost(MKTTOT25),
      
      # Handle NA in n_buildings (NA means 0 buildings)
      existing_buildings = ifelse(is.na(n_buildings), 0, n_buildings),
      
      # Get baseline dwellings directly
      baseline_dwellings = calc_baseline_subdivision(TAXACRES)$total_dwellings,
      
      # BASELINE: Only build dwellings that don't already exist
      dwellings_to_build_baseline = max(0, baseline_dwellings - existing_buildings),
      baseline_construction_calc = dwellings_to_build_baseline * construction_cost,
      baseline_total_cost_calc = land_cost_calibrated + baseline_construction_calc,
      
      # Revenue based on TOTAL dwellings (existing + new) using subdivision structure
      baseline_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
        }, d_acres, d_dwellings, d_nparcels))
      },
      baseline_roi_calc = (baseline_revenue_calc - baseline_total_cost_calc) / baseline_total_cost_calc,
      
      # TDR SCENARIO: Total dwellings with credits
      tdr_dwellings_total = baseline_dwellings + n_credits,
      dwellings_to_build_tdr = max(0, tdr_dwellings_total - existing_buildings),
      tdr_construction_calc = dwellings_to_build_tdr * construction_cost,
      
      # TDR revenue with distributed dwellings
      tdr_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        
        # Distribute extra dwellings proportionally
        baseline_total_per_type <- d_nparcels * d_dwellings
        shares <- baseline_total_per_type / sum(baseline_total_per_type)
        extra <- round(shares * n_credits)
        tdr_total_per_type <- baseline_total_per_type + extra
        tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
        tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
        
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
        }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
      },
      
      # Incremental values
      incremental_revenue = tdr_revenue_calc - baseline_revenue_calc,
      incremental_construction = n_credits * construction_cost,
      
      # Breakeven price where TDR ROI = 0
      # For ROI = 0: revenue = total_cost
      # revenue = land + construction + (n_credits * price)
      # price = (revenue - land - construction) / n_credits
      breakeven_price = (tdr_revenue_calc - land_cost_calibrated - tdr_construction_calc) / n_credits,
      n_credits = n_credits
    ) %>%
    ungroup() %>%
    select(TMK, TAXACRES, MKTTOT25, land_cost_calibrated, n_buildings, existing_buildings, 
           baseline_dwellings, dwellings_to_build_baseline, dwellings_to_build_tdr,
           baseline_roi_calc, n_credits, incremental_revenue, 
           incremental_construction, breakeven_price)
}

# Calculate breakeven TDR credit ratio for credits to single parcels for a given credit price (ROI = 0)
calculate_split_breakeven_ratio_roi0 <- function(parcels_data, credit_price, max_credits = 50) {
  
  parcels_data %>%
    rowwise() %>%
    mutate(
      # Calibrate land cost
      land_cost_calibrated = predict_land_cost(MKTTOT25),
      
      # Handle NA in n_buildings
      existing_buildings = ifelse(is.na(n_buildings), 0, n_buildings),
      
      # Get baseline dwellings directly
      baseline_dwellings = calc_baseline_subdivision(TAXACRES)$total_dwellings,
      
      # BASELINE calculations
      dwellings_to_build_baseline = max(0, baseline_dwellings - existing_buildings),
      baseline_construction_calc = dwellings_to_build_baseline * construction_cost,
      baseline_total_cost_calc = land_cost_calibrated + baseline_construction_calc,
      
      baseline_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
        }, d_acres, d_dwellings, d_nparcels))
      },
      baseline_roi_calc = (baseline_revenue_calc - baseline_total_cost_calc) / baseline_total_cost_calc,
      
      # Store credit price as column
      credit_price_used = credit_price,
      
      # Find breakeven number of credits using optimization
      breakeven_n_credits = {
        # Get subdivision details for use in objective function
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres_local <- details$acres_per_parcel
        d_dwellings_local <- details$dwellings_per_parcel
        d_nparcels_local <- details$n_parcels
        
        # Objective function: find where TDR ROI = 0
        objective <- function(n_cred) {
          if(n_cred < 0) return(Inf)  # Can't have negative credits
          
          tdr_dwellings_total <- baseline_dwellings + n_cred
          dwellings_to_build <- max(0, tdr_dwellings_total - existing_buildings)
          tdr_construction <- dwellings_to_build * construction_cost
          tdr_total_cost <- land_cost_calibrated + tdr_construction + (n_cred * credit_price)
          
          # TDR revenue with distributed dwellings
          baseline_total_per_type <- d_nparcels_local * d_dwellings_local
          shares <- baseline_total_per_type / sum(baseline_total_per_type)
          extra <- round(shares * n_cred)
          tdr_total_per_type <- baseline_total_per_type + extra
          tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels_local
          tdr_acres_per_dwelling <- d_acres_local / tdr_dwellings_per_parcel
          
          tdr_revenue <- sum(mapply(function(a, d, n) {
            n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
          }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels_local))
          
          tdr_roi <- (tdr_revenue - tdr_total_cost) / tdr_total_cost
          
          # We want TDR ROI = 0, so minimize the squared difference from 0
          tdr_roi^2
        }
        
        # Use optimize to find the number of credits that minimizes the difference
        result <- optimize(objective, interval = c(0, max_credits))
        result$minimum
      }
    ) %>%
    ungroup() %>%
    select(TMK, TAXACRES, MKTTOT25, land_cost_calibrated, n_buildings, existing_buildings,
           baseline_dwellings, baseline_roi_calc, 
           credit_price_used, breakeven_n_credits)
}

# Calculate breakeven TDR credit price for single credits to single parcels
calculate_split_breakeven_price <- function(parcels_data, n_credits = 1) {
  
  # If multiple credit amounts provided, calculate for each and combine
  if(length(n_credits) > 1) {
    breakeven_list <- lapply(n_credits, function(n_cred) {
      calculate_split_breakeven_price(parcels_data, n_credits = n_cred)
    })
    return(bind_rows(breakeven_list))
  }
  
  # Single credit amount calculation
  parcels_data %>%
    rowwise() %>%
    mutate(
      # Calibrate land cost
      land_cost_calibrated = predict_land_cost(MKTTOT25),
      
      # Handle NA in n_buildings (NA means 0 buildings)
      existing_buildings = ifelse(is.na(n_buildings), 0, n_buildings),
      
      # Get baseline dwellings directly
      baseline_dwellings = calc_baseline_subdivision(TAXACRES)$total_dwellings,
      
      # BASELINE: Only build dwellings that don't already exist
      dwellings_to_build_baseline = max(0, baseline_dwellings - existing_buildings),
      baseline_construction_calc = dwellings_to_build_baseline * construction_cost,
      baseline_total_cost_calc = land_cost_calibrated + baseline_construction_calc,
      
      # Revenue based on TOTAL dwellings (existing + new) using subdivision structure
      baseline_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
        }, d_acres, d_dwellings, d_nparcels))
      },
      baseline_roi_calc = (baseline_revenue_calc - baseline_total_cost_calc) / baseline_total_cost_calc,
      
      # TDR SCENARIO: Total dwellings with credits
      tdr_dwellings_total = baseline_dwellings + n_credits,
      dwellings_to_build_tdr = max(0, tdr_dwellings_total - existing_buildings),
      tdr_construction_calc = dwellings_to_build_tdr * construction_cost,
      
      # TDR revenue with distributed dwellings
      tdr_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        
        # Distribute extra dwellings proportionally
        baseline_total_per_type <- d_nparcels * d_dwellings
        shares <- baseline_total_per_type / sum(baseline_total_per_type)
        extra <- round(shares * n_credits)
        tdr_total_per_type <- baseline_total_per_type + extra
        tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
        tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
        
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
        }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
      },
      
      # Incremental values
      incremental_revenue = tdr_revenue_calc - baseline_revenue_calc,
      incremental_construction = n_credits * construction_cost,
      
      # Breakeven price where TDR ROI = baseline ROI
      breakeven_price = (tdr_revenue_calc / (1 + baseline_roi_calc) - land_cost_calibrated - tdr_construction_calc) / n_credits,
      n_credits = n_credits
    ) %>%
    ungroup() %>%
    select(TMK, TAXACRES, MKTTOT25, land_cost_calibrated, n_buildings, existing_buildings, 
           baseline_dwellings, dwellings_to_build_baseline, dwellings_to_build_tdr,
           baseline_roi_calc, n_credits, incremental_revenue, 
           incremental_construction, breakeven_price)
}

# Calculate breakeven TDR credit ratio for credits to single parcels for a given credit price
calculate_split_breakeven_ratio <- function(parcels_data, credit_price, max_credits = 50) {
  
  parcels_data %>%
    rowwise() %>%
    mutate(
      # Calibrate land cost
      land_cost_calibrated = predict_land_cost(MKTTOT25),
      
      # Handle NA in n_buildings
      existing_buildings = ifelse(is.na(n_buildings), 0, n_buildings),
      
      # Get baseline dwellings directly
      baseline_dwellings = calc_baseline_subdivision(TAXACRES)$total_dwellings,
      
      # BASELINE calculations
      dwellings_to_build_baseline = max(0, baseline_dwellings - existing_buildings),
      baseline_construction_calc = dwellings_to_build_baseline * construction_cost,
      baseline_total_cost_calc = land_cost_calibrated + baseline_construction_calc,
      
      baseline_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
        }, d_acres, d_dwellings, d_nparcels))
      },
      baseline_roi_calc = (baseline_revenue_calc - baseline_total_cost_calc) / baseline_total_cost_calc,
      
      # Store credit price as column
      credit_price_used = credit_price,
      
      # Find breakeven number of credits using optimization
      breakeven_n_credits = {
        # Get subdivision details for use in objective function
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres_local <- details$acres_per_parcel
        d_dwellings_local <- details$dwellings_per_parcel
        d_nparcels_local <- details$n_parcels
        
        # Objective function: difference between TDR ROI and baseline ROI
        objective <- function(n_cred) {
          if(n_cred < 0) return(Inf)  # Can't have negative credits
          
          tdr_dwellings_total <- baseline_dwellings + n_cred
          dwellings_to_build <- max(0, tdr_dwellings_total - existing_buildings)
          tdr_construction <- dwellings_to_build * construction_cost
          tdr_total_cost <- land_cost_calibrated + tdr_construction + (n_cred * credit_price)
          
          # TDR revenue with distributed dwellings
          baseline_total_per_type <- d_nparcels_local * d_dwellings_local
          shares <- baseline_total_per_type / sum(baseline_total_per_type)
          extra <- round(shares * n_cred)
          tdr_total_per_type <- baseline_total_per_type + extra
          tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels_local
          tdr_acres_per_dwelling <- d_acres_local / tdr_dwellings_per_parcel
          
          tdr_revenue <- sum(mapply(function(a, d, n) {
            n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
          }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels_local))
          
          tdr_roi <- (tdr_revenue - tdr_total_cost) / tdr_total_cost
          
          # We want TDR ROI = baseline ROI, so minimize the squared difference
          (tdr_roi - baseline_roi_calc)^2
        }
        
        # Use optimize to find the number of credits that minimizes the difference
        result <- optimize(objective, interval = c(0, max_credits))
        result$minimum
      }
    ) %>%
    ungroup() %>%
    select(TMK, TAXACRES, MKTTOT25, land_cost_calibrated, n_buildings, existing_buildings,
           baseline_dwellings, baseline_roi_calc, 
           credit_price_used, breakeven_n_credits)
}


#construction breakeven

# Calculate breakeven construction cost where TDR ROI = 0
calculate_split_breakeven_construction_cost_roi0 <- function(parcels_data, n_credits = 1, credit_price = sending_tdrcredit_per_unit) {
  
  parcels_data %>%
    rowwise() %>%
    mutate(
      # Calibrate land cost
      land_cost_calibrated = predict_land_cost(MKTTOT25),
      
      # Handle NA in n_buildings
      existing_buildings = ifelse(is.na(n_buildings), 0, n_buildings),
      
      # Get baseline dwellings directly
      baseline_dwellings = calc_baseline_subdivision(TAXACRES)$total_dwellings,
      
      dwellings_to_build_baseline = max(0, baseline_dwellings - existing_buildings),
      
      # Calculate revenues using subdivision structure
      baseline_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
        }, d_acres, d_dwellings, d_nparcels))
      },
      
      tdr_dwellings_total = baseline_dwellings + n_credits,
      
      tdr_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        
        # Distribute extra dwellings proportionally
        baseline_total_per_type <- d_nparcels * d_dwellings
        shares <- baseline_total_per_type / sum(baseline_total_per_type)
        extra <- round(shares * n_credits)
        tdr_total_per_type <- baseline_total_per_type + extra
        tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
        tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
        
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
        }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
      },
      
      # TDR credit cost
      tdr_credit_cost = n_credits * credit_price,
      dwellings_to_build_tdr = max(0, tdr_dwellings_total - existing_buildings),
      
      # Solve for construction cost where TDR ROI = 0
      # For ROI = 0: tdr_revenue = land + construction + credit_cost
      # construction_cost = (tdr_revenue - land - credit_cost) / dwellings_to_build_tdr
      breakeven_construction_cost = (tdr_revenue_calc - land_cost_calibrated - tdr_credit_cost) / dwellings_to_build_tdr,
      
      credit_price_used = credit_price,
      n_credits_used = n_credits
    ) %>%
    ungroup() %>%
    select(TMK, TAXACRES, MKTTOT25, land_cost_calibrated, n_buildings, existing_buildings,
           baseline_dwellings, dwellings_to_build_baseline, dwellings_to_build_tdr,
           baseline_revenue_calc, tdr_revenue_calc, 
           credit_price_used, n_credits_used, breakeven_construction_cost)
}

# Calculate breakeven construction cost for split parcels (ROI = 0)
calculate_split_breakeven_construction_cost <- function(parcels_data, n_credits = 1, credit_price = sending_tdrcredit_per_unit) {
  
  parcels_data %>%
    rowwise() %>%
    mutate(
      # Calibrate land cost
      land_cost_calibrated = predict_land_cost(MKTTOT25),
      
      # Handle NA in n_buildings
      existing_buildings = ifelse(is.na(n_buildings), 0, n_buildings),
      
      # Get baseline dwellings directly
      baseline_dwellings = calc_baseline_subdivision(TAXACRES)$total_dwellings,
      
      dwellings_to_build_baseline = max(0, baseline_dwellings - existing_buildings),
      
      # Calculate revenues (independent of construction cost) using subdivision structure
      baseline_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
        }, d_acres, d_dwellings, d_nparcels))
      },
      
      tdr_dwellings_total = baseline_dwellings + n_credits,
      
      tdr_revenue_calc = {
        baseline_sub <- calc_baseline_subdivision(TAXACRES)
        details <- baseline_sub$subdivision_details
        d_acres <- details$acres_per_parcel
        d_dwellings <- details$dwellings_per_parcel
        d_nparcels <- details$n_parcels
        
        # Distribute extra dwellings proportionally
        baseline_total_per_type <- d_nparcels * d_dwellings
        shares <- baseline_total_per_type / sum(baseline_total_per_type)
        extra <- round(shares * n_credits)
        tdr_total_per_type <- baseline_total_per_type + extra
        tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
        tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
        
        sum(mapply(function(a, d, n) {
          n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
        }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
      },
      
      # TDR credit cost
      tdr_credit_cost = n_credits * credit_price,
      dwellings_to_build_tdr = max(0, tdr_dwellings_total - existing_buildings),
      
      # Solve for construction cost where TDR ROI = baseline ROI
      # After algebra (same as above):
      breakeven_construction_cost = {
        numerator <- tdr_revenue_calc * land_cost_calibrated - baseline_revenue_calc * (land_cost_calibrated + tdr_credit_cost)
        denominator <- baseline_revenue_calc * dwellings_to_build_tdr - tdr_revenue_calc * dwellings_to_build_baseline
        numerator / denominator
      },
      
      credit_price_used = credit_price,
      n_credits_used = n_credits
    ) %>%
    ungroup() %>%
    select(TMK, TAXACRES, MKTTOT25, land_cost_calibrated, n_buildings, existing_buildings,
           baseline_dwellings, dwellings_to_build_baseline, dwellings_to_build_tdr,
           baseline_revenue_calc, tdr_revenue_calc, 
           credit_price_used, n_credits_used, breakeven_construction_cost)
}

# Calculate breakeven construction cost where TDR ROI = baseline ROI
calculate_breakeven_construction_cost <- function(acres, land_cost, baseline_dwellings, dwellings_to_add, tdr_credits = 58, tdr_credit_price = sending_tdrcredit_per_unit) {
  
  # Get subdivision details
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # Calculate revenues (these don't depend on construction cost)
  # Baseline revenue
  baseline_revenue <- sum(mapply(function(a, d, n) {
    n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
  }, d_acres, d_dwellings, d_nparcels))
  
  # TDR revenue
  tdr_dwellings <- baseline_dwellings + tdr_credits
  tdr_revenue <- {
    baseline_total_per_type <- d_nparcels * d_dwellings
    shares <- baseline_total_per_type / sum(baseline_total_per_type)
    extra <- round(shares * tdr_credits)
    tdr_total_per_type <- baseline_total_per_type + extra
    tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
    tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
    
    sum(mapply(function(a, d, n) {
      n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
    }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
  }
  
  # TDR credit cost (fixed)
  tdr_credit_cost <- tdr_credits * tdr_credit_price
  
  # Solve for construction cost where TDR ROI = baseline ROI
  numerator <- tdr_revenue * land_cost - baseline_revenue * (land_cost + tdr_credit_cost)
  denominator <- baseline_revenue * (dwellings_to_add + tdr_credits) - tdr_revenue * dwellings_to_add
  
  breakeven_construction_cost <- numerator / denominator
  
  return(breakeven_construction_cost)
}

# Calculate breakeven construction cost for split parcels
calculate_split_breakeven_construction_cost <- function(parcels_data, n_credits = 1, credit_price = sending_tdrcredit_per_unit) {
  
  parcels_data %>%
    rowwise() %>%
    mutate(
      # Handle NA in n_buildings
      existing_buildings = ifelse(is.na(n_buildings), 0, n_buildings),
      dwellings_to_build_baseline = max(0, baseline_dwellings - existing_buildings),
      
      # Calculate revenues (independent of construction cost)
      baseline_total_sqft = baseline_dwellings * ag_bldg_sqft,
      baseline_revenue_calc = predict_ag_acredwel_calib(TAXACRES, baseline_total_sqft),
      
      tdr_dwellings_total = baseline_dwellings + n_credits,
      tdr_total_sqft = tdr_dwellings_total * ag_bldg_sqft,
      tdr_revenue_calc = predict_ag_acredwel_calib(TAXACRES, tdr_total_sqft),
      
      # TDR credit cost
      tdr_credit_cost = n_credits * credit_price,
      dwellings_to_build_tdr = max(0, tdr_dwellings_total - existing_buildings),
      
      # Solve for construction cost where TDR ROI = baseline ROI
      # After algebra (same as above):
      breakeven_construction_cost = {
        numerator <- tdr_revenue_calc * MKTTOT25 - baseline_revenue_calc * (MKTTOT25 + tdr_credit_cost)
        denominator <- baseline_revenue_calc * dwellings_to_build_tdr - tdr_revenue_calc * dwellings_to_build_baseline
        numerator / denominator
      },
      
      credit_price_used = credit_price,
      n_credits_used = n_credits
    ) %>%
    ungroup() %>%
    select(TMK, TAXACRES, MKTTOT25, n_buildings, existing_buildings,
           baseline_dwellings, dwellings_to_build_baseline, dwellings_to_build_tdr,
           baseline_revenue_calc, tdr_revenue_calc, 
           credit_price_used, n_credits_used, breakeven_construction_cost)
}



#calculate breakeven sales price

# Calculate breakeven selling price where TDR ROI = 0
calculate_breakeven_selling_price_roi0 <- function(acres, land_cost, current_dwellings, tdr_credits = 58, tdr_credit_price = sending_tdrcredit_per_unit) {
  
  # Calculate max allowed and dwellings to add
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  max_allowed <- baseline_sub$total_dwellings
  dwellings_to_add <- max(0, max_allowed - current_dwellings)
  
  # Extract subdivision details
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # TDR scenario costs
  tdr_dwellings <- current_dwellings + dwellings_to_add + tdr_credits
  tdr_construction <- (dwellings_to_add + tdr_credits) * construction_cost
  tdr_credit_cost <- tdr_credits * tdr_credit_price
  tdr_total_cost <- land_cost + tdr_construction + tdr_credit_cost
  
  # Predicted revenue from model using subdivision structure
  predicted_revenue <- {
    baseline_total_per_type <- d_nparcels * d_dwellings
    shares <- baseline_total_per_type / sum(baseline_total_per_type)
    extra <- round(shares * tdr_credits)
    tdr_total_per_type <- baseline_total_per_type + extra
    tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
    tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
    
    sum(mapply(function(a, d, n) {
      n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
    }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
  }
  
  # Breakeven revenue (for ROI = 0, revenue must equal total cost)
  breakeven_revenue <- tdr_total_cost
  
  # Required premium over predicted value
  required_premium <- breakeven_revenue - predicted_revenue
  required_premium_pct <- (required_premium / predicted_revenue) * 100
  
  # Calculate total sqft for price per sqft calculations
  tdr_total_sqft <- tdr_dwellings * ag_bldg_sqft
  
  # Price per square foot
  predicted_price_per_sqft <- predicted_revenue / tdr_total_sqft
  breakeven_price_per_sqft <- breakeven_revenue / tdr_total_sqft
  
  return(tibble(
    tdr_dwellings = tdr_dwellings,
    tdr_total_sqft = tdr_total_sqft,
    tdr_total_cost = tdr_total_cost,
    predicted_revenue = predicted_revenue,
    breakeven_revenue = breakeven_revenue,
    required_premium = required_premium,
    required_premium_pct = required_premium_pct,
    predicted_price_per_sqft = predicted_price_per_sqft,
    breakeven_price_per_sqft = breakeven_price_per_sqft
  ))
}

# For comparing TDR to baseline ROI
calculate_breakeven_selling_price <- function(acres, land_cost, current_dwellings, tdr_credits = 58, tdr_credit_price = sending_tdrcredit_per_unit) {
  
  # Calculate max allowed and dwellings to add
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  max_allowed <- baseline_sub$total_dwellings
  dwellings_to_add <- max(0, max_allowed - current_dwellings)
  
  # Extract subdivision details
  d_acres <- details$acres_per_parcel
  d_dwellings <- details$dwellings_per_parcel
  d_nparcels <- details$n_parcels
  
  # Baseline scenario
  baseline_dwellings <- current_dwellings + dwellings_to_add
  baseline_construction <- dwellings_to_add * construction_cost
  baseline_total_cost <- land_cost + baseline_construction
  
  # Baseline revenue using subdivision structure
  baseline_revenue <- sum(mapply(function(a, d, n) {
    n * d * predict_ag_acredwel_calib(a / d, ag_bldg_sqft)
  }, d_acres, d_dwellings, d_nparcels))
  
  baseline_roi <- (baseline_revenue - baseline_total_cost) / baseline_total_cost
  
  # TDR scenario costs
  tdr_dwellings <- current_dwellings + dwellings_to_add + tdr_credits
  tdr_construction <- (dwellings_to_add + tdr_credits) * construction_cost
  tdr_credit_cost <- tdr_credits * tdr_credit_price
  tdr_total_cost <- land_cost + tdr_construction + tdr_credit_cost
  
  # Predicted revenue from model using subdivision structure
  predicted_revenue <- {
    baseline_total_per_type <- d_nparcels * d_dwellings
    shares <- baseline_total_per_type / sum(baseline_total_per_type)
    extra <- round(shares * tdr_credits)
    tdr_total_per_type <- baseline_total_per_type + extra
    tdr_dwellings_per_parcel <- tdr_total_per_type / d_nparcels
    tdr_acres_per_dwelling <- d_acres / tdr_dwellings_per_parcel
    
    sum(mapply(function(a, d, n) {
      n * d * predict_ag_acredwel_calib(a, ag_bldg_sqft)
    }, tdr_acres_per_dwelling, tdr_dwellings_per_parcel, d_nparcels))
  }
  
  # Breakeven revenue (for TDR ROI = baseline ROI)
  breakeven_revenue <- tdr_total_cost * (1 + baseline_roi)
  
  # Required premium
  required_premium <- breakeven_revenue - predicted_revenue
  required_premium_pct <- (required_premium / predicted_revenue) * 100
  
  return(tibble(
    baseline_roi = baseline_roi,
    tdr_dwellings = tdr_dwellings,
    tdr_total_cost = tdr_total_cost,
    predicted_revenue = predicted_revenue,
    breakeven_revenue = breakeven_revenue,
    required_premium = required_premium,
    required_premium_pct = required_premium_pct
  ))
}







## SINGLE PARCEL SCENARIO

# Run baseline & TDR analysis for single parcels
tdr_allparcels <- assessors_ag_noIAL %>%
  filter(TAXACRES >= 10) %>%
  rowwise() %>%
  mutate(
    baseline_sub = list(calc_baseline_subdivision(TAXACRES)),
    baseline_dwellings_calc = baseline_sub$total_dwellings,
    current_dwellings = ifelse(is.na(n_buildings), 0, n_buildings),
    land_cost_calibrated = predict_land_cost(MKTTOT25)
  ) %>%
  mutate(analysis = list(calculate_tdr_roi(TAXACRES, land_cost_calibrated, current_dwellings,n_sending_parcels))) %>%
  unnest(analysis) %>%
  ungroup()

tdr_allparcels_split <- assessors_ag_noIAL %>%
  rowwise() %>%
  mutate(
    baseline_sub = list(calc_baseline_subdivision(TAXACRES)),
    baseline_dwellings_calc = baseline_sub$total_dwellings,
    current_dwellings = ifelse(is.na(n_buildings), 0, n_buildings),
    land_cost_calibrated = predict_land_cost(MKTTOT25)
  ) %>%
  mutate(analysis = list(calculate_tdr_roi(TAXACRES, land_cost_calibrated, current_dwellings,n_sending_parcels))) %>%
  unnest(analysis) %>%
  ungroup()

# Summary statistics
tdr_allparcels %>%
  summarise(
    n_parcels = n(),
    mean_baseline_roi = mean(baseline_roi, na.rm = TRUE),
    mean_tdr_roi = mean(tdr_roi, na.rm = TRUE),
    median_baseline_roi = median(baseline_roi, na.rm = TRUE),
    median_tdr_roi = median(tdr_roi, na.rm = TRUE),
    n_positive_baseline_roi = sum(baseline_roi > 0, na.rm = TRUE),
    n_positive_tdr_roi = sum(tdr_roi > 0, na.rm = TRUE),
    n_tdr_better_than_baseline = sum(tdr_roi > baseline_roi, na.rm = TRUE)
  )









#SPLIT ALLOCATION SCENARIO

# Allocate credits one at a time
tdr_split_allocation_1 <- run_allocation(bundle_size = 1)

tdr_split_allocation_realprice <- run_real_tdr_allocation()

#try with bundled credits (more than one at a time - testing economies of scale)
#tdr_split_allocation_5 <- run_allocation(bundle_size = 5)
#tdr_split_allocation_10 <- run_allocation(bundle_size = 10)
#tdr_split_allocation_20 <- run_allocation(bundle_size = 20)
#tdr_split_allocation_40 <- run_allocation(bundle_size = 40)
#tdr_split_allocation_58 <- run_allocation(bundle_size = 58)

# Calculate final ROI for each parcel with allocated credits
#the below code only is relevant if credits are allocated above

# tdr_split_results <- if (nrow(filter(tdr_split_allocation, credits_allocated > 0)) > 0) {
#   filter(tdr_split_allocation, credits_allocated > 0) %>% 
#     rowwise() %>% 
#     mutate(analysis = list(calculate_tdr_roi(TAXACRES, MKTTOT25, credits_allocated))) %>% 
#     unnest(analysis) %>% ungroup()
# } else {
#   filter(tdr_split_allocation, credits_allocated > 0)
# }






# AG CPR NEIGHBORHOOD DEVELOPMENT SCENARIO

cpr_baseline_analysis <- assessors_agCPR_noIAL %>%
  group_by(COTMK) %>%
  summarise(
    n_units = n(),
    total_acres = sum(TAXACRES),
    
    # Current state
    current_market_value = sum(MKTTOT25),  
    land_cost_calibrated = predict_land_cost(current_market_value),
    current_dwellings = sum(n_buildings, na.rm = TRUE),  
    n_vacant = sum(n_buildings == 0 | is.na(n_buildings)),
    
    # Calculate max dwellings for each unit
    total_max_dwellings = sum(sapply(TAXACRES, calc_dwellings_per_parcel)),
    
    # Dwellings we can add (vacant lots built to max)
    dwellings_to_add = total_max_dwellings - current_dwellings,
    
    .groups = 'drop'
  ) %>%
  filter(n_units > 1) %>%  # Only actual CPR developments
  rowwise() %>%
  mutate(
    # Baseline scenario: buy + build out vacant
    baseline_purchase_cost = land_cost_calibrated,
    baseline_construction_cost = dwellings_to_add * construction_cost,
    baseline_total_cost = baseline_purchase_cost + baseline_construction_cost,
    
    # Predict value when fully built
    baseline_revenue = predict_ag_acredwel_calib(
      total_acres / total_max_dwellings,  # acres per dwelling
      ag_bldg_sqft
    ) * total_max_dwellings,
    baseline_profit = baseline_revenue - baseline_total_cost,
    baseline_roi = baseline_profit / baseline_total_cost
  ) %>%
  ungroup()

# Apply all 58 credits to each CPR development
tdr_cpr_dev <- cpr_baseline_analysis %>%
  rowwise() %>%
  mutate(
    analysis = list(calculate_cpr_tdr_roi(pick(everything()), n_sending_parcels))
  ) %>%
  unnest(analysis) %>%
  ungroup()











## BREAKEVEN CALCULATIONS





#breakeven by lowering tdr credit price

# Calculate breakeven for single parcels where roi > 0
tdr_breakeven_single_roi0 <- tdr_allparcels %>%
  rowwise() %>%
  mutate(
    land_cost_calibrated = predict_land_cost(MKTTOT25),
    breakeven_tdr_price = calculate_breakeven_tdr_price_roi0(TAXACRES, land_cost_calibrated, current_dwellings,n_sending_parcels)
  ) %>%
  ungroup()

# Calculate breakeven for single parcels where TDR roi > baseline roi
tdr_breakeven_single <- tdr_allparcels %>%
  rowwise() %>%
  mutate(
    land_cost_calibrated = predict_land_cost(MKTTOT25),
    breakeven_tdr_price = calculate_breakeven_tdr_price(TAXACRES, land_cost_calibrated, current_dwellings, n_sending_parcels)
  ) %>%
  ungroup()

# Calculate breakeven for CPR developments
tdr_breakeven_cpr <- tdr_cpr_dev %>%
  rowwise() %>%
  mutate(breakeven_tdr_price = calculate_cpr_breakeven_tdr_price(cur_data(), n_sending_parcels)) %>%
  ungroup()

# Calculate breakeven for split allocation 
tdr_breakeven_split <- calculate_split_breakeven_price(tdr_allparcels_split, n_credits = c(1)) #, 2, 5, 10, 20))

tdr_breakeven_split_roi0 <- calculate_split_breakeven_price_roi0(tdr_allparcels_split, n_credits = c(1)) #, 2, 5, 10, 20))

# Get sending parcel cost stats
tdr_breakeven_split_sending_costs <- assessors_setback_res_noncpr_nobuild_nogov %>%
  mutate(total_credit_cost = MKTTOT25_calibrated + demo_cost)

# Add ratio column
tdr_breakeven_split <- tdr_breakeven_split %>%
  mutate(
    #what multiple of current credits would be needed to break even?
    density_ratio = breakeven_price / (incremental_revenue / n_credits)
  )




# breakeven by increasing density ratio

# Calculate breakeven ratio for single parcels
tdr_breakeven_ratio_single <- tdr_allparcels %>%
  rowwise() %>%
  mutate(land_cost_calibrated = predict_land_cost(MKTTOT25)) %>%
  mutate(
    result = list(calculate_breakeven_density_ratio(
      TAXACRES,
      land_cost_calibrated,
      baseline_dwellings,
      dwellings_to_add,
      n_sending_parcels
    )),
    breakeven_density_ratio = result$breakeven_density_ratio,
    baseline_roi = result$baseline_roi
  ) %>%
  select(-result) %>%  # Remove the intermediate list column
  ungroup()

tdr_breakeven_ratio_single_roi0 <- tdr_allparcels %>%
  rowwise() %>%
  mutate(land_cost_calibrated = predict_land_cost(MKTTOT25)) %>%
  mutate(
    breakeven_density_ratio = calculate_breakeven_density_ratio_roi0(
      TAXACRES,
      land_cost_calibrated,
      baseline_dwellings,
      dwellings_to_add,  
      n_sending_parcels
    )
  ) %>%
  ungroup()

# Calculate breakeven ratio for CPR developments
tdr_breakeven_ratio_cpr <- tdr_cpr_dev %>%
  rowwise() %>%
  mutate(
    # For over-density parcels, set dwellings_to_add to 0
    dwellings_to_add_adjusted = max(0, dwellings_to_add),
    
    result = list(calculate_breakeven_density_ratio(
      acres = total_acres,
      land_cost = land_cost_calibrated,  
      baseline_dwellings = current_dwellings,
      dwellings_to_add = dwellings_to_add_adjusted,  
      tdr_credits = tdr_credits
    )),
    breakeven_density_ratio = result$breakeven_density_ratio,
    baseline_roi_calc = result$baseline_roi  
  ) %>%
  select(-result) %>%
  ungroup()

tdr_breakeven_ratio_cpr_roi0 <- tdr_cpr_dev %>%
  rowwise() %>%
  mutate(
    # For over-density parcels, set dwellings_to_add to 0
    dwellings_to_add_adjusted = max(0, dwellings_to_add),
    
    breakeven_density_ratio = calculate_breakeven_density_ratio_roi0(
      total_acres,           
      land_cost_calibrated,  
      total_max_dwellings,   
      dwellings_to_add_adjusted,      
      n_sending_parcels      
    )
  ) %>%
  ungroup()

# Calculate breakeven ratio for split allocation (portfolio-level)
tdr_breakeven_ratio_split <- calculate_split_breakeven_ratio(tdr_allparcels_split, credit_price = sending_tdrcredit_per_unit)







# calculate construction cost breakeven cost

# Calculate breakeven construction cost for single parcels
tdr_breakeven_construction <- tdr_allparcels %>%
  rowwise() %>%
  mutate(
    breakeven_construction_cost = calculate_breakeven_construction_cost(
      TAXACRES, 
      land_cost_calibrated, 
      current_dwellings,
      dwellings_to_add,  
      n_sending_parcels,
      sending_tdrcredit_per_unit
    )
  ) %>%
  ungroup()






tdr_breakeven_construction_split <- calculate_split_breakeven_construction_cost(
  tdr_allparcels_split, 
  n_credits = 1, 
  credit_price = sending_tdrcredit_per_unit
)





tdr_breakeven_construction_cpr <- tdr_cpr_dev %>%
  rowwise() %>%
  mutate(
    breakeven_tdr_price = calculate_breakeven_construction_cost(
      total_acres,
      land_cost_calibrated,
      total_max_dwellings,
      dwellings_to_add,
      n_sending_parcels
    )
  ) %>%
  ungroup()






#calculate sales price breakeven



# Apply to all parcels - TDR ROI = baseline ROI version
tdr_breakeven_selling <- tdr_allparcels %>%
  filter(TAXACRES >= 1) %>%
  mutate(current_dwellings = ifelse(is.na(n_buildings), 0, n_buildings)) %>%
  rowwise() %>%
  mutate(
    breakeven_analysis = list(calculate_breakeven_selling_price(
      TAXACRES,
      land_cost_calibrated,
      current_dwellings,
      n_sending_parcels,
      sending_tdrcredit_per_unit
    ))
  ) %>%
  unnest(breakeven_analysis, names_sep = "_") %>%
  ungroup()%>%
  mutate(
    required_premium_per_dwelling = breakeven_analysis_required_premium / tdr_dwellings,
    predicted_revenue_per_dwelling = breakeven_analysis_predicted_revenue / tdr_dwellings,
    breakeven_revenue_per_dwelling = breakeven_analysis_breakeven_revenue / tdr_dwellings,
    required_premium_pct_per_dwelling = (required_premium_per_dwelling / predicted_revenue_per_dwelling) * 100
  )





