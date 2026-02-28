library(data.table)
library(mgcv)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)


source('config_dir.R')

# Disable scientific notation
options(scipen = 999)




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
assessors_setback_res_noncpr_nobuild_nogov[assessors_setback_res_noncpr_nobuild_nogov$TMK == 435002029,"n_buildings"] <- 1 #manual fix for parcel with building
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




#calibration model for vacant parcels in setback

#model based on vacant parcels in setback
assessors_setback_res_vacant <- assessors_setback_res[is.na(assessors_setback_res$n_buildings),]
bk_assessors_setback_res_vacant <- bk_assessors %>%
  filter(PARID %in% assessors_setback_res_vacant$PARID)
sales_calibration_send_vacant <- bk_assessors_setback_res_vacant %>%
  filter(!is.na(MKTTOT25) & !is.na(sales_price_2025) & 
           sales_price_2025 > 50000 & sales_price_2025 < 30000000 & 
           MKTTOT25 < 30000000)
calibration_model_send_vacant <- lm(sales_price_2025 ~ MKTTOT25, data = sales_calibration_send_vacant)
summary(calibration_model_send_vacant)
ggplot(sales_calibration_send_vacant, aes(x = MKTTOT25, y = sales_price_2025)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + # 1:1 line for reference
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Assessed Value (MKTTOT25)", y = "Sales Price (2025 $)") +
  theme_classic()

#multiplier ratio calibration
sales_calibration_send_vacant <- sales_calibration_send_vacant %>%
  mutate(ratio = sales_price_2025 / MKTTOT25)
summary(sales_calibration_send_vacant$ratio)
sales_calibration_send_vacant_clean <- sales_calibration_send_vacant %>%
  filter(ratio < 10)
sending_calibration_ratio <- median(sales_calibration_send_vacant_clean$ratio)



#apply calibration  to vacant parcels with no buildable area
#isolate just vacant parcels
sending_vacant <- assessors_setback_res_noncpr_nobuild_nogov[is.na(assessors_setback_res_noncpr_nobuild_nogov$n_buildings),]
sending_vacant_tmk <- unique(sending_vacant$PARID)
bk_sending_vacant <- bk_assessors_setback_res_noncpr_nobuild_nogov[is.na(bk_assessors_setback_res_noncpr_nobuild_nogov$n_buildings),]

bk_sending_vacant <- bk_sending_vacant %>%
  mutate( MKTTOT25_calibrated = MKTTOT25 * sending_calibration_ratio)
sending_vacant <- sending_vacant %>%
  mutate(MKTTOT25_calibrated = MKTTOT25 * sending_calibration_ratio)
sending_total_value_vacant <- sum(sending_vacant$MKTTOT25_calibrated,na.rm=T)
summary(sending_vacant$MKTTOT25_calibrated)

















#receiving area
# ag non-cpr'd parcels

# Estimate 2025 SFH price index 
# https://data.uhero.hawaii.edu/#/series?id=160330&data_list_id=29&sa=true&geo=KAU&freq=A&start=1994-01-01
sfh_price_index <- data.frame(
  year = 1994:2025,
  price_index = c(
    246.2, 264.6, 220.6, 231.7, 233.1, 236.8, 259, 286.5, 335, 367.6, 
    499.5, 632.3, 694.8, 654.1, 607.4, 489.1, 493.6, 474.1, 475.9, 520.9, 
    547.2, 631.3, 628.5, 656.2, 708.8, 668.8, 792.3, 1133.80, 1214.10, 
    1200.80, 1343.90, 1215.0
  ) * 1000  
)
sfh_index_2025 <- 1215.0 *1000

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


#isolate just vacant ag parcels
assessors_ag_vacant <- assessors_ag[is.na(assessors_ag$n_buildings),]
assessors_agCPR_vacant <-assessors_agCPR[is.na(assessors_agCPR$n_buildings),]

ag_vacant_parid<- unique(assessors_ag_vacant$PARID)
agCPR_vacant_parid<- unique(assessors_agCPR_vacant$PARID)
bk_ag_vacant <- bk_ag[bk_ag$PARID %in% ag_vacant_parid, ]
bk_agCPR_vacant <- bk_ag[bk_ag$PARID %in% agCPR_vacant_parid, ]
bk_ag_vacant <- bk_ag_vacant %>%
  mutate(
    date_sale = as.Date(as.character(contract_date), format = "%Y%m%d"),
    year_sale = as.numeric(format(date_sale, "%Y")),
    deflator = sapply(year_sale, adjust_to_2025),
    sales_price_2025 = sales_price * deflator
  )
bk_agCPR_vacant <- bk_agCPR_vacant %>%
  mutate(
    date_sale = as.Date(as.character(contract_date), format = "%Y%m%d"),
    year_sale = as.numeric(format(date_sale, "%Y")),
    deflator = sapply(year_sale, adjust_to_2025),
    sales_price_2025 = sales_price * deflator
  )
bk_agCPR_vacant_non0_nonoutlier <- bk_agCPR_vacant %>%
  filter(!is.na(sales_price_2025) & sales_price_2025 > 50000 & sales_price_2025 < 50000000)

#estimate sale price per unit
median(bk_agCPR_vacant_non0_nonoutlier$sales_price_2025) #mean sales price of a CPR
median(assessors_agCPR_vacant$MKTTOT25) #mean value of a CPR




#calibrate model: actual sales price is higher than assessors data

#for ag non-CPR parcels (starting non-developed parcel)
sales_calibration_ag <- bk_ag_vacant %>%
  filter(!is.na(MKTTOT25) & !is.na(sales_price_2025) &
           sales_price_2025 > 50000 & sales_price_2025 < 15000000 &
           MKTTOT25 < 15000000)
calibration_model_vacant <- lm(sales_price_2025 ~ MKTTOT25, data = sales_calibration_ag)
summary(calibration_model_vacant)
predict_land_cost <- function(mkttot25) {
  predict(calibration_model_vacant, newdata = data.frame(MKTTOT25 = mkttot25))
}


#for ag CPR parcels (non-developed parcels to be sold)
#part A: acres → assessed value
ag_acre_vacant_mkt_model <- lm(MKTTOT25 ~ TAXACRES, 
                               data = assessors_agCPR_vacant %>% 
                                 filter(TAXACRES < 50, MKTTOT25 < 5000000))
summary(ag_acre_vacant_mkt_model)
#part B: assessed value → sales price
sales_calibration_agCPR <- bk_agCPR_vacant %>%
  filter(!is.na(MKTTOT25) & !is.na(sales_price_2025) &
           sales_price_2025 > 50000 & sales_price_2025 < 15000000 &
           MKTTOT25 < 15000000)
calibration_model_agCPR <- lm(sales_price_2025 ~ MKTTOT25, data = sales_calibration_agCPR)
summary(calibration_model_agCPR)
predict_ag_acre_vacant_calib <- function(acres) {
  # Stage 1: acres → assessed value
  pred_mkt <- predict(ag_acre_vacant_mkt_model, newdata = data.frame(TAXACRES = acres))
  # Stage 2: assessed value → sales price
  predict(calibration_model_agCPR, newdata = data.frame(MKTTOT25 = pred_mkt))
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


#SPLIT ALLOCATION WITH DYNAMIC CREDIT PRICING
# Modified marginal ROI function that takes a specific credit price
calculate_marginal_roi_vacant <- function(acres, assessed_value, current_dwellings, current_credits, total_credit_cost_so_far,next_credit_price) {
  land_cost <- predict_land_cost(assessed_value)
  
  baseline_sub <- calc_baseline_subdivision(acres)
  details <- baseline_sub$subdivision_details
  max_allowed <- baseline_sub$total_dwellings
  

  d_acres <- details$acres_per_parcel
  d_nparcels <- details$n_parcels
  
  # CURRENT state
  current_total_cost <- land_cost + total_credit_cost_so_far
  
  current_revenue <- {
      current_total_parcels <- sum(d_nparcels) + current_credits
      avg_acres_per_parcel <- acres / current_total_parcels
      current_total_parcels * predict_ag_acre_vacant_calib(avg_acres_per_parcel)
  }
  current_profit <- current_revenue - current_total_cost
  current_roi <- current_profit / current_total_cost
  
  # NEXT state (with one more credit)
  next_total_cost <- land_cost + total_credit_cost_so_far + next_credit_price
  
  next_revenue <- {
    next_total_parcels <- sum(d_nparcels) + current_credits + 1
    avg_acres_per_next_parcel <- acres / next_total_parcels
    next_total_parcels * predict_ag_acre_vacant_calib(avg_acres_per_next_parcel)
  }
  next_profit <- next_revenue - next_total_cost
  next_roi <- next_profit / next_total_cost 
  
  return(list(
    # Current state
    current_cost = current_total_cost,
    current_revenue = current_revenue,
    current_profit = current_profit,
    current_roi = current_roi,
    
    # Next state
    next_cost = next_total_cost,
    next_revenue = next_revenue,
    next_profit = next_profit,
    next_roi = next_roi,
    
    # Change
    marginal_roi_gain = next_roi - current_roi,
    
    # Context
    land_cost_calibrated = land_cost,
    n_subdivisions = baseline_sub$total_subdivisions
  ))
}

# Main allocation function using real TDR prices
run_real_tdr_allocation_vacant <- function() {
  sending_parcels <- sending_vacant %>%
    mutate(
      sending_market_value = MKTTOT25_calibrated,
      total_credit_cost = MKTTOT25_calibrated 
    ) %>%
    arrange(total_credit_cost) %>%
    select(sending_TMK = TMK, 
           sending_market_value,
           total_credit_cost)
  
  receiving_parcels <- assessors_ag_vacant %>%
    filter(TAXACRES >= 1,
           !grepl("COUNTY OF KAUAI|STATE OF HAWAII|HAWAIIAN HOME LANDS", OWN1, ignore.case = TRUE)) %>%
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

    # Calculate marginal ROI with full details for each receiving parcel
    details_list <- lapply(1:nrow(receiving_parcels), function(j) {
      tmk <- as.character(receiving_parcels$TMK[j])
      curr_credits <- credits_by_tmk[[tmk]]
      curr_cost <- cost_by_tmk[[tmk]]
      
      if(is.na(curr_credits)) curr_credits <- 0
      if(is.na(curr_cost)) curr_cost <- 0
      
      calculate_marginal_roi_vacant(
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
      total_credit_cost = total_credit_cost,
      receiving_TMK = best_tmk,
      receiving_PARID = receiving_parcels$PARID[best_idx], 
      receiving_acres = receiving_parcels$TAXACRES[best_idx],
      receiving_assessed_value = receiving_parcels$MKTTOT25[best_idx],
      receiving_calibrated_value = best_details$land_cost_calibrated,
      
      # Baseline/context
      n_subdivisions = best_details$n_subdivisions,
      
      # CURRENT state (before this credit)
      current_credits = credits_by_tmk[[best_tmk]] - 1,
      current_cost = best_details$current_cost,
      current_revenue = best_details$current_revenue,
      current_profit = best_details$current_profit,
      current_roi = best_details$current_roi,
      
      # NEXT state (after accepting this credit)
      next_credits = credits_by_tmk[[best_tmk]],
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







#SPLIT ALLOCATION SCENARIO

# Allocate credits one at a time
tdr_split_allocation_realprice_vacant <- run_real_tdr_allocation_vacant()










