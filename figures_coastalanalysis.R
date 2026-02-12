# coastal analysis figures

library(patchwork)
library(stargazer)



bk_setback_res
assessors_setback_res


#all res in setback
bk_setback_res <- bk_setback_res %>%
  left_join(assessors_hazard, by = "PARID")


ggplot(bk_setback_res %>% filter(!is.na(buildable_area), !is.na(sales_price_2025)),
       aes(x = buildable_area, y = sales_price_2025, color = year_sale)) +
  geom_point(alpha = 0.5, size = 2) +
  #geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_y_continuous(labels = label_dollar(), limits = c(0, 50000000)) +
  scale_x_continuous(labels = label_comma(), limits=c(0,500000)) + # , limits=c(0,500000)
  scale_color_viridis_c(option = "plasma", name = "Sale Year") +
  labs(
    x = "Buildable area (sq ft)",
    y = "Sales price ($2025)" #, caption = paste0("N = ", sum(!is.na(bk_setback_res$buildable_area) & !is.na(bk_setback_res$sales_price_2025)))
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))



#non cpr res wtih <1000 sqft buildable area
bk_setback_res_noncpr_nobuild <- bk_setback_res %>%
  filter(!is.na(buildable_area), 
         buildable_area < 1000,
         PARCEL_SQFT.x > 80)
assessors_setback_res_noncpr_nobuild <- assessors_setback_res %>%
  filter(!is.na(buildable_area), 
         buildable_area < 1000,
         PARCEL_SQFT > 80)

ggplot(bk_setback_res_noncpr_nobuild %>% filter(!is.na(buildable_area), !is.na(sales_price_2025)),
       aes(x = buildable_area, y = sales_price_2025, color = year_sale)) +
  geom_point(alpha = 0.5, size = 2) +
  #geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_y_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_x_continuous(labels = label_comma(), limits=c(0,1000)) + # , limits=c(0,500000)
  scale_color_viridis_c(option = "plasma", name = "Sale Year") +
  labs(
    x = "Buildable area (sq ft)",
    y = "Sales price ($2025)" #, caption = paste0("N = ", sum(!is.na(bk_setback_res$buildable_area) & !is.na(bk_setback_res$sales_price_2025)))
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))










bk_setback_res_non0_noncpr_nobuild <- bk_setback_res %>%
  filter(!is.na(buildable_area), 
         !is.na(sales_price_2025),
         buildable_area < 1000,
         sales_price_2025 > 50000,
         sales_price_2025 < 50000000,
         PARCEL_SQFT.x > 80)



ggplot(bk_setback_res_non0_noncpr_nobuild, aes(x = sales_price_2025)) +
  geom_histogram(bins = 30, fill = "grey", alpha = 0.7,color="black") +
  geom_vline(aes(xintercept = median(sales_price_2025)), 
             color = "darkred", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    x = "Sales Price ($2025)",
    y = "Number of sales",
    caption = paste0("N = ", nrow(bk_setback_res_non0_noncpr_nobuild), 
                     " | Median = ", dollar(median(bk_setback_res_non0_noncpr_nobuild$sales_price_2025)))
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))


ggplot(assessors_setback_res_noncpr_nobuild, aes(x = ASSDTOT25)) +
  geom_histogram(bins = 30, fill = "grey", alpha = 0.7,color="black") +
  geom_vline(aes(xintercept = median(ASSDTOT25)), 
             color = "darkred", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    x = "Assessed total value ($2025)",
    y = "Number",
    caption = paste0("N = ", nrow(assessors_setback_res_noncpr_nobuild), 
                     " | Median = ", dollar(median(assessors_setback_res_noncpr_nobuild$ASSDTOT25)))
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))






# sales price vs assessors value 

bk_setback_res_non0_noncpr_nobuild <- bk_setback_res_non0_noncpr_nobuild %>%
  mutate(
    # Ratio: Sales / Assessed (>1 means underassessed, <1 means overassessed)
    ratio_sale_to_mkt = sales_price_2025 / MKTTOT25,
    ratio_sale_to_assd = sales_price_2025 / ASSDTOT25,
    
    # Absolute difference
    diff_sale_minus_mkt = sales_price_2025 - MKTTOT25,
    diff_sale_minus_assd = sales_price_2025 - ASSDTOT25
  )
bk_setback_res_non0_noncpr <- bk_setback_res_non0_noncpr %>%
  mutate(
    # Ratio: Sales / Assessed (>1 means underassessed, <1 means overassessed)
    ratio_sale_to_mkt = sales_price_2025 / MKTTOT25,
    ratio_sale_to_assd = sales_price_2025 / ASSDTOT25,
    
    # Absolute difference
    diff_sale_minus_mkt = sales_price_2025 - MKTTOT25,
    diff_sale_minus_assd = sales_price_2025 - ASSDTOT25
  )


ggplot(bk_setback_res_non0_noncpr_nobuild %>% filter(!is.na(MKTTOT25) & sales_price_2025 > 50000 & PARCEL_SQFT.x > 80 & sales_price_2025 < 30000000), 
       aes(x = MKTTOT25, y = sales_price_2025)) +
  geom_point( alpha = 0.6) + #aes(color = buildable_area),
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  scale_x_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_y_continuous(labels = label_dollar(), limits = c(0, NA)) +
  #scale_color_viridis_c(name = "Buildable\nArea (sq ft)") +
  labs(
    x = "Assessor Market Value (2025)",
    y = "Sales Price (2025 $)"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

ggplot(bk_setback_res_non0_noncpr %>% filter(!is.na(MKTTOT25) & sales_price_2025 > 50000 & PARCEL_SQFT > 80 & sales_price_2025 < 50000000), 
       aes(x = MKTTOT25, y = sales_price_2025)) +
  geom_point( alpha = 0.6) + #aes(color = buildable_area),
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  scale_x_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_y_continuous(labels = label_dollar(), limits = c(0, NA)) +
  #scale_color_viridis_c(name = "Buildable\nArea (sq ft)") +
  labs(
    x = "Assessor Market Value (2025)",
    y = "Sales Price (2025 $)"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

ggplot(bk_setback_res_non0_noncpr_nobuild %>% filter(!is.na(ratio_sale_to_mkt)& sales_price_2025 > 50000 & PARCEL_SQFT.x > 80, ratio_sale_to_mkt < 5 & sales_price_2025 < 50000000), 
       aes(x = ratio_sale_to_mkt)) +
  geom_histogram(bins = 50, fill = "grey",color="black", alpha = 0.7) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(ratio_sale_to_mkt, na.rm = TRUE)), 
             color = "darkblue", linetype = "dashed", size = 1) +
  labs(
    subtitle = "Red line = perfect assessment (1.0), Blue line = median ratio",
    x = "Sales Price / Assessed Value",
    y = "Count",
    caption = "Values > 1 indicate underassessment, < 1 indicate overassessment"
  ) +
  theme_minimal()

ggplot(bk_setback_res_non0_noncpr %>% filter(!is.na(ratio_sale_to_mkt)& sales_price_2025 > 50000 & PARCEL_SQFT > 80, ratio_sale_to_mkt < 5 & sales_price_2025 < 50000000), 
       aes(x = ratio_sale_to_mkt)) +
  geom_histogram(bins = 50, fill = "grey",color="black", alpha = 0.7) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(ratio_sale_to_mkt, na.rm = TRUE)), 
             color = "darkblue", linetype = "dashed", size = 1) +
  labs(
    subtitle = "Red line = perfect assessment (1.0), Blue line = median ratio",
    x = "Sales Price / Assessed Value",
    y = "Count",
    caption = "Values > 1 indicate underassessment, < 1 indicate overassessment"
  ) +
  theme_minimal()



bk_inland <- bk_inland %>%
  mutate(
    date_sale = as.Date(as.character(contract_date), format = "%Y%m%d"),
    year_sale = as.numeric(format(date_sale, "%Y")),
    deflator = sapply(year_sale, adjust_to_2025),
    sales_price_2025 = sales_price * deflator,
    
    # Ratio: Sales / Assessed (>1 means underassessed, <1 means overassessed)
    ratio_sale_to_mkt = sales_price_2025 / MKTTOT25,
    ratio_sale_to_assd = sales_price_2025 / ASSDTOT25,
    
    # Absolute difference
    diff_sale_minus_mkt = sales_price_2025 - MKTTOT25,
    diff_sale_minus_assd = sales_price_2025 - ASSDTOT25
  )


ggplot(bk_inland %>% filter(!is.na(MKTTOT25) & sales_price_2025 > 50000 & PARCEL_SQFT > 80 & sales_price_2025 < 50000000  & MKTTOT25 < 50000000), 
       aes(x = MKTTOT25, y = sales_price_2025)) +
  geom_point( alpha = 0.6) + #aes(color = buildable_area),
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  scale_x_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_y_continuous(labels = label_dollar(), limits = c(0, NA)) +
  #scale_color_viridis_c(name = "Buildable\nArea (sq ft)") +
  labs(
    x = "Assessor Market Value (2025)",
    y = "Sales Price (2025 $)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggplot(bk_inland %>% filter(!is.na(ratio_sale_to_mkt)& sales_price_2025 > 50000 & PARCEL_SQFT > 80 & sales_price_2025 < 50000000 & ratio_sale_to_mkt < 5), 
       aes(x = ratio_sale_to_mkt)) +
  geom_histogram(bins = 50, fill = "grey",color="black", alpha = 0.7) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(ratio_sale_to_mkt, na.rm = TRUE)), 
             color = "darkblue", linetype = "dashed", size = 1) +
  labs(
    subtitle = "Red line = perfect assessment (1.0), Blue line = median ratio",
    x = "Sales Price / Assessed Value",
    y = "Count",
    caption = "Values > 1 indicate underassessment, < 1 indicate overassessment"
  ) +
  theme_minimal()



#year of sale

ggplot(bk_setback_res_non0_noncpr_nobuild, aes(x = year_sale)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(
    x = "Year of Sale",
    y = "Number of Sales"
  ) +
  theme_classic()


#sales per parcel
bk_setback_res_non0_noncpr_nobuild <- bk_setback_res_non0_noncpr_nobuild %>%
  group_by(PARID) %>%
  summarise(n_sales = n()) %>%
  ungroup()

ggplot(bk_setback_res_non0_noncpr_nobuild, aes(x = n_sales)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(x = "Number of Times Sold",
    y = "Number of Parcels"
  ) +
  scale_x_continuous(breaks = seq(1, max(bk_setback_res_non0_noncpr_nobuild$n_sales), by = 1)) +
  theme_classic()



#buildable area
ggplot(assessors_setback_res_noncpr_nobuild, aes(x = buildable_area)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  labs(x = "Buildable Area (sq ft)",
    y = "Number of Parcels"
  ) +
  scale_x_continuous(labels = scales::comma) +
  theme_classic()


# building footprint
ggplot(assessors_setback_res_noncpr_nobuild, aes(x = largest_bldg_sqft)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  labs(x = "Building footprint (sq ft)",
       y = "Number of Parcels"
  ) +
  scale_x_continuous(labels = scales::comma) +
  theme_classic()


#sqft remaining after ahwwf
assessors_setback_res_noncpr_nobuild <- assessors_setback_res_noncpr_nobuild %>%
  left_join(assessors_hazard, by = "PARID")
assessors_setback_res_noncpr_nobuild$pct_wf05[is.na(assessors_setback_res_noncpr_nobuild$pct_wf05)] <- 0


ggplot(assessors_setback_res_noncpr_nobuild, aes(x = PARCEL_SQFT)) +
  geom_histogram(bins = 50,  fill = "grey", color = "black") +
  labs(x = "Parcel Area (sq ft)",
    y = "Number of Parcels"
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_classic()

ggplot(assessors_setback_res_noncpr_nobuild, aes(x = PARCEL_SQFT*(1-pct_wf05))) +
  geom_histogram(bins = 50, fill = "grey", color = "black") +
  labs(x = "Parcel Area after AHWF shoreline (sq ft)",
    y = "Number of Parcels"
  ) +
  scale_x_continuous(labels = scales::comma,limits=c(0,12500)) +
  scale_y_continuous(labels = scales::comma) +
  theme_classic()










# vacant lots 
assessors_setback_res_noncpr_nobuild_vacant <- assessors_setback_res %>%
  filter(is.na(n_buildings),
         !is.na(buildable_area), 
         buildable_area < 1000,
         PARCEL_SQFT > 80,
         !grepl("state|county|LOTT|robertson", OWN1, ignore.case = TRUE))
assessors_setback_res_noncpr_nobuild_vacant_parid <- unique(assessors_setback_res_noncpr_nobuild_vacant$PARID)

bk_setback_res_non0_noncpr_nobuild_vacant <- bk_setback_res %>%
  filter(PARID %in% assessors_setback_res_noncpr_nobuild_vacant_parid,
         !is.na(buildable_area), 
         !is.na(sales_price_2025),
         buildable_area < 1000,
         sales_price_2025 > 50000,
         sales_price_2025 < 50000000,
         PARCEL_SQFT.x > 80)



ggplot(bk_setback_res_non0_noncpr_nobuild_vacant, aes(x = sales_price_2025)) +
  geom_histogram(bins = 30, fill = "grey", alpha = 0.7,color="black") +
  geom_vline(aes(xintercept = median(sales_price_2025)), 
             color = "darkred", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    x = "Sales Price ($2025)",
    y = "Number of sales",
    caption = paste0("N = ", nrow(bk_setback_res_non0_noncpr_nobuild_vacant), 
                     " | Median = ", dollar(median(bk_setback_res_non0_noncpr_nobuild_vacant$sales_price_2025)))
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

ggplot(assessors_setback_res_noncpr_nobuild_vacant, aes(x = ASSDTOT25)) +
  geom_histogram(bins = 30, fill = "grey", alpha = 0.7,color="black") +
  geom_vline(aes(xintercept = median(ASSDTOT25)), 
             color = "darkred", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    x = "Assessed total value ($2025)",
    y = "Number",
    caption = paste0("N = ", nrow(assessors_setback_res_noncpr_nobuild_vacant), 
                     " | Median = ", dollar(median(assessors_setback_res_noncpr_nobuild_vacant$ASSDTOT25)))
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))














# ag plots
bk_ag
bk_agCPR

assessors_ag
assessors_agCPR



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

# value
#sqft
#sales

ggplot(bk_ag %>%filter(!is.na(MKTTOT25) & !is.na(sales_price_2025) & sales_price_2025 > 50000 & sales_price_2025 < 50000000), 
       aes(x = MKTTOT25, y = sales_price_2025,color=PARCEL_SQFT)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_continuous(labels = dollar_format(),limits=c(0,20000000)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis_c(option = "plasma",limits = c(0,10000000)) +
  labs(x = "Assessor Market Value (2025 $)",
       y = "Sales Price (2025 $)") +
  theme_classic()

ggplot(bk_agCPR %>%filter(!is.na(MKTTOT25) & !is.na(sales_price_2025)& sales_price_2025 > 50000 & sales_price_2025 < 50000000), 
       aes(x = MKTTOT25, y = sales_price_2025,color=PARCEL_SQFT)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_continuous(labels = dollar_format(),limits=c(0,15000000)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis_c(option = "plasma",limits = c(0,10000000)) +
  labs(x = "Assessor Market Value (2025 $)",
       y = "Sales Price (2025 $)") +
  theme_classic()

median((bk_ag %>% filter(sales_price_2025 > 50000 & sales_price_2025 < 50000000))$sales_price_2025, na.rm = T)
median((bk_ag %>% filter(sales_price_2025 > 50000 & sales_price_2025 < 50000000))$sales_price_2025 / 
         (bk_ag %>% filter(sales_price_2025 > 50000 & sales_price_2025 < 50000000))$PARCEL_SQFT, na.rm = T)

median((bk_agCPR %>% filter(sales_price_2025 > 50000 & sales_price_2025 < 50000000))$sales_price_2025, na.rm = T)
median((bk_agCPR %>% filter(sales_price_2025 > 50000 & sales_price_2025 < 50000000))$sales_price_2025 / 
         (bk_agCPR %>% filter(sales_price_2025 > 50000 & sales_price_2025 < 50000000))$PARCEL_SQFT, na.rm = T)



assessors_agCPR
assessors_ag
ggplot(assessors_ag ,aes(x = PARCEL_SQFT, y = MKTTOT25)) +
  geom_point(alpha = 0.4) +
  scale_x_continuous(labels = comma_format(),limits = c(0,300000000)) +
  scale_y_continuous(labels = dollar_format(),limits = c(0,50000000)) +
  labs(x = "Parcel sqft",
       y = "Assessor Market Value ($2025)") +
  theme_classic()

ggplot(assessors_agCPR ,aes(x = PARCEL_SQFT, y = MKTTOT25)) +
  geom_point(alpha = 0.4) +
  scale_x_continuous(labels = comma_format()) +
  scale_y_continuous(labels = dollar_format(),limits = c(0,50000000)) +
  labs(x = "Parcel sqft",
       y = "Assessor Market Value ($2025)") +
  theme_classic()

median(assessors_ag$MKTTOT25, na.rm = T)
median(assessors_ag$MKTTOT25 / assessors_ag$PARCEL_SQFT, na.rm = T)

median(assessors_agCPR$MKTTOT25, na.rm = T)
median(assessors_agCPR$MKTTOT25 / assessors_agCPR$PARCEL_SQFT, na.rm = T)




ggplot(bk_agCPR %>%filter(!is.na(MKTTOT25) & !is.na(sales_price_2025)& sales_price_2025 > 50000 & sales_price_2025 < 50000000), 
       aes(x = PARCEL_SQFT, y = sales_price_2025)) +
  geom_point(alpha = 0.4) +
  scale_x_continuous(labels = comma_format()) +
  scale_y_continuous(labels = dollar_format(),limits = c(0,50000000)) +
  labs(x = "Parcel sqft",
       y = "Sales price ($2025)") +
  theme_classic()

ggplot(assessors_agCPR, aes(x = PARCEL_SQFT)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  labs(x = "Parcel sqft",
       y = "Number of Parcels"
  ) +
  scale_x_continuous(labels = scales::comma,limits=c(0,5000000)) +
  theme_classic()










st_write(assessors_setback_res_noncpr_nobuild,"assessors_setback_res_noncpr_nobuild.shp")

assessors_setback_res_noncpr_nobuild %>%
  select(TMK, geometry, PARID,LD_SETBK_F,pct_wf05,buildable_area,AVE_DEP_FT,AVE_WID_FT) %>%
  st_write("assessors_setback_res_noncpr_nobuild1.shp",append=FALSE,delete_dsn = TRUE)



### OLD 


# quick figures for setback parcels
bk_setback$date_sale <- as.Date(as.character(bk_setback$contract_date), format = "%Y%m%d")

# parcels that have been sold
sale_parid <- unique(bk_setback$PARID[!is.na(bk_setback$PARID)])

assessors_setback <- assessors_setback %>%
  mutate(recent_sale = ifelse(PARID %in% sale_parid, 1, 0)) 

#filter residential
# exclude Agricultural, Hotel and Resort, Commercial, Conservation, Industrial
residential <- c("Owner-Occupied", "Vacation Rental","Non-Owner-Occupied Residential","Owner-Occupied Mixed Use")  
bk_setback_res <- bk_setback[bk_setback$TAXCLASS25 %in% residential, ]
assessors_setback <- assessors_setback %>%
  mutate(is_residential = ifelse(TAXCLASS25 %in% residential, "Residential", "Non-Residential"))

#filter residential non-CPR
cotmk_counts <- bk_setback %>%
  group_by(COTMK) %>%
  summarise(unique_parid_count = n_distinct(PARID, na.rm = TRUE), .groups = 'drop') %>%
  filter(unique_parid_count < 5)
bk_setback_res_ncpr <- bk_setback[bk_setback$TAXCLASS25 %in% residential & bk_setback$COTMK %in% cotmk_counts$COTMK, ]











# tax classes

# Count TMKs by tax class
taxclass_counts <- assessors_setback %>%
  group_by(TAXCLASS25) %>%
  summarise(count = n_distinct(COTMK)) %>%
  arrange(desc(count))

ggplot(taxclass_counts, aes(x = factor(TAXCLASS25), y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Tax Class",
       y = "Number of TMKs") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

assessors_setback <- assessors_setback %>%
  group_by(COTMK) %>%
  mutate(cpr = n() > 5) %>%
  ungroup()

# Count CPR'd TMKs by tax class
taxclass_cpr<- assessors_setback %>%
  filter(cpr == TRUE) %>%
  group_by(TAXCLASS25) %>%
  summarise(unique_tmks = n_distinct(COTMK)) %>%
  arrange(desc(unique_tmks))

# Count non-CPR'd TMKs by tax class
taxclass_ncpr <- assessors_setback %>%
  filter(cpr == FALSE) %>%
  group_by(TAXCLASS25) %>%
  summarise(unique_tmks = n_distinct(COTMK)) %>%
  arrange(desc(unique_tmks))

taxclass_cprs <- bind_rows(
  taxclass_cpr %>% mutate(type = "CPR"),
  taxclass_ncpr %>% mutate(type = "Non-CPR")
)

ggplot(taxclass_cprs, aes(x = factor(TAXCLASS25), y = unique_tmks, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("CPR" = "lightgrey", "Non-CPR" = "darkgrey")) +
  labs(x = "Tax Class",
       y = "Count of TMKs",
       fill = "Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#sales metrics
# Calculate sale metrics for each PARID
sale_metrics <- bk_setback %>%
  filter(!is.na(PARID) & !is.na(date_sale)) %>%
  group_by(PARID) %>%
  summarise(
    sale_frequency = n(),  # Count of sales per parcel
    most_recent_sale_date = max(date_sale, na.rm = TRUE),  # Most recent sale date
    most_recent_sale_year = year(max(date_sale, na.rm = TRUE)),  # Year of most recent sale
    most_recent_sale_price = mean_sale_price[which.max(date_sale)][1],  # Price of most recent sale
    .groups = 'drop'
  )
assessors_setback <- assessors_setback %>%
  left_join(sale_metrics, by = "PARID") %>%
  mutate(
    # Replace NA with 0 for parcels that haven't been sold
    sale_frequency = ifelse(is.na(sale_frequency), 0, sale_frequency),
    recent_sale = ifelse(PARID %in% sale_parid, 1, 0)
  )


#sales frequency
ggplot(assessors_setback %>% filter(sale_frequency > 0), 
       aes(x = sale_frequency, fill = TAXCLASS25)) +
  geom_density(alpha = 0.5) +
  labs(x = "Number of Sales per Parcel",
       y = "Density",
       fill = "Tax Class") +
  theme_classic()




#most recent sale year

ggplot(assessors_setback %>% filter(!is.na(most_recent_sale_year)), 
       aes(x = factor(most_recent_sale_year))) +
  geom_bar() +
  labs(x = "Year of Most Recent Sale",
       y = "Number of Parcels") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#most recent sale price

ggplot(assessors_setback %>% filter(!is.na(most_recent_sale_price)), 
       aes(x = most_recent_sale_price)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Sale Price",
       y = "Number of Parcels") +
  theme_classic()



#sales price

all_tax_classes <- unique(bk_setback$TAXCLASS25)
tax_colors <- setNames(
  scales::hue_pal()(length(all_tax_classes)),
  all_tax_classes
)

p1 <- ggplot(bk_setback%>% filter(mean_sale_price > 0),  aes(x = date_sale, y = mean_sale_price, color = TAXCLASS25)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(values = tax_colors, drop = FALSE) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("10 years")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title='All',
       x = "Contract date",
       y = "Sale price") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(bk_setback_res%>% filter(mean_sale_price > 0), aes(x = date_sale, y = mean_sale_price, color = TAXCLASS25)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(values = tax_colors, drop = FALSE) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("10 years")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title='Residential',
       x = "Contract date",
       y = "Sale price") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(bk_setback_res_ncpr%>% filter(mean_sale_price > 0),  aes(x = date_sale, y = mean_sale_price, color = TAXCLASS25)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(values = tax_colors, drop = FALSE) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("10 years")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title='Residential non-CPR',
       x = "Contract date",
       y = "Sale price") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p1 + p2 + p3 + 
  plot_layout(guides = "collect")




#mean sales price over time

# yearly mean 
bk_setback_summary <- bk_setback %>%
  mutate(year = year(date_sale)) %>%
  group_by(year) %>%
  summarise(mean_price = median(mean_sale_price, na.rm = TRUE),
            n = n())
bk_setback_res_summary <- bk_setback_res %>%
  mutate(year = year(date_sale)) %>%
  group_by(year) %>%
  summarise(mean_price = median(mean_sale_price, na.rm = TRUE),
            n = n())
bk_setback_res_ncpr_summary <- bk_setback_res_ncpr %>%
  mutate(year = year(date_sale)) %>%
  group_by(year) %>%
  summarise(mean_price = median(mean_sale_price, na.rm = TRUE),
            n = n())

p_trend <- ggplot() +
  # geom_line(data = bk_setback_summary, 
  #           aes(x = year, y = mean_price, color = "All"), 
  #           linewidth = 1) +
  geom_line(data = bk_setback_res_summary, 
            aes(x = year, y = mean_price, color = "Residential"), 
            linewidth = 1) +
  geom_line(data = bk_setback_res_ncpr_summary, 
            aes(x = year, y = mean_price, color = "Residential non-CPR"), 
            linewidth = 1) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_manual(values = c("All" = "#E41A1C", 
                                "Residential" = "#377EB8", 
                                "Residential non-CPR" = "#4DAF4A")) +
  labs(x = "Year",
       y = "Median Sale Price",
       color = "Category") +
  theme_classic()




#parcel sales frequency

sale_frequency <- bk_setback %>%
  count(PARID) %>%  
  count(n, name = "num_parcels") %>%
  rename(times_sold = n)

sale_frequency_res <- bk_setback_res %>%
  count(PARID) %>%  
  count(n, name = "num_parcels") %>%
  rename(times_sold = n)

sale_frequency_res_ncpr <- bk_setback_res_ncpr %>%
  count(PARID) %>%  
  count(n, name = "num_parcels") %>%
  rename(times_sold = n)

# Create histogram
p_freq <- ggplot(sale_frequency, aes(x = times_sold, y = num_parcels)) +
  geom_col(fill = "grey", alpha = 0.8,color="black") +
  scale_x_continuous(breaks = seq(0, max(sale_frequency$times_sold), by = 1)) +
  labs(title="All",
       x = "Number of Times Sold",
       y = "Number of Parcels") +
  theme_classic()

p_freq_res <- ggplot(sale_frequency_res, aes(x = times_sold, y = num_parcels)) +
  geom_col(fill = "grey", alpha = 0.8,color="black") +
  scale_x_continuous(breaks = seq(0, max(sale_frequency_res$times_sold), by = 1)) +
  labs(title="Residential",
       x = "Number of Times Sold",
       y = "Number of Parcels") +
  theme_classic()

p_freq_res_ncpr <- ggplot(sale_frequency_res_ncpr, aes(x = times_sold, y = num_parcels)) +
  geom_col(fill = "grey", alpha = 0.8,color="black") +
  scale_x_continuous(breaks = seq(0, max(sale_frequency_res_ncpr$times_sold), by = 1)) +
  labs(title="Residential non-CPR",
       x = "Number of Times Sold",
       y = "Number of Parcels") +
  theme_classic()

p_freq + p_freq_res + p_freq_res_ncpr












#sales price vs market value
assessors_setback$mktvalue_saleprice <- assessors_setback$MKTTOT25 - assessors_setback$most_recent_sale_price

ggplot(assessors_setback %>% 
         filter(!is.na(most_recent_sale_price) & 
                  !is.na(MKTTOT25) & 
                  TAXCLASS25 %in% residential), 
       aes(x = MKTTOT25, y = most_recent_sale_price)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  coord_equal(xlim = c(0, 30000000), ylim = c(0, 30000000)) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "Market Total Value (MKTTOT25)",
       y = "Most Recent Sale Price") +
  theme_classic()






#lot size
p1 <- ggplot(bk_setback %>% 
               distinct(PARID, .keep_all = TRUE) %>%
               filter(lot_size_sqft > 0 & !is.na(lot_size_sqft)), aes(x = lot_size_sqft)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", linewidth = 0.2) +
  scale_x_continuous(labels = comma_format()) +
  xlim(0, 1000000) +
  labs(title='All',
       x = "Parcel/lot size (sqft)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(bk_setback_res %>% 
               distinct(PARID, .keep_all = TRUE) %>%
               filter(lot_size_sqft > 0 & !is.na(lot_size_sqft)), aes(x = lot_size_sqft)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", linewidth = 0.2) +
  scale_x_continuous(labels = comma_format()) +
  xlim(0, 1000000) +
  labs(title='Residential',
       x = "Parcel/lot size (sqft)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(bk_setback_res_ncpr %>% 
               distinct(PARID, .keep_all = TRUE) %>%
               filter(lot_size_sqft > 0 & !is.na(lot_size_sqft)), aes(x = lot_size_sqft)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", linewidth = 0.2) +
  scale_x_continuous(labels = comma_format()) +
  xlim(0, 1000000) +
  labs(title='Residential non-CPR',
       x = "Parcel/lot size (sqft)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p1 | p2 | p3



#building size
p1 <- ggplot(bk_setback %>% 
               distinct(PARID, .keep_all = TRUE) %>%
               filter(lot_size_sqft > 0 & !is.na(lot_size_sqft)), aes(x = bldg_area)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", linewidth = 0.2) +
  scale_x_continuous(labels = comma_format()) +
  labs(title='All',
       x = "Building area (sqft)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(bk_setback_res %>% 
               distinct(PARID, .keep_all = TRUE) %>%
               filter(lot_size_sqft > 0 & !is.na(lot_size_sqft)), aes(x = bldg_area)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", linewidth = 0.2) +
  scale_x_continuous(labels = comma_format()) +
  labs(title='Residential',
       x = "Building area (sqft)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(bk_setback_res_ncpr %>% 
               distinct(PARID, .keep_all = TRUE) %>%
               filter(lot_size_sqft > 0 & !is.na(lot_size_sqft)), aes(x = bldg_area)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", linewidth = 0.2) +
  scale_x_continuous(labels = comma_format()) +
  labs(title='Residential non-CPR',
       x = "Building area (sqft)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p1 | p2 | p3


#sale frequency
sale_freq_setback <- bk_setback %>%
  filter(!is.na(PARID) & !is.na(date_sale)) %>%
  group_by(PARID) %>%
  summarise(
    sale_count = n_distinct(date_sale, na.rm = TRUE),
    TAXCLASS25 = first(TAXCLASS25),
    COTMK = first(COTMK),
    .groups = 'drop'
  ) %>%
  filter(sale_count > 0)

sale_freq_setback_res <- sale_freq_setback %>%
  filter(TAXCLASS25 %in% residential)

sale_freq_setback_res_ncpr <- sale_freq_setback_res %>%
  filter(COTMK %in% cotmk_counts$COTMK)

p1 <- ggplot(sale_freq_setback, aes(x = sale_count)) +
  geom_bar(fill = "grey", alpha = 0.7, color = "black", linewidth = 0.2,width=0.8) +
  scale_x_continuous(breaks = seq(1, max(sale_freq_setback$sale_count), by = 1)) +
  scale_y_continuous(labels = comma_format()) +
  labs(title='All',
       x = "Number of sales per parcel",
       y = "Count of parcels") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(sale_freq_setback_res, aes(x = sale_count)) +
  geom_bar(fill = "grey", alpha = 0.7, color = "black", linewidth = 0.2,width=0.8) +
  scale_x_continuous(breaks = seq(1, max(sale_freq_setback$sale_count), by = 1)) +
  scale_y_continuous(labels = comma_format()) +
  labs(title='Residential',
       x = "Number of sales per parcel",
       y = "Count of parcels") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(sale_freq_setback_res_ncpr, aes(x = sale_count)) +
  geom_bar(fill = "grey", alpha = 0.7, color = "black", linewidth = 0.2,width=0.8) +
  scale_x_continuous(breaks = seq(1, max(sale_freq_setback$sale_count), by = 1)) +
  scale_y_continuous(labels = comma_format()) +
  labs(title='Residential non-CPR',
       x = "Number of sales per parcel",
       y = "Count of parcels") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p1 | p2 | p3




#value $2025

p1 <- ggplot(assessors_setback , aes(x = MKTTOT25)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", size = 0.2) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "All ",
       x = "Total market value ($2025)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5))

p2 <- ggplot(assessors_setback %>% filter(TAXCLASS25 %in% residential), aes(x = MKTTOT25)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", size = 0.2) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Residential ",
       x = "Total market value ($2025)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5))

p3 <- ggplot(assessors_setback %>% filter(TAXCLASS25 %in% residential & COTMK %in% cotmk_counts$COTMK), aes(x = MKTTOT25)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", size = 0.2) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Residential non-CPR ",
       x = "Total market value ($2025)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5))



(p1 | p2 | p3) 

