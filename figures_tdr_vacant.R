library(ggplot2)
library(scales)
library(patchwork)
library(stargazer)





#sales of vacant parcels
sending_vacant <- assessors_setback_res_noncpr_nobuild_nogov[is.na(assessors_setback_res_noncpr_nobuild_nogov$n_buildings),]
sending_vacant_tmk <- unique(sending_vacant$PARID)
bk_sending_vacant <- bk_assessors_setback_res_noncpr_nobuild_nogov[is.na(bk_assessors_setback_res_noncpr_nobuild_nogov$n_buildings),]
bk_sending_vacant <- bk_sending_vacant %>% filter(sales_price_2025 < 20000000 & PARID %in% sending_vacant_tmk)

#sale frequency
bk_sending_vacant %>%
  count(PARID, name = "n_sales") %>%
  count(n_sales, name = "n_parcels") %>%
  ggplot(aes(x = factor(n_sales), y = n_parcels)) +
  geom_col() +
  geom_text(aes(label = n_parcels), vjust = -0.4, size = 3.5) +
  labs(
    title = "Sale frequency from 1994 to 2023",
    x = "Number of times sold",
    y = "Number of parcels"
  ) +
  theme_classic()

# sale price over time
bk_sending_vacant %>%
  filter(!is.na(sales_price_2025), !is.na(year_sale)) %>%
  ggplot(aes(x = year_sale, y = sales_price_2025)) +
  geom_point(alpha = 0.6, color = "black", size = 2) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Parcel sale prices ($2025)",
    x = "Year",
    y = "Sale price ($2025)"
  ) +
  theme_classic()

#sales v assessors
bk_sending_vacant %>%
  filter(!is.na(sales_price_2025), !is.na(MKTTOT25),sales_price_2025>50000) %>%
  ggplot(aes(x = MKTTOT25, y = sales_price_2025)) +
  geom_point(alpha = 0.6, color = "black", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(
    x = "Assessed Total Market Value (2025$)",
    y = "Sale Price (2025$)"
  ) +
  theme_classic()

# assessors val vacant
ggplot(sending_vacant, aes(x = MKTTOT25)) +
  geom_histogram(color="white",bins = 30) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Total Market Value ($2025)",
    y = "Number of parcels"
  ) +
  theme_classic()


#1.9 multiplier

# hist of multipliers (sales : assessed ratio)
ggplot(sales_calibration_send_vacant, aes(x = ratio)) +
  geom_histogram(bins = 30, fill = "darkgrey", color = "white") +
  geom_vline(aes(xintercept = median(ratio, na.rm = TRUE)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = median(sales_calibration_send_vacant$ratio) + 0.5, 
           y = Inf, label = "median", color = "red", vjust = 2, hjust = 0) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  labs(x = "Sales Price / Assessed Value Ratio", y = "Count") +
  theme_classic()

#outliers removed
ggplot(sales_calibration_send_vacant_clean, aes(x = ratio)) +
  geom_histogram(bins = 30, fill = "darkgrey", color = "white") +
  geom_vline(aes(xintercept = median(ratio, na.rm = TRUE)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = median(sales_calibration_send_vacant_clean$ratio) + 0.5, 
           y = Inf, label = paste0("median: ", round(median(sales_calibration_send_vacant_clean$ratio, na.rm = TRUE), 2)),
           color = "red", vjust = 2, hjust = 0) +
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  labs(x = "Sales Price / Assessed Value Ratio", y = "Count") +
  theme_classic()

# 1.9 line
ggplot(sales_calibration_send_vacant, aes(x = MKTTOT25, y = sales_price_2025)) +
  geom_point() +
  geom_abline(aes(slope = sending_calibration_ratio, intercept = 0, color = "1.9x multiplier"), 
              linetype = "dashed", linewidth = 1) +
  geom_abline(aes(slope = 1, intercept = 0, color = "1:1"), 
              linetype = "dashed", linewidth = 1) +
  scale_color_manual(name = NULL, values = c("1.9x multiplier" = "blue", "1:1" = "red")) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Assessed Value", y = "Sales Price (2025 $)") +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2))

#histogram of calibrated sending values
ggplot(sending_vacant, aes(x = MKTTOT25_calibrated)) +
  geom_histogram(bins = 30, fill = "darkgrey", color = "white") +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Calibrated Market Value", y = "Count") +
  theme_classic()






#ag calibration

#model 1 - acreage to assessed value
pred_purchase <- data.frame(MKTTOT25 = seq(0, 15000000, length.out = 200)) %>%
  mutate(sales_price_2025 = predict(calibration_model_vacant, newdata = .))

ggplot(sales_calibration_ag, aes(x = MKTTOT25, y = sales_price_2025)) +
  geom_point(alpha = 0.3) +
  geom_line(data = pred_purchase, aes(x = MKTTOT25, y = sales_price_2025),
            color = "blue", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(labels = scales::dollar, limits = c(0, 10000000)) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 10000000)) +
  labs(title = "Purchase: Ag Vacant", x = "Assessed Value (MKTTOT25)", y = "Sales Price (2025 $)") +
  theme_classic()



#model 2 - assessed value to sale value
#A: acres → assessed value
pred_acres <- data.frame(TAXACRES = seq(0, 100, length.out = 200)) %>%
  mutate(MKTTOT25 = predict(ag_acre_vacant_mkt_model, newdata = .))
ggplot(assessors_agCPR_vacant %>% filter(TAXACRES < 100, MKTTOT25 < 5000000), 
             aes(x = TAXACRES, y = MKTTOT25)) +
  geom_point(alpha = 0.3) +
  geom_line(data = pred_acres, aes(x = TAXACRES, y = MKTTOT25),
            color = "blue", linewidth = 1) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Sale Part A: Acres → Assessed Value",
       x = "Acres", y = "Assessed Value (MKTTOT25)") +
  theme_classic()


# B: assessed -> sale
pred_sale <- data.frame(MKTTOT25 = seq(0, 10000000, length.out = 200)) %>%
  mutate(sales_price_2025 = predict(calibration_model_agCPR, newdata = .))

ggplot(sales_calibration_agCPR, aes(x = MKTTOT25, y = sales_price_2025)) +
  geom_point(alpha = 0.3) +
  geom_line(data = pred_sale, aes(x = MKTTOT25, y = sales_price_2025),
            color = "blue", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Sale Part B: Assessed Value → Sales Price",
       x = "Assessed Value (MKTTOT25)", y = "Sales Price (2025 $)") +
  theme_classic()






# tdr dynamic pricing on vacant parcels

ggplot(tdr_split_allocation_realprice_vacant $transactions, aes(x = total_credit_cost/1e6, y = marginal_roi_gain)) +
  geom_point(aes(color = transaction_number), size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_viridis_c(name = "Transaction\nOrder") +
  scale_x_continuous(labels = dollar_format(suffix = "M")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    subtitle = paste(tdr_split_allocation_realprice_vacant $summary$credits_allocated, "credits allocated"),
    x = "Total Credit Cost",
    y = "Marginal ROI Gain"
  ) +
  theme_classic()

#tdr roi comparison - dynamic split
ggplot(tdr_split_allocation_realprice_vacant$transactions,
       aes(x = current_roi, y = next_roi)) +
  geom_point(aes(color = transaction_number), alpha = 0.7, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray30", linetype = "dotted") +
  geom_vline(xintercept = 0, color = "gray30", linetype = "dotted") +
  scale_color_viridis_c(name = "Transaction\nOrder") +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = "Baseline ROI",
    y = "TDR ROI"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

tdr_split_credits_per_parcel_real_vacant <- tdr_split_allocation_realprice_vacant$transactions %>%
  group_by(receiving_TMK) %>%
  summarise(n_credits = n(), total_paid = sum(total_credit_cost))
ggplot(tdr_split_credits_per_parcel_real_vacant, aes(x = reorder(receiving_TMK, -n_credits), y = n_credits)) +
  geom_col(aes(fill = total_paid/1e6)) +
  scale_fill_viridis_c(name = "Total Paid\n($M)", option = "plasma") +
  labs(x = "Receiving Parcel", y = "Credits") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

#acreage of receiving parcels
ggplot(tdr_split_allocation_realprice_vacant$transactions, aes(x = receiving_acres)) +
  geom_histogram(bins = 20, fill = "darkgrey", color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Receiving Parcel Acres", y = "Count") +
  theme_classic()




