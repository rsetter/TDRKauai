library(ggplot2)
library(scales)
library(patchwork)
library(stargazer)

#final report figures




# sending area

#sales v assessed data
ggplot(bk_assessors_setback_res_noncpr_nobuild_nogov %>% filter(!is.na(MKTTOT25) & sales_price_2025 > 50000 & PARCEL_SQFT > 80 & sales_price_2025 < 30000000), 
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

#calibrated market value
ggplot(assessors_setback_res_noncpr_nobuild_nogov, aes(x = MKTTOT25_calibrated)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") + #aes(color = buildable_area),
  scale_x_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_y_continuous( limits = c(0, NA)) +
  labs(
    x = "Calibrated market value ($2025)",
    y = "Count"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

# sales value
ggplot(bk_assessors_setback_res_noncpr_nobuild_nogov %>% filter(sales_price_2025 > 50000 & PARCEL_SQFT > 80 & sales_price_2025 < 30000000), aes(x = sales_price_2025)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") + #aes(color = buildable_area),
  scale_x_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_y_continuous( limits = c(0, NA)) +
  labs(
    x = "Sales price ($2025)",
    y = "Count"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))




# receiving area

#sales
ggplot(sales_calibration %>% filter(sales_price_2025 > 50000  & sales_price_2025 < 15000000), aes(x = sales_price_2025)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") + #aes(color = buildable_area),
  scale_x_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_y_continuous( limits = c(0, NA)) +
  #scale_color_viridis_c(name = "Buildable\nArea (sq ft)") +
  labs(
    x = "Sales price ($2025)",
    y = "Count"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

ggplot(assessors_agCPR_noIAL%>% filter( MKTTOT25 < 10000000), aes(x = MKTTOT25)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") + #aes(color = buildable_area),
  scale_x_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_y_continuous( limits = c(0, NA)) +
  labs(
    x = "Assessor's market value ($2025)",
    y = "Count"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

#assessed market value vs sales price
ggplot(bk_agCPR %>%filter(!is.na(MKTTOT25) & !is.na(sales_price_2025)& sales_price_2025 > 50000 & sales_price_2025 < 30000000 & MKTTOT25 < 30000000), 
       aes(x = MKTTOT25, y = sales_price_2025,color=PARCEL_SQFT)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis_c(option = "plasma",limits = c(0,10000000)) +
  labs(x = "Assessor Market Value (2025 $)",
       y = "Sales Price (2025 $)") +
  theme_classic()



#plot linear model

# Plot 1: Value vs Acres (controlling for building size at median)
median_bldg <- median(assessors_agCPR_noIAL$largest_bldg_sqft, na.rm = TRUE)
acres_range <- seq(min(assessors_agCPR_noIAL$TAXACRES, na.rm = TRUE),
                   max(assessors_agCPR_noIAL$TAXACRES, na.rm = TRUE), 
                   length.out = 100)
pred_acres <- data.frame(
  TAXACRES = acres_range,
  largest_bldg_sqft = median_bldg
)
pred_acres$predicted_mkt <- predict(ag_acredwel_mkt_model, newdata = pred_acres)
pred_acres$predicted_sales <- predict(calibration_model,newdata = data.frame(MKTTOT25 = pred_acres$predicted_mkt))

p1 <- ggplot(assessors_agCPR_noIAL, aes(x = TAXACRES, y = MKTTOT25)) +
  geom_point(alpha = 0.3) +
  geom_line(data = pred_acres, aes(y = predicted_mkt, color = "Assessor Value"), linewidth = 1.5) +
  geom_line(data = pred_acres, aes(y = predicted_sales, color = "Calibrated Sales Price"), linewidth = 1.5) +
  scale_color_manual(values = c("Assessor Value" = "red", "Calibrated Sales Price" = "green")) +
  scale_x_log10() +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(
    title = "Value vs Acres",
    subtitle = paste("Building size held at median:", scales::comma(median_bldg), "sqft"),
    x = "Acres (log scale)",
    y = "Value (2025 $)"
  ) +
  theme_classic()

# Plot 2: Value vs Building Size (controlling for acres at median)
median_acres <- median(assessors_agCPR_noIAL$TAXACRES, na.rm = TRUE)
bldg_range <- seq(min(assessors_agCPR_noIAL$largest_bldg_sqft, na.rm = TRUE),
                  max(assessors_agCPR_noIAL$largest_bldg_sqft, na.rm = TRUE),
                  length.out = 100)
pred_bldg <- data.frame(
  TAXACRES = median_acres,
  largest_bldg_sqft = bldg_range
)
pred_bldg$predicted_mkt <- predict(ag_acredwel_mkt_model, newdata = pred_bldg)
pred_bldg$predicted_sales <- predict(calibration_model, newdata = data.frame(MKTTOT25 = pred_bldg$predicted_mkt))

p2 <- ggplot(assessors_agCPR_noIAL, aes(x = largest_bldg_sqft, y = MKTTOT25)) +
  geom_point(alpha = 0.3) +
  geom_line(data = pred_bldg, aes(y = predicted_mkt, color = "Assessor Value"), linewidth = 1.5) +
  geom_line(data = pred_bldg, aes(y = predicted_sales, color = "Calibrated Sales Price"), linewidth = 1.5) +
  scale_color_manual(values = c("Assessor Value" = "red", "Calibrated Sales Price" = "green")) +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Value vs Building Size",
    subtitle = paste("Acres held at median:", round(median_acres, 1)),
    x = "Largest Building (sqft)",
    y = "Value (2025 $)"
  ) +
  theme_classic()

# Show both plots
p1 / p2







# tdr analysis 


#tdr roi comparison - single parcels
ggplot(tdr_allparcels %>% filter(!is.na(baseline_roi), !is.na(tdr_roi)),
       aes(x = baseline_roi, y = tdr_roi)) +
  geom_point(alpha = 0.3, size = 1.5, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray30", linetype = "dotted") +
  geom_vline(xintercept = 0, color = "gray30", linetype = "dotted") +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = "Baseline ROI",
    y = "TDR ROI",
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

#tdr roi comparison - split parcels
ggplot(tdr_split_allocation_1 $transactions, aes(x = total_credit_cost/1e6, y = marginal_roi_gain)) +
  geom_point(aes(color = transaction_number), size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_viridis_c(name = "Transaction\nOrder") +
  scale_x_continuous(labels = dollar_format(suffix = "M")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    subtitle = paste(tdr_split_allocation_1 $summary$credits_allocated, "credits allocated"),
    x = "Total Credit Cost",
    y = "Marginal ROI Gain"
  ) +
  theme_classic()
tdr_split_credits_per_parcel_avg <- tdr_split_allocation_1$transactions %>%
  group_by(receiving_TMK) %>%
  summarise(n_credits = n(), total_paid = sum(total_credit_cost))
ggplot(tdr_split_credits_per_parcel_avg, aes(x = reorder(receiving_TMK, -n_credits), y = n_credits)) +
  geom_col(aes(fill = total_paid/1e6)) +
  scale_fill_viridis_c(name = "Total Paid\n($M)", option = "plasma") +
  labs(x = "Receiving Parcel", y = "Credits") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggplot(tdr_split_allocation_realprice $transactions, aes(x = total_credit_cost/1e6, y = marginal_roi_gain)) +
  geom_point(aes(color = transaction_number), size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_viridis_c(name = "Transaction\nOrder") +
  scale_x_continuous(labels = dollar_format(suffix = "M")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Real Prices",
    subtitle = paste(tdr_split_allocation_realprice $summary$credits_allocated, "credits allocated"),
    x = "Total Credit Cost",
    y = "Marginal ROI Gain"
  ) +
  theme_classic()
tdr_split_credits_per_parcel_real <- tdr_split_allocation_realprice$transactions %>%
  group_by(receiving_TMK) %>%
  summarise(n_credits = n(), total_paid = sum(total_credit_cost))
ggplot(tdr_split_credits_per_parcel_real, aes(x = reorder(receiving_TMK, -n_credits), y = n_credits)) +
  geom_col(aes(fill = total_paid/1e6)) +
  scale_fill_viridis_c(name = "Total Paid\n($M)", option = "plasma") +
  labs(title = "Real Prices", x = "Receiving Parcel", y = "Credits") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

#tdr roi comparison - CPR development
ggplot(tdr_cpr_dev %>% filter(!is.na(baseline_roi), !is.na(tdr_roi)),
       aes(x = baseline_roi, y = tdr_roi)) +
  geom_point(alpha = 0.3, size = 1.5, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray30", linetype = "dotted") +
  geom_vline(xintercept = 0, color = "gray30", linetype = "dotted") +
  scale_x_continuous(labels = label_percent(), limits = c(-1, 1)) +
  scale_y_continuous(labels = label_percent(), limits = c(-1, 1)) +
  labs(
    x = "Baseline ROI",
    y = "TDR ROI",
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))




#tdr breakeven price - single parcels - tdr roi > baseline
ggplot(tdr_breakeven_single %>% filter(!is.na(breakeven_tdr_price)),
       aes(x = breakeven_tdr_price)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
  geom_vline(xintercept = sending_tdrcredit_per_unit, color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = label_dollar(), limits = c(-500000,3000000)) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Breakeven TDR price",
    y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

#tdr breakeven price - split parcels
ggplot(tdr_breakeven_split,aes(x = breakeven_price)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
  geom_vline(xintercept = sending_tdrcredit_per_unit, color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = label_dollar(), limits = c(-500000,3500000)) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Breakeven TDR price",
    y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

#tdr breakeven price - CPR development
ggplot(tdr_breakeven_cpr,aes(x = breakeven_tdr_price)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
  geom_vline(xintercept = sending_tdrcredit_per_unit, color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Breakeven TDR price",
    y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))






#tdr breakeven ratio - single parcels
ggplot(tdr_breakeven_ratio_single,aes(x = breakeven_density_ratio)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
  geom_vline(xintercept = 1, color = "red", linewidth = 1, linetype = "dashed") +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Breakeven TDR ratio",
    y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))


#tdr breakeven ratio - split
# ggplot(tdr_breakeven_ratio_split %>% filter(!is.na(breakeven_n_credits)),
#        aes(x = breakeven_n_credits)) +
#   geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
#   geom_vline(xintercept = 1, color = "red", linewidth = 1, linetype = "dashed") +
#   scale_y_continuous(labels = label_comma()) +
#   labs(
#     x = "Breakeven TDR ratio",
#     y = "Count") +
#   theme_classic() +
#   theme(plot.title = element_text(face = "bold"))


#tdr breakeven ratio - cpr
ggplot(tdr_breakeven_ratio_cpr %>% filter(!is.na(breakeven_density_ratio)),
       aes(x = breakeven_density_ratio)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
  geom_vline(xintercept = 1, color = "red", linewidth = 1, linetype = "dashed") +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Breakeven TDR ratio",
    y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))





#tdr breakeven construction cost - single parcel
ggplot(tdr_breakeven_construction %>% filter(!is.na(breakeven_construction_cost)),
       aes(x = breakeven_construction_cost)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
  geom_vline(xintercept = construction_cost, color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Breakeven construction cost",
    y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))


#tdr breakeven construction cost - split
ggplot(tdr_breakeven_construction_split %>% filter(!is.na(breakeven_construction_cost)),
       aes(x = breakeven_construction_cost)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
  geom_vline(xintercept = construction_cost, color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Breakeven construction cost",
    y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))



 #tdr breakeven construction cost - cpr dev
ggplot(tdr_breakeven_construction_cpr %>% filter(!is.na(breakeven_tdr_price)),
       aes(x = breakeven_tdr_price)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
  geom_vline(xintercept = construction_cost, color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = label_dollar(),limits=c(-10000000,5000000)) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Breakeven construction cost",
    y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))








# tdr breakeven sales cost - single parcel
ggplot(tdr_breakeven_selling,aes(x = required_premium_per_dwelling)) +
  geom_histogram(fill = "grey", alpha = 0.7, bins = 30, color = "white") +
  geom_vline(xintercept = 0, color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Breakeven additional sales revenue per dwelling",
    y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))


ggplot(tdr_breakeven_selling, aes(x = breakeven_analysis_predicted_revenue, 
                                       y = breakeven_analysis_breakeven_revenue)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +  # 45-degree line
  geom_point(aes(color = breakeven_analysis_required_premium > 0), alpha = 0.6) +
  scale_color_manual(values = c("TRUE" = "darkred", "FALSE" = "darkgreen"),
                     name = "") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar, limits = c(-2500000,750000000)) +
  labs(x = "Predicted revenue",
    y = "Required revenue for breakeven"
  ) +
  theme_classic()
