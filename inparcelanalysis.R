library(sf)
library(ggplot2)
library(dplyr)


csv_data2 <- read.csv(setback_parcel_file)
csv_data3 <- read.csv(setback_erosion_file, fileEncoding = "latin1")
csv_data3$builda_ft <- ((csv_data3$PAR_ar_f - csv_data3$Area_sqft)/(csv_data3$avgwidth)-10)*(csv_data3$avgwidth-10)
tmk_buildings <- read.csv(tmkbldg_file)

summarized_data <- tmk_buildings %>%
  group_by(TMK) %>%
  summarise(build_area = sum(GIS_SQFT, na.rm = TRUE))


# Left join csv_data2 with building based on CGG_INDEX
csv_data2 <- left_join(csv_data2, summarized_data %>% select(TMK, build_area), by = "TMK")
csv_data3 <- left_join(csv_data3, summarized_data %>% select(TMK, build_area), by = "TMK")

csv_data2$builda_ft[csv_data2$builda_ft < 0] <- 0
csv_data3$builda_ft[csv_data3$builda_ft < 0] <- 0

csv_data2$build_a_lost <- csv_data2$build_area.x - csv_data2$builda_ft
csv_data3$build_a_lost <- csv_data3$build_area - csv_data3$builda_ft

csv_data2$build_a_lost <- ifelse(csv_data2$builda_ft > csv_data2$build_area.x, 0, csv_data2$build_area.x - csv_data2$builda_ft)
csv_data3$build_a_lost <- ifelse(csv_data3$builda_ft > csv_data3$build_area, 0, csv_data3$build_area - csv_data3$builda_ft)

csv_data2$lost_perc <- csv_data2$build_a_lost / csv_data2$build_area.x*100
csv_data3$lost_perc <- csv_data3$build_a_lost / csv_data3$build_area*100
# Remove duplicate rows based on unique CGG_INDEX value
#merged_data <- merged_data[!is.na(merged_data$GIS_SQFT.x), ]
# Arrange the data by descending PAR_ar_f values
#merged_data <- merged_data %>%
#  arrange(desc(PAR_ar_f))

# Keep rows with distinct GIS_SQFT.x values, retaining the row with the highest PAR_ar_f value
#merged_data <- merged_data %>%
#  distinct(GIS_SQFT.x, .keep_all = TRUE)



#cleaned_data <- distinct(merged_data, CGG_INDEX, .keep_all = TRUE)
#cleaned_data$builda_ft[cleaned_data$builda_ft < 0] <- 0
#cleaned_data$build_a_lost <- cleaned_data$GIS_SQFT - cleaned_data$builda_ft 
#cleaned_data$build_a_lost[cleaned_data$build_a_lost < 0] <- 0
#cleaned_data$lost_perc <- cleaned_data$build_a_lost / cleaned_data$GIS_SQFT*100

#csv_data2 <- left_join(csv_data2, building %>% select(CGG_INDEX, GIS_SQFT), by = "CGG_INDEX")
#csv_data2 <- distinct(csv_data2, CGG_INDEX, .keep_all = TRUE)

csv_subset <- csv_data2[, c("builda_ft")]
csv_subset2 <- csv_data3[, c("builda_ft")]
csv_subset3 <- cleaned_data[, c("lost_perc")]
csv_subset4 <- cleaned_data[, c("build_a_lost")]

min_value <- min(csv_data2$builda_ft, na.rm = TRUE)
max_value <- max(csv_data2$builda_ft, na.rm = TRUE)

# Assuming csv_data2 is your data frame with a column named "builda_ft"
# Define custom breaks and labels
breaks <- c(-Inf, 1, 250, 500, 1000, 1500, 2000, 3000, 4000, 5000, Inf)
labels <- c("0", "1 - 250" , "250 - 500", "500 - 1000", "1000 - 1500", "1500 - 2000", "2000 - 3000", "3000 - 4000", "4000 - 5000", "> 5000")

breaks1 <- c(0, 20, 40, 60, 80, 99.99, 100)
labels1 <- c("0 - 20%", "20 - 40%", "40 - 60%", "60 - 80%", "80 - 99%", "100%")

breaks2 <- c(-Inf, 1, 500, 1000, 1500, 2000, 3000, 4000, 5000, 10000,Inf)
labels2 <- c("0", "1 - 500", "500 - 1000", "1000 - 1500", "1500 - 2000", "2000 - 3000", "3000 - 4000", "4000 - 5000", "5000 - 10000", "> 10000")

csv_data2 <- csv_data2[!is.na(csv_data2$lost_perc), ]
csv_data3 <- csv_data3[!is.na(csv_data3$lost_perc), ]
# Create factor variable with custom breaks
csv_data2$builda_ft_factor <- cut(csv_data2$builda_ft, breaks = breaks, labels = labels, include.lowest = TRUE)
csv_data3$builda_ft_factor <- cut(csv_data3$builda_ft, breaks = breaks, labels = labels, include.lowest = TRUE)
csv_data2$lost_perc_fac <- cut(csv_data2$lost_perc, breaks = breaks1, labels = labels1, include.lowest = TRUE)
csv_data3$lost_perc_fac <- cut(csv_data3$lost_perc, breaks = breaks1, labels = labels1, include.lowest = TRUE)
csv_data2$lost_area_fac <- cut(csv_data2$build_a_lost, breaks = breaks2, labels = labels2, include.lowest = TRUE)
csv_data3$lost_area_fac <- cut(csv_data3$build_a_lost, breaks = breaks2, labels = labels2, include.lowest = TRUE)



# Create histogram using ggplot2
plots <- ggplot(csv_data2, aes(x = builda_ft_factor)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + # Add counts on top of bars
  scale_x_discrete(drop = FALSE) +  # Retain all factor levels
  labs(title = "Buildable Area of Parcels", x = "Remaining Buildable Area (sq ft)", y = "Count") +
  theme_minimal()
print(plots)
# Create histogram using ggplot2
plots2 <- ggplot(csv_data3, aes(x = builda_ft_factor)) +
  geom_bar(fill = "lightcoral", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + # Add counts on top of bars
  scale_x_discrete(drop = FALSE) +  # Retain all factor levels
  labs(title = "Buildable Area of Parcels (Setback + CE)", x = "Buildable Area (sq ft)", y = "Count") +
  theme_minimal()
print(plots2)

plots3 <- ggplot(csv_data2, aes(x = lost_perc_fac)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + # Add counts on top of bars
  scale_x_discrete(drop = FALSE) +  # Retain all factor levels
  labs(title = "Building Size Lost", x = "Building Size Lost (%)", y = "Count") +
  theme_minimal()

print(plots3)
plots4 <- ggplot(csv_data3, aes(x = lost_perc_fac)) +
  geom_bar(fill = "lightcoral", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + # Add counts on top of bars
  scale_x_discrete(drop = FALSE) +  # Retain all factor levels
  labs(title = "Building Area Lost (Setback + CE)", x = "Building Size Lost (%)", y = "Count") +
  theme_minimal()
print(plots4)

plots5 <- ggplot(csv_data2, aes(x = lost_area_fac)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + # Add counts on top of bars
  scale_x_discrete(drop = FALSE) +  # Retain all factor levels
  labs(title = "Building Size Lost", x = "Lost Building Size (sq ft)", y = "Count") +
  theme_minimal()

print(plots5)

plots6 <- ggplot(csv_data3, aes(x = lost_area_fac)) +
  geom_bar(fill = "lightcoral", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + # Add counts on top of bars
  scale_x_discrete(drop = FALSE) +  # Retain all factor levels
  labs(title = "Building Size Lost (Setback + CE)", x = "Lost Building Size (sq ft)", y = "Count") +
  theme_minimal()
print(plots6)







csv_data2 <- merge(csv_data2, assessors_setback, by = "PARID", all.x = TRUE)

# Filter for rows where builda_ft == 0 and calculate median/mean of MKTTOT25
setback_nobuild <- csv_data2[csv_data2$builda_ft == 0 & !is.na(csv_data2$builda_ft), ]

# Calculate median and mean
median(setback_nobuild$MKTTOT25, na.rm = TRUE)
mean(setback_nobuild$MKTTOT25, na.rm = TRUE)



