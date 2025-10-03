# coastal analysis figures


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

#filter residential non-CPR
cotmk_counts <- bk_setback %>%
  group_by(COTMK) %>%
  summarise(unique_parid_count = n_distinct(PARID, na.rm = TRUE), .groups = 'drop') %>%
  filter(unique_parid_count < 5)
bk_setback_res_ncpr <- bk_setback[bk_setback$TAXCLASS25 %in% residential & bk_setback$COTMK %in% cotmk_counts$COTMK, ]












#sales price
p1 <- ggplot(bk_setback, aes(x = date_sale, y = mean_sale_price)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("10 years")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title='All',
       x = "Contract date",
       y = "Sale price") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(bk_setback_res, aes(x = date_sale, y = mean_sale_price)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("10 years")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title='Residential',
       x = "Contract date",
       y = "Sale price") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(bk_setback_res_ncpr, aes(x = date_sale, y = mean_sale_price)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("10 years")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title='Residential non-CPR',
       x = "Contract date",
       y = "Sale price") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p1 | p2 | p3


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

p1 <- ggplot(assessors_setback %>% filter(recent_sale == 1), aes(x = MKTTOT25)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", size = 0.2) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "All - sold",
       x = "Total market value ($2025)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5))

p2 <- ggplot(assessors_setback %>% filter(recent_sale == 1 & TAXCLASS25 %in% residential), aes(x = MKTTOT25)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", size = 0.2) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Residential - sold",
       x = "Total market value ($2025)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5))

p3 <- ggplot(assessors_setback %>% filter(recent_sale == 1 & TAXCLASS25 %in% residential & COTMK %in% cotmk_counts$COTMK), aes(x = MKTTOT25)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", size = 0.2) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Residential non-CPR - sold",
       x = "Total market value ($2025)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5))

p4 <- ggplot(assessors_setback %>% filter(recent_sale == 0), aes(x = MKTTOT25)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", size = 0.2) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "All - never sold",
       x = "Total market value ($2025)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5))

p5 <- ggplot(assessors_setback %>% filter(recent_sale == 0& TAXCLASS25 %in% residential), aes(x = MKTTOT25)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", size = 0.2) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Residential - never sold",
       x = "Total market value ($2025)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5))

p6 <- ggplot(assessors_setback %>% filter(recent_sale == 0 & TAXCLASS25 %in% residential & COTMK %in% cotmk_counts$COTMK), aes(x = MKTTOT25)) +
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7, color = "black", size = 0.2) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Residential non-CPR - never sold",
       x = "Total market value ($2025)",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5))

(p1 | p2 | p3) / (p4 | p5 | p6)

