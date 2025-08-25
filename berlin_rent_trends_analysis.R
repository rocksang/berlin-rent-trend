```R
# Load required libraries
library(tidyverse)
library(sf)

# Define wohnlage_mapping
wohnlage_mapping <- tibble(
  plz = c("10115", "10997", "12555", "10707"),
  Wohnlage = c("Gute", "Mittlere", "Mittlere", "Gute"),
  place_name = c("Berlin-Mitte", "Kreuzberg", "Köpenick", "Wilmersdorf")
)

# Process flats data
flats <- read_csv("berlin_flats-till2020.csv", show_col_types = FALSE) %>%
  select(Region, Space, Rent) %>%
  mutate(
    Region = case_when(
      str_detect(Region, "Mitte") ~ "Mitte",
      str_detect(Region, "Kreuzberg") ~ "Kreuzberg",
      str_detect(Region, "Köpenick|Koepenick") ~ "Köpenick",
      str_detect(Region, "Wilmersdorf") ~ "Wilmersdorf",
      TRUE ~ Region
    ),
    Wohnfläche = "All Sizes"
  )

region_to_plz <- tibble(
  Region = c("Mitte", "Kreuzberg", "Köpenick", "Wilmersdorf"),
  plz = c("10115", "10997", "12555", "10707")
)

flats_with_plz <- flats %>%
  left_join(region_to_plz, by = "Region") %>%
  filter(!is.na(plz))

# Map Wohnlage and place_name, and compute rent per m²
flats_mapped <- flats_with_plz %>%
  left_join(wohnlage_mapping, by = "plz") %>%
  filter(!is.na(plz)) %>%
  group_by(plz, place_name, Wohnlage, Wohnfläche) %>%
  summarise(mean_rent = mean(Rent / Space, na.rm = TRUE), .groups = "drop")

# Historical Data
historical <- read_csv("historical_prices_berlin.csv", show_col_types = FALSE) %>%
  rename(
    price_35m2 = "Price Per m2 (35m2)",
    price_42.5m2 = "Price Per m2 (42.5m2)",
    percent_change = "% Change"
  ) %>%
  mutate(
    percent_change = parse_number(percent_change, na = c("N/A", "")),
    price_35_45m2 = (price_35m2 + price_42.5m2) / 2,
    Year = as.character(Year),
    Wohnfläche = "All Sizes"
  ) %>%
  select(Year, price_35_45m2, Wohnfläche, percent_change)

# Show historical data calculations
cat("\n=== Historical Data (Past Average) ===\n")
cat("Calculating price_35_45m2 as (price_35m2 + price_42.5m2) / 2\n")
print(historical %>% select(Year, price_35m2, price_42.5m2, price_35_45m2, percent_change))

trend <- historical %>%
  summarise(
    avg_change = mean(percent_change, na.rm = TRUE) / 100,
    last_year = max(as.numeric(Year)),
    last_35_45m2 = last(price_35_45m2)
  )

# Show trend calculations
cat("\n=== Trend Calculation for Projections ===\n")
cat("Average percent change (avg_change) = mean(percent_change) / 100\n")
cat("percent_change values:\n")
print(historical$percent_change)
cat("avg_change =", trend$avg_change, "\n")
cat("last_year =", trend$last_year, "\n")
cat("last_35_45m2 (2023 rent) =", trend$last_35_45m2, "\n")

# Create years_extended with plz values by repeating historical data for each plz
years_extended_base <- tibble(Year = as.character(2010:2026)) %>%
  left_join(historical %>% select(Year, price_35_45m2), by = "Year") %>%
  mutate(
    price_35_45m2 = case_when(
      Year <= "2023" ~ price_35_45m2,
      TRUE ~ trend$last_35_45m2 * (1 + trend$avg_change)^(as.numeric(Year) - trend$last_year)
    ),
    Wohnfläche = "All Sizes",
    Data_Source = "Past Average",
    Wohnlage = "Past Average"
  )

# Show projection calculations for 2024-2026
cat("\n=== Projections for Past Average (2024-2026) ===\n")
cat("Formula: price_35_45m2 = last_35_45m2 * (1 + avg_change)^(Year - last_year)\n")
for (year in 2024:2026) {
  year_str <- as.character(year)
  exponent <- as.numeric(year) - trend$last_year
  projected_rent <- trend$last_35_45m2 * (1 + trend$avg_change)^exponent
  cat("Year", year_str, ": last_35_45m2 * (1 + avg_change)^(", year, " - ", trend$last_year, ") = ",
      trend$last_35_45m2, " * (1 + ", trend$avg_change, ")^", exponent, " = ", projected_rent, "\n")
}

# Repeat historical data for each plz
years_extended <- expand_grid(
  plz = c("10115", "10997", "12555", "10707"),
  years_extended_base
) %>%
  left_join(tibble(
    plz = c("10115", "10997", "12555", "10707"),
    place_name = c("Berlin-Mitte", "Kreuzberg", "Köpenick", "Wilmersdorf")
  ), by = "plz") %>%
  select(plz, place_name, Data_Source, Wohnlage, Wohnfläche, Year, Rent = price_35_45m2)

# Mietspiegel 2024 Data
mietspiegel_all <- bind_rows(
  read_csv("einfache_wohnlage.csv", show_col_types = FALSE) %>% mutate(Wohnlage = "Einfache"),
  read_csv("mittlere_wohnlage.csv", show_col_types = FALSE) %>% mutate(Wohnlage = "Mittlere"),
  read_csv("gute_wohnlage.csv", show_col_types = FALSE) %>% mutate(Wohnlage = "Gute")
) %>%
  mutate(
    across(c("untere Spanne", "Mittelwert", "obere Spanne"), ~ as.numeric(str_replace(str_replace(., "€", ""), ",", "."))),
    Wohnfläche = str_trim(Wohnfläche)
  ) %>%
  group_by(Wohnlage) %>%
  summarise(Mittelwert = mean(Mittelwert, na.rm = TRUE),
            Wohnfläche = "All Sizes", .groups = "drop")

# Show Mietspiegel base values
cat("\n=== Mietspiegel Base Values (Einfache, Mittlere, Gute) ===\n")
cat("Mittelwert (average rent per m²) for each Wohnlage, averaged across Wohnfläche:\n")
print(mietspiegel_all)

# Create a data frame with all combinations of plz and Wohnlage
all_combinations <- expand_grid(
  plz = c("10115", "10997", "12555", "10707"),
  Wohnlage = c("Einfache", "Mittlere", "Gute"),
  Wohnfläche = "All Sizes"
) %>%
  left_join(tibble(
    plz = c("10115", "10997", "12555", "10707"),
    place_name = c("Berlin-Mitte", "Kreuzberg", "Köpenick", "Wilmersdorf")
  ), by = "plz")

# Join with flats_mapped to get mean_rent where available
all_combinations <- all_combinations %>%
  left_join(flats_mapped %>% select(plz, Wohnlage, mean_rent), by = c("plz", "Wohnlage"))

# Combine Data for Mietspiegel (2024-2026)
plz_geo <- st_read("plz.geojson") %>%
  filter(plz %in% c("10115", "10997", "12555", "10707"))

rent_by_plz <- plz_geo %>%
  right_join(all_combinations, by = "plz") %>%
  left_join(mietspiegel_all, by = c("Wohnlage", "Wohnfläche")) %>%
  mutate(
    Mittelwert = if_else(is.na(Mittelwert), mean_rent, Mittelwert),
    adj_2024 = mean(years_extended$Rent[years_extended$Year == "2024"], na.rm = TRUE) / mean(Mittelwert, na.rm = TRUE),
    adj_2025 = mean(years_extended$Rent[years_extended$Year == "2025"], na.rm = TRUE) / mean(Mittelwert, na.rm = TRUE),
    adj_2026 = mean(years_extended$Rent[years_extended$Year == "2026"], na.rm = TRUE) / mean(Mittelwert, na.rm = TRUE),
    `2024` = Mittelwert * adj_2024,
    `2025` = Mittelwert * adj_2025,
    `2026` = Mittelwert * adj_2026,
    Data_Source = "Mietspiegel"
  ) %>%
  pivot_longer(cols = c("2024", "2025", "2026"), names_to = "Year", values_to = "Rent") %>%
  select(plz, place_name, Data_Source, Wohnlage, Wohnfläche, Year, Rent)

# Show Mietspiegel adjustment calculations
cat("\n=== Mietspiegel Adjustments for 2024-2026 ===\n")
cat("Adjustment factors: adj_year = mean(years_extended$Rent[Year == year]) / mean(Mittelwert)\n")
cat("mean(Mittelwert) =", mean(rent_by_plz$Mittelwert, na.rm = TRUE), "\n")
cat("mean(years_extended$Rent[Year == 2024]) =", mean(years_extended$Rent[years_extended$Year == "2024"], na.rm = TRUE), "\n")
cat("adj_2024 =", rent_by_plz$adj_2024[1], "\n")
cat("mean(years_extended$Rent[Year == 2025]) =", mean(years_extended$Rent[years_extended$Year == "2025"], na.rm = TRUE), "\n")
cat("adj_2025 =", rent_by_plz$adj_2025[1], "\n")
cat("mean(years_extended$Rent[Year == 2026]) =", mean(years_extended$Rent[years_extended$Year == "2026"], na.rm = TRUE), "\n")
cat("adj_2026 =", rent_by_plz$adj_2026[1], "\n")
cat("Adjusted rents: year = Mittelwert * adj_year\n")
rent_by_plz_sample <- rent_by_plz %>%
  filter(Year == "2024") %>%
  select(Wohnlage, Mittelwert, `2024`, `2025`, `2026`)
cat("Sample of adjusted rents for 2024 (one row per Wohnlage):\n")
print(rent_by_plz_sample %>% distinct(Wohnlage, .keep_all = TRUE))

# Combine with Historical Data
final_data <- bind_rows(
  rent_by_plz,
  years_extended
)

# Drop the geometry column to avoid issues with group_by
final_data <- st_drop_geometry(final_data)

# Aggregate for Single Chart (average across all PLZ)
rent_aggregated <- final_data %>%
  filter(!is.na(Rent)) %>%
  group_by(Year, Data_Source, Wohnlage) %>%
  summarise(Avg_Rent = mean(Rent, na.rm = TRUE), .groups = "drop") %>%
  filter(!(Data_Source == "Past Average" & Year %in% c("2024", "2025", "2026")))

# Show final aggregated values
cat("\n=== Final Aggregated Values for Chart ===\n")
cat("Averaging Rent across all PLZ codes for each Year and Wohnlage:\n")
print(rent_aggregated)

# Create the plot
rent_plot <- ggplot(rent_aggregated, aes(x = Year, y = Avg_Rent, fill = Wohnlage, color = Wohnlage)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_line(aes(group = Wohnlage), size = 1) +
  geom_point(size = 2) +
  scale_fill_manual(values = c("Past Average" = "red", "Einfache" = "lightblue", "Mittlere" = "lightgreen", "Gute" = "lightcoral")) +
  scale_color_manual(values = c("Past Average" = "red", "Einfache" = "blue", "Mittlere" = "green", "Gute" = "coral")) +
  labs(title = "Rent Trends for All Sizes (2010-2026) Across All Locations",
       x = "Year", y = "Average Rent (€/m²)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Display the plot
print(rent_plot)

# Export the chart as a PNG file
ggsave("rent_trends_chart.png", plot = rent_plot, width = 10, height = 6, dpi = 300)
```