# Load required libraries
library(tidyverse)
library(sf)

# ---- Data Preparation ----

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

flats_mapped <- flats_with_plz %>%
  left_join(wohnlage_mapping, by = "plz") %>%
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
    price_35_45m2 = (price_35m2 + price_42.5m2)/2,
    Year = as.character(Year),
    Wohnfläche = "All Sizes"
  ) %>%
  select(Year, price_35_45m2, Wohnfläche, percent_change)

trend <- historical %>%
  summarise(
    avg_change = mean(percent_change, na.rm = TRUE)/100,
    last_year = max(as.numeric(Year)),
    last_35_45m2 = last(price_35_45m2)
  )

# Extend years for 2010-2026
years_extended_base <- tibble(Year = as.character(2010:2026)) %>%
  left_join(historical %>% select(Year, price_35_45m2), by = "Year") %>%
  mutate(
    price_35_45m2 = if_else(
      Year <= "2023",
      price_35_45m2,
      trend$last_35_45m2 * (1 + trend$avg_change)^(as.numeric(Year) - trend$last_year)
    ),
    Wohnfläche = "All Sizes",
    Data_Source = "Past Average",
    Wohnlage = "Past Average"
  )

years_extended <- expand_grid(
  plz = c("10115","10997","12555","10707"),
  years_extended_base
) %>%
  left_join(tibble(
    plz = c("10115","10997","12555","10707"),
    place_name = c("Berlin-Mitte","Kreuzberg","Köpenick","Wilmersdorf")
  ), by = "plz") %>%
  select(plz, place_name, Data_Source, Wohnlage, Wohnfläche, Year, Rent = price_35_45m2)

# Mietspiegel data
mietspiegel_all <- bind_rows(
  read_csv("einfache_wohnlage.csv", show_col_types = FALSE) %>% mutate(Wohnlage = "Einfache"),
  read_csv("mittlere_wohnlage.csv", show_col_types = FALSE) %>% mutate(Wohnlage = "Mittlere"),
  read_csv("gute_wohnlage.csv", show_col_types = FALSE) %>% mutate(Wohnlage = "Gute")
) %>%
  mutate(across(c("untere Spanne","Mittelwert","obere Spanne"), ~ as.numeric(str_replace(str_replace(.,"€",""),",","."))),
         Wohnfläche = str_trim(Wohnfläche)) %>%
  group_by(Wohnlage) %>%
  summarise(Mittelwert = mean(Mittelwert, na.rm=TRUE),
            Wohnfläche = "All Sizes", .groups = "drop")

# Prepare rent_by_plz
all_combinations <- expand_grid(
  plz = c("10115","10997","12555","10707"),
  Wohnlage = c("Einfache","Mittlere","Gute"),
  Wohnfläche = "All Sizes"
) %>%
  left_join(tibble(
    plz = c("10115","10997","12555","10707"),
    place_name = c("Berlin-Mitte","Kreuzberg","Köpenick","Wilmersdorf")
  ), by = "plz") %>%
  left_join(flats_mapped %>% select(plz, Wohnlage, mean_rent), by = c("plz","Wohnlage"))

plz_geo <- st_read("plz.geojson") %>%
  filter(plz %in% c("10115","10997","12555","10707"))

rent_by_plz <- plz_geo %>%
  right_join(all_combinations, by = "plz") %>%
  left_join(mietspiegel_all, by = c("Wohnlage","Wohnfläche")) %>%
  mutate(
    Mittelwert = if_else(is.na(Mittelwert), mean_rent, Mittelwert),
    adj_2024 = mean(years_extended$Rent[years_extended$Year=="2024"], na.rm=TRUE)/mean(Mittelwert, na.rm=TRUE),
    adj_2025 = mean(years_extended$Rent[years_extended$Year=="2025"], na.rm=TRUE)/mean(Mittelwert, na.rm=TRUE),
    adj_2026 = mean(years_extended$Rent[years_extended$Year=="2026"], na.rm=TRUE)/mean(Mittelwert, na.rm=TRUE),
    `2024` = Mittelwert*adj_2024,
    `2025` = Mittelwert*adj_2025,
    `2026` = Mittelwert*adj_2026,
    Data_Source="Mietspiegel"
  ) %>%
  pivot_longer(cols=c("2024","2025","2026"), names_to="Year", values_to="Rent") %>%
  select(plz, place_name, Data_Source, Wohnlage, Wohnfläche, Year, Rent)

# Combine all data
final_data <- bind_rows(rent_by_plz, years_extended) %>%
  st_drop_geometry()

rent_aggregated <- final_data %>%
  filter(!is.na(Rent)) %>%
  group_by(Year, Data_Source, Wohnlage) %>%
  summarise(Avg_Rent = mean(Rent, na.rm=TRUE), .groups="drop") %>%
  filter(!(Data_Source=="Past Average" & Year %in% c("2024","2025","2026")))

# ---- Plotting ----

rent_plot <- ggplot(rent_aggregated, aes(x=Year, y=Avg_Rent, fill=Wohnlage, color=Wohnlage)) +
  geom_col(position="dodge", width=0.7) +
  geom_line(aes(group=Wohnlage), linewidth=1.2) +
  geom_point(size=3) +
  scale_fill_manual(values=c(
    "Past Average"="red", "Einfache"="#4DA6FF", "Mittlere"="#66CC66", "Gute"="#FF6666"
  )) +
  scale_color_manual(values=c(
    "Past Average"="red", "Einfache"="#0055CC", "Mittlere"="#339933", "Gute"="#CC0000"
  )) +
  labs(title="Rent Trends for All Sizes (2010-2026) Across All Locations",
       x="Year", y="Average Rent (€/m²)", fill="Wohnlage", color="Wohnlage") +
  theme_minimal(base_size=14) +
  theme(
    axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, color="black"),
    axis.text.y=element_text(color="black"),
    axis.title=element_text(face="bold", color="black"),
    legend.title=element_text(face="bold", color="black"),
    legend.text=element_text(color="black"),
    plot.title=element_text(face="bold", color="black", hjust=0.5),
    plot.background=element_rect(fill="white", color=NA),
    panel.background=element_rect(fill="white", color=NA)
  )

# Display the plot
print(rent_plot)

# ---- Export to GitHub repo folder ----
ggsave("C:\\Users\\rocksang\\Downloads\\Berlin rent price\\rent_trends_chart2.png",
       +        plot = rent_plot, width = 10, height = 6, dpi = 300, bg = "white")