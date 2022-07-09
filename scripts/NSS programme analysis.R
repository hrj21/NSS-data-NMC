# Load packages -----------------------------------------------------------
library(tidyverse)
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(pheatmap)

# Read data ---------------------------------------------------------------
nss <- read_csv("data/nss no uni title.csv")
nss <- arrange(nss, Year, Total)
locations <- read_csv("data/NSS22 CSV.csv")[, 1:2]

# Plot heatmaps -----------------------------------------------------------
walk2(.x = rep(unique(nss$Programme), 3), .y = rep(c("2020", "2021", "2022"), each = 5), function(.x, .y){
  mat <- as.matrix(filter(nss, Year == .y, Programme == .x)[, 4:31])
  rownames(mat) <- filter(nss, Year == .y, Programme == .x)$AEI
  
  pheatmap(
    mat,
    color = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    scale = "column",
    fontsize_row = 6,
    fontsize_col = 6,
    filename = paste0("plots/", .x, " ", .y, " - heatmap.png"),
    height = 5,
    width = 5,
    main = paste(.x, .y)
  )
})

# Maps --------------------------------------------------------------------
# register_google([googlemapsAPI goes here])
# long_lat <- geocode(locations$Postcode)
#saveRDS(long_lat, file = "data/long_lat.rds")
long_lat <- readRDS("data/long_lat.rds")

locations <- cbind(locations, long_lat)
locations$Full <- locations$AEI

nss_scaled <- nss |> 
  group_by(Programme, Year) |> 
  mutate(across(where(is.numeric), function(x) as.vector(scale(x)))) |> 
  ungroup() |> 
  left_join(locations, by = "Full")

# world <- map_data("world")
world <- world_shp <- ne_download(scale = 10, type = "ocean", category = "physical", returnclass = "sf")

ggplot(data = world) +
  geom_sf(col = "lightgrey", fill = "antiquewhite") +
  facet_grid(Year ~ Programme) +
  geom_point(data = nss_scaled, aes(x = lon, y = lat, fill = Total), shape = 21, size = 2) +
  theme_void() +
  scale_fill_distiller(type = "div", palette = "RdYlGn", direction = 1,) +
  coord_sf(xlim = c(-15,8), ylim = c(49.5, 59.5), expand = FALSE) +
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.position = "right",
        plot.background = element_rect(fill = "white", colour = "white"))

ggsave("plots/All - maps (scaled within programme and year).png", width = 12, height = 10)

ggplot(data = world) +
  geom_sf(col = "lightgrey", fill = "antiquewhite") +
  facet_wrap(~ Programme) +
  geom_point(
    data = filter(nss, Year == "2020") |> left_join(locations, by = "Full"), 
    aes(x = lon, y = lat, fill = Total), shape = 21, size = 3
  ) +
  theme_void() +
  scale_fill_distiller(type = "div", palette = "RdYlGn", direction = 1,) +
  coord_sf(xlim = c(-15,8), ylim = c(49.5, 59.5), expand = FALSE) +
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.position = c(0.85, 0.2),
        plot.background = element_rect(fill = "white", colour = "white"))

ggsave("plots/NSS2020 - maps.png", width = 12, height = 12)

ggplot(data = world) +
  geom_sf(col = "lightgrey", fill = "antiquewhite") +
  facet_wrap(~ Programme) +
  geom_point(
    data = filter(nss, Year == "2021") |> left_join(locations, by = "Full"), 
    aes(x = lon, y = lat, fill = Total), shape = 21, size = 3
  ) +
  theme_void() +
  scale_fill_distiller(type = "div", palette = "RdYlGn", direction = 1,) +
  coord_sf(xlim = c(-15,8), ylim = c(49.5, 59.5), expand = FALSE) +
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.position = c(0.85, 0.2),
        plot.background = element_rect(fill = "white", colour = "white"))

ggsave("plots/NSS2021 - maps.png", width = 12, height = 12)

ggplot(data = world) +
  geom_sf(col = "lightgrey", fill = "antiquewhite") +
  facet_wrap(~ Programme) +
  geom_point(
    data = filter(nss, Year == "2022") |> left_join(locations, by = "Full"), 
    aes(x = lon, y = lat, fill = Total), shape = 21, size = 3
  ) +
  theme_void() +
  scale_fill_distiller(type = "div", palette = "RdYlGn", direction = 1,) +
  coord_sf(xlim = c(-15,8), ylim = c(49.5, 59.5), expand = FALSE) +
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.position = c(0.85, 0.2),
        plot.background = element_rect(fill = "white", colour = "white"))

ggsave("plots/NSS2022 - maps.png", width = 12, height = 12)

# Change over time --------------------------------------------------------
nat_av <- nss |> 
  group_by(Year, Programme) |> 
  summarise(Total = mean(Total)) |> 
  mutate(AEI = "National average") |> 
  ungroup()

line_dat <- rbind(nss[, c(2, 3, 31, 43)], nat_av)
line_dat$AEI <- factor(line_dat$AEI, levels = unique(line_dat$AEI))

line_dat |> 
  ggplot(aes(as.factor(Year), Total, group = Programme, color = Programme)) +
  facet_wrap(~ AEI, ncol = 12) +
  geom_hline(yintercept = c(1500, 2000), size = 0.1) +
  geom_point(size = 0.5) + 
  geom_line() +
  theme_test() +
  scale_color_brewer(type = "qual", palette = "Set1") +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(x = "", y = "") +
  theme(
    panel.spacing = unit(0, "lines"),
    legend.position = c(0.83, 0.02),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "white", colour = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("plots/NSS - lines.png", width = 12, height = 12)

