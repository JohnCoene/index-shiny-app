library(sf)
library(arrow)
library(tidyverse)

# ---- Resilience Index ----
ri <- read_csv("https://github.com/britishredcrosssociety/resilience-index/raw/main/data/processed/resilience%20index.csv")

# Remove Isles of Scily due to missing data
ri <- ri %>%
  filter(!str_detect(LAD19NM, "Scilly"))

# ---- Vulnerability Index ----
vi <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-MSOA-England.csv")

# Neater versions of MSOA names
msoa_names <- read_csv("https://visual.parliament.uk/msoanames/static/MSOA-Names-1.7.csv") %>%
  select(MSOA11CD = msoa11cd, Name_clean = msoa11hclnm)

# Lookup MSOAs to LAs
msoa_lad <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv")

vi <- vi %>%
  left_join(msoa_names, by = c("Code" = "MSOA11CD")) %>%
  left_join(
    msoa_lad %>% select(-LAD17CD),
    by = c("Code" = "MSOA11CD")
  )

write_feather(vi, "inst/data/vulnerability-index-msoa-england.feather", compression = "uncompressed")

# ---- Bivariate Scores ----
# create 3 buckets for vulnerability and resilience to map to colours
# Create 3 buckets: 1-4, 5-8, 9-10
# decile_buckets <- c(1, 4, 8, 10)
quintile_buckets <- c(1, 2, 4, 5)
vul_buckets <- quintile_buckets
res_buckets <- quintile_buckets

# create color scale that encodes two variables
# red for vulnerability and blue for resilience
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949",
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1",
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A",
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E",
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0"
) %>%
  gather("group", "fill")

# Cut vulnerability and resilience scores into groups defined by buckets
# Join colour scale
ri_bivariate <- ri %>%
  mutate(
    vul_cut = cut(
      `Vulnerability quintile`,
      breaks = vul_buckets,
      include.lowest = TRUE,
      right = TRUE
    ),
    res_cut = cut(
      `Capacity quintile`,
      breaks = res_buckets,
      include.lowest = TRUE,
      right = TRUE
    ),
    group = paste(
      as.numeric(vul_cut),
      "-",
      as.numeric(res_cut)
    )
  ) %>%
  left_join(bivariate_color_scale, by = "group")

# unique(ri_bivariate$vul_cut)
# unique(ri_bivariate$res_cut)

# - Create legend -
bivariate_color_scale <- bivariate_color_scale %>%
  separate(group, into = c("vul", "res"), sep = " - ") %>%
  mutate(
    vul = as.integer(vul),
    res = as.integer(res)
  )

# Legend
ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = vul,
      y = res,
      fill = fill
    ),
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  theme_void() +
  coord_fixed()

ggsave(
  filename = "inst/www/bivar-legend-void.jpg",
  bg = "transparent",
  width = 5.21,
  height = 5.21
)

# ---- Prep RI ----
write_feather(ri_bivariate, "inst/data/resilience-index.feather", compression = "uncompressed")

# Create labels for map
labels <-
  paste0(
    sprintf("<strong>%s</strong><br/>", ri_bivariate$LAD19NM),
    "Vulnerability quintile (5 = most vulnerable): ",
    ri_bivariate$`Vulnerability quintile`,
    "<br/>",
    "Capacity quintile (5 = lowest capacity): ",
    ri_bivariate$`Capacity quintile`
  ) %>%
  lapply(htmltools::HTML)

# write_feather(labels, "data/la-labels.feather")
write_rds(labels, "inst/data/la-labels.rds")

# ---- LAs ----
# Local Authority Districts (December 2019) Boundaries UK BUC
# Source: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc
lad_shp <- read_sf("https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson")

# Save a shapefile that we'll join onto later
lad_shp <- lad_shp %>%
  filter(str_sub(lad19cd, 1, 1) == "E") %>% # England only for now

  st_transform(crs = 4326) %>%
  select(lad19cd, lad19nm)

lad_shp %>% write_sf("inst/datasets/lad.shp")

# Calculate and save centroids
lad_shp %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(lad19cd = lad_shp$lad19cd) %>%
  select(lad19cd, lng = X, lat = Y) %>%
  write_feather("inst/datasets/la-centroids.feather")

# ---- MSOAs ----
# Middle Layer Super Output Areas (December 2011) EW BSC V2
# Source: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-ew-bsc-v2
msoa_shp <- read_sf("https://opendata.arcgis.com/datasets/23cdb60ee47e4fef8d72e4ee202accb0_0.geojson")

# Save a shapefile that we'll join onto later
msoa_shp %>%
  filter(str_sub(MSOA11CD, 1, 1) == "E") %>% # England only for now
  st_transform(crs = 4326) %>%
  select(MSOA11CD, MSOA11NM) %>%
  write_sf("inst/datasets/msoa.shp")

# ---- LRFs ----
# Local Resilience Forums (December 2019) EW BUC
# Source: https://geoportal.statistics.gov.uk/datasets/local-resilience-forums-december-2019-ew-buc
lrf_shp <- read_sf("https://opendata.arcgis.com/datasets/578216055f8c45a98f6d692856e0c03a_0.geojson")

# Save a shapefile that we'll join onto later
lrf_shp %>%
  filter(str_sub(LRF19CD, 1, 1) == "E") %>% # England only for now
  st_transform(crs = 4326) %>%
  select(LRF19CD, LRF19NM) %>%
  write_sf("inst/datasets/lrf.shp")
