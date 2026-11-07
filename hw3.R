# Install packages once in the Console if needed:
# install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

vehicles <- ggplot2::mpg # Full dataset
gems <- ggplot2::diamonds # Full dataset


#1a

vehicles_small <- vehicles |>
  filter(displ <= 5)

p_A <- vehicles_small |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~ drv) +
  labs(
    x     = "Engine displacement (litres)",
    y     = "Highway fuel economy (mpg)",
    color = "drv"
  ) +
  theme_bw()

p_A

tibble(
  total_rows = nrow(vehicles),
  plotted    = nrow(vehicles_small),
  excluded   = nrow(vehicles) - nrow(vehicles_small)
)

#the reference x axis stops at 5 and the r panel has nothing past 4.6 even though mpg has rear drive cars up to 7
#so the reference is filtered to displ <= 5, same filter as p_final in module 8

#facet_wrap(~ drv) splits the rows into 3 panels before any stat is computed, so geom_smooth fits lm once per panel
#color is only mapped inside geom_point so the smoother isnt grouped by color, the 3 lines come from the facets
#without the facet the same code would draw one overall line


#1b

p_positions <- ggplot(vehicles, aes(x = drv, fill = factor(cyl)))

p_stack <- p_positions +
  geom_bar(position = "stack") +
  theme_bw()

p_fill <- p_positions +
  geom_bar(position = "fill") +
  labs(y = "Proportion within drive type") +
  theme_bw()

p_stack
p_fill

drv_cyl_props <- vehicles |>
  count(drv, cyl) |>
  mutate(
    drv_total  = sum(n),
    proportion = n / drv_total,
    .by = drv
  )

drv_cyl_props

#fill divides each segment by its own bars total, so every bar adds up to 1 no matter how many cars are in it
#the totals arent the same, 4 has 103 cars, f has 106, r has only 25, and the rescaling throws that away
#equal heights just mean each bar is 100% of itself, not equal sample sizes
#the r shares rest on 25 cars, one car switching groups moves a share by 4 points vs about 1 for 4 and f


#1c

smaller <- gems |>
  filter(carat < 3)

n_retained <- nrow(smaller)
n_excluded <- nrow(gems) - n_retained

tibble(
  retained = n_retained,
  excluded = n_excluded
)

kept_note <- paste0("Diamonds under 3 carats: ", format(n_retained, big.mark = ","),
                    " retained, ", n_excluded, " excluded")

p_fine <- ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  coord_cartesian(xlim = c(0, 3)) +
  labs(
    title   = "Diamond weights, bin width = 0.01 carat",
    x       = "Weight (carats)",
    y       = "Count",
    caption = kept_note
  ) +
  theme_bw()

p_coarse <- ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  coord_cartesian(xlim = c(0, 3)) +
  labs(
    title   = "Diamond weights, bin width = 0.5 carat",
    x       = "Weight (carats)",
    y       = "Count",
    caption = kept_note
  ) +
  theme_bw()

p_fine
p_coarse

carat_exact <- gems |>
  filter(carat %in% c(0.99, 1.00)) |>
  count(carat, name = "n_diamonds")

carat_exact

#narrow bins show sharp spikes right at round weights like 0.3, 0.5, 0.7, 1, 1.5 and 2 carats
#each spike tapers off to the right and theres almost nothing just below the next round weight
#the counts back it up, 1558 diamonds at exactly 1.00 vs only 23 at 0.99
#the 0.5 bins average each spike in with its gap so the pattern disappears into one smooth right skew


#2 data

lib_raw_path   <- "data_raw/chicago_libraries_raw.csv"
bound_raw_path <- "data_raw/chicago_community_areas.geojson"

data_sources <- tibble(
  dataset      = c("libraries", "community_areas"),
  title        = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
                   "Boundaries - Community Areas (current)"),
  publisher    = c("City of Chicago Data Portal (Chicago Public Library)",
                   "City of Chicago Data Portal"),
  dataset_id   = c("x8fc-8rcq", "cauq-8yn6 (replaced on the portal by igwz-8jzy in 2025)"),
  landing_page = c("https://data.cityofchicago.org/d/x8fc-8rcq",
                   "https://data.cityofchicago.org/d/cauq-8yn6"),
  download_url = c("https://data.cityofchicago.org/api/views/x8fc-8rcq/rows.csv?accessType=DOWNLOAD",
                   "https://raw.githubusercontent.com/RandomFractals/ChicagoCrimes/master/data/chicago-community-areas.geojson"),
  retrieved_on = c("2026-11-07", "2026-11-07"),
  data_date    = c("portal metadata last modified 2024-10-04",
                   "github mirror of the portal geojson export, committed 2017-08-30"),
  license      = c("Public, City of Chicago Data Portal Terms of Use",
                   "Public, City of Chicago Data Portal Terms of Use"),
  local_file   = c(lib_raw_path, bound_raw_path),
  md5          = unname(tools::md5sum(c(lib_raw_path, bound_raw_path))),
  notes        = c("official csv export saved unchanged, LOCATION holds (latitude, longitude) in WGS 84",
                   "unmodified export with the portal field names, portal was not reachable when downloading")
)

write_csv(data_sources, "data_sources.csv")

lib_raw <- read_csv(lib_raw_path, show_col_types = FALSE)
n_raw   <- nrow(lib_raw)

lib_parsed <- lib_raw |>
  extract(
    LOCATION,
    into    = c("lat", "lon"),
    regex   = "\\(\\s*([-0-9.]+)\\s*,\\s*([-0-9.]+)\\s*\\)",
    convert = TRUE,
    remove  = FALSE
  ) |>
  mutate(
    site_id       = sprintf("CPL%03d", as.integer(basename(WEBSITE))),
    facility_name = trimws(gsub("\\s+", " ", BRANCH)),
    facility_name = if_else(facility_name == "Harold Washtington Library Center",
                            "Harold Washington Library Center", facility_name),
    is_closed     = grepl("closed", `SERVICE HOURS`, ignore.case = TRUE)
  )

#site_id comes from the branch number in each library website url so it stays the same across downloads
#harold washington is misspelled in the source, its only fixed in the cleaned copy, the raw file is untouched

coord_audit <- lib_parsed |>
  summarise(
    records              = n(),
    missing_location     = sum(is.na(LOCATION) | LOCATION == ""),
    unparsed_coordinates = sum(is.na(lat) | is.na(lon)),
    out_of_range         = sum(abs(lat) > 90 | abs(lon) > 180, na.rm = TRUE),
    zero_coordinates     = sum(lat == 0 | lon == 0, na.rm = TRUE),
    possibly_swapped     = sum(lat < 0 | lon > 0, na.rm = TRUE),
    outside_chicago_box  = sum(!between(lat, 41.6, 42.1) | !between(lon, -87.95, -87.5), na.rm = TRUE),
    exact_duplicate_rows = sum(duplicated(lib_raw)),
    duplicate_site_ids   = sum(duplicated(site_id)),
    duplicate_names      = sum(duplicated(facility_name)),
    shared_coordinates   = sum(duplicated(paste(lat, lon)) | duplicated(paste(lat, lon), fromLast = TRUE))
  )

coord_audit |>
  pivot_longer(everything(), names_to = "check", values_to = "n")

lib_valid_coords <- lib_parsed |>
  filter(!is.na(lat), !is.na(lon))

lib_clean <- lib_valid_coords |>
  filter(!is_closed)

cleaning_log <- tibble(
  step          = c("raw file", "drop missing coordinates", "drop closed branches"),
  records_after = c(n_raw, nrow(lib_valid_coords), nrow(lib_clean)),
  removed       = c(0, n_raw - nrow(lib_valid_coords), nrow(lib_valid_coords) - nrow(lib_clean)),
  removed_names = c("", "", paste(lib_valid_coords$facility_name[lib_valid_coords$is_closed], collapse = "; "))
)

cleaning_log

#no coordinate problems and no duplicates
#shared coordinates would only be flagged, not removed, 2 branches at 1 address are still 2 facilities
#galewood-mont clare says closed until further notice so it isnt serving anyone, dropped it as not eligible
#82 records before cleaning, 81 after, thats the only removal

sites_m <- lib_clean |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(26916) |>
  select(site_id, facility_name, address = ADDRESS, zip = ZIP, lat, lon)

st_crs(sites_m)$Name
st_crs(sites_m)$units

sites_m |>
  st_drop_geometry() |>
  write_csv("sites_clean.csv")

#sites_m is in NAD83 / UTM zone 16N (EPSG 26916), units are meters
#didnt use illinois state plane (EPSG 3435) because its in us feet, so a 500 buffer would be 500 feet

boundary_raw <- st_read(bound_raw_path, quiet = TRUE)

city_outline <- boundary_raw |>
  st_transform(st_crs(sites_m)) |>
  st_make_valid() |>
  st_union() |>
  st_sf(geometry = _)


#2 areas

union_buffer <- function(points, radius_m) {
  buffer_zone <- points |>
    st_buffer(dist = radius_m) |>
    st_union()

  return(buffer_zone)
}

buffer_500  <- union_buffer(sites_m, 500)
buffer_1000 <- union_buffer(sites_m, 1000)

buffer_summary <- tibble(
  radius_m       = c(500, 1000),
  union_area_km2 = c(as.numeric(st_area(buffer_500)), as.numeric(st_area(buffer_1000))) / 1e6
)

buffer_summary

buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1]

city_area_km2 <- as.numeric(st_area(city_outline)) / 1e6

tibble(
  radius_m         = c(500, 1000),
  separate_km2     = c(sum(as.numeric(st_area(st_buffer(sites_m, 500)))),
                       sum(as.numeric(st_area(st_buffer(sites_m, 1000))))) / 1e6,
  union_km2        = buffer_summary$union_area_km2,
  overlap_km2      = separate_km2 - union_km2,
  inside_city_km2  = c(as.numeric(st_area(st_intersection(buffer_500, st_geometry(city_outline)))),
                       as.numeric(st_area(st_intersection(buffer_1000, st_geometry(city_outline))))) / 1e6,
  pct_city_covered = 100 * inside_city_km2 / city_area_km2
)

nn_dist <- units::drop_units(st_distance(sites_m))
diag(nn_dist) <- NA
nn_km <- apply(nn_dist, 1, min, na.rm = TRUE) / 1000

tibble(
  median_spacing_km = median(nn_km),
  max_spacing_km    = max(nn_km),
  most_isolated     = paste(sites_m$facility_name[nn_km == max(nn_km)], collapse = " and ")
)

#st_union dissolves the circles together so any overlap only gets counted once
#the buffers arent clipped to the city so area outside the boundary is included like the assignment asks


#2 maps

source_caption <- paste0(
  "Source: City of Chicago Data Portal; Libraries (x8fc-8rcq),\n",
  "retrieved 2026-11-07; Community Areas (current), 2017-08-30 snapshot.\n",
  "CRS: NAD83 / UTM zone 16N (EPSG:26916), meters."
)

map_theme <- theme_bw() +
  theme(plot.caption.position = "plot", plot.title.position = "plot")

p_sites <- ggplot() +
  geom_sf(data = city_outline, aes(color = "Chicago city limits"), fill = "grey96", linewidth = 0.4) +
  geom_sf(data = sites_m, aes(shape = "Library branch"), color = "#1f4e79", size = 1.8) +
  scale_color_manual(name = NULL, values = "grey35") +
  scale_shape_manual(name = NULL, values = 16) +
  labs(
    title    = paste0("Chicago Public Library branches (n = ", nrow(sites_m), ")"),
    subtitle = "All retained facilities after cleaning",
    caption  = source_caption
  ) +
  map_theme

p_buffers <- ggplot() +
  geom_sf(data = city_outline, aes(color = "Chicago city limits"), fill = "grey96", linewidth = 0.4) +
  geom_sf(data = st_sf(geometry = buffer_500), aes(fill = "Within 500 m of a library"),
          color = NA, alpha = 0.6) +
  geom_sf(data = sites_m, aes(shape = "Library branch"), color = "#1f4e79", size = 0.6) +
  scale_color_manual(name = NULL, values = "grey35") +
  scale_fill_manual(name = NULL, values = "#f4a261") +
  scale_shape_manual(name = NULL, values = 16) +
  labs(
    title    = "Area within 500 m of at least one library branch",
    subtitle = sprintf("Dissolved 500 m buffer = %.1f sq. km (overlap counted once,\nincluding area outside city limits)",
                       buffer_summary$union_area_km2[1]),
    caption  = source_caption
  ) +
  map_theme

p_sites
p_buffers

#the branches are spread across the whole city instead of bunched downtown
#the biggest gaps are around ohare and the far southeast, altgeld and hegewisch are 4.2 km from their nearest neighbor
#typical spacing between neighbors is about 1.9 km so the 500 m circles barely touch, 63.5 sq km union vs 0.06 of overlap
#at 1000 m the union is 247 sq km, 3.9x bigger instead of 4x because doubling the radius makes neighbors overlap
#clipped to the city thats about 10.6% covered at 500 m and 40% at 1000 m, most of chicago is over 1 km from a branch
#these are straight line distances, they ignore streets, rivers and rail yards so real walking access is worse


#3 boundaries

districts_raw <- boundary_raw |>
  st_transform(st_crs(sites_m))

tibble(
  features       = nrow(districts_raw),
  missing_ids    = sum(is.na(districts_raw$area_numbe) | districts_raw$area_numbe == ""),
  duplicate_ids  = sum(duplicated(districts_raw$area_numbe)),
  missing_names  = sum(is.na(districts_raw$community) | districts_raw$community == ""),
  empty_shapes   = sum(st_is_empty(districts_raw)),
  invalid_shapes = sum(!st_is_valid(districts_raw))
)

unique(st_geometry_type(districts_raw))

districts <- districts_raw |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  transmute(
    district_id   = as.integer(area_numbe),
    district_name = tools::toTitleCase(tolower(community)),
    district_name = recode(district_name, "Ohare" = "O'Hare", "Mckinley Park" = "McKinley Park")
  ) |>
  arrange(district_id)

districts$area_km2 <- as.numeric(units::set_units(st_area(districts), km^2))

tibble(
  all_valid       = all(st_is_valid(districts)),
  unique_ids      = !anyDuplicated(districts$district_id),
  same_crs        = st_crs(districts) == st_crs(sites_m),
  areas_positive  = all(districts$area_km2 > 0),
  areas_kept      = nrow(districts)
)

#77 areas, no missing or duplicate ids, nothing empty or invalid, so st_make_valid didnt have to fix anything
#nothing got removed and areas with no libraries stay in
#the boundaries are a 2017 snapshot and the libraries are from 2026, the community area lines havent changed but the dates dont match


#3 matches and summary

match_list <- st_intersects(sites_m, districts)

site_match_audit <- tibble(
  site_id        = sites_m$site_id,
  facility_name  = sites_m$facility_name,
  n_area_matches = lengths(match_list)
)

write_csv(site_match_audit, "site_match_audit.csv")

match_summary <- site_match_audit |>
  mutate(match_type = case_when(
    n_area_matches == 0 ~ "zero",
    n_area_matches == 1 ~ "exactly one",
    .default = "multiple"
  )) |>
  count(match_type, name = "n_sites")

match_summary

not_exactly_one <- site_match_audit |>
  filter(n_area_matches != 1)

not_exactly_one

n_unassigned <- nrow(not_exactly_one)

one_match  <- lengths(match_list) == 1
site_areas <- tibble(
  site_id     = sites_m$site_id[one_match],
  district_id = districts$district_id[unlist(match_list[one_match])]
)
n_assigned <- nrow(site_areas)

district_counts <- districts |>
  st_drop_geometry() |>
  as_tibble() |>
  left_join(count(site_areas, district_id, name = "n_sites"), by = join_by(district_id)) |>
  mutate(
    n_sites       = coalesce(n_sites, 0L),
    sites_per_km2 = n_sites / area_km2
  ) |>
  select(district_id, district_name, n_sites, area_km2, sites_per_km2)

print(district_counts, n = Inf)

tibble(
  areas_kept         = nrow(district_counts) == nrow(districts),
  counts_match_sites = sum(district_counts$n_sites) == n_assigned,
  areas_positive     = all(district_counts$area_km2 > 0)
)

top_by_count <- district_counts |>
  arrange(desc(n_sites), desc(sites_per_km2)) |>
  slice_head(n = 5)

top_by_density <- district_counts |>
  arrange(desc(sites_per_km2)) |>
  slice_head(n = 5)

top_by_count
top_by_density

cor(district_counts$n_sites, district_counts$sites_per_km2, method = "spearman")

#st_intersects gives a list of matching areas for each site and lengths() counts them
#all 81 sites match exactly one area so nothing is unassigned
#a site sitting right on a shared border would match 2 areas and get left out instead of counted twice
#left_join from the areas side plus coalesce keeps the 17 areas with no library as 0 instead of dropping them


#3 maps

districts_map <- districts |>
  select(district_id, geometry) |>
  left_join(district_counts, by = join_by(district_id))

map_box <- st_bbox(districts)

map_caption <- paste0(
  source_caption, "\n", n_assigned, " of ", nrow(sites_m), " sites assigned to one area; ",
  n_unassigned, " unassigned.\n",
  "Dashed outline = 0 libraries; dark grey = missing (none)."
)

zero_areas <- districts_map |>
  filter(n_sites == 0)

zero_layer <- geom_sf(data = zero_areas, aes(linetype = "No library (0)"),
                      fill = NA, color = "grey15", linewidth = 0.45)
zero_scale <- scale_linetype_manual(name = NULL, values = "22")

p_count <- ggplot(districts_map) +
  geom_sf(aes(fill = n_sites), color = "white", linewidth = 0.2) +
  scale_fill_distiller(name = "Libraries\n(count)", palette = "YlGnBu", direction = 1,
                       breaks = 0:max(district_counts$n_sites), na.value = "grey30") +
  zero_layer +
  zero_scale +
  coord_sf(xlim = map_box[c("xmin", "xmax")], ylim = map_box[c("ymin", "ymax")]) +
  labs(
    title   = "Library branches per community area",
    caption = map_caption
  ) +
  map_theme

p_density <- ggplot(districts_map) +
  geom_sf(aes(fill = sites_per_km2), color = "white", linewidth = 0.2) +
  scale_fill_distiller(name = "Libraries\nper sq. km", palette = "YlOrRd", direction = 1,
                       na.value = "grey30") +
  zero_layer +
  zero_scale +
  coord_sf(xlim = map_box[c("xmin", "xmax")], ylim = map_box[c("ymin", "ymax")]) +
  labs(
    title   = "Library branches per square kilometer (sq. km)",
    caption = map_caption
  ) +
  map_theme

p_count
p_density

#by count the leaders are humboldt park, portage park, new city, near west side and austin with 3 each
#per sq km the leaders are small areas instead, douglas 0.47, armour square 0.39, woodlawn 0.37, kenwood 0.37, uptown 0.33
#austin drops to the middle because its 3 branches are spread over 18.5 sq km
#spearman correlation between count and density is about 0.79, related but which area has the most depends on the denominator
#the lines matter too, hyde park shows 0 only because the blackstone branch is just across the border in kenwood
#per person would be a better denominator than area but that needs population data this assignment doesnt have


#saving plots for the report

dir.create("output", showWarnings = FALSE)

pdf("output/HW3_plots.pdf", width = 8.5, height = 11)
for (p in list(p_A, p_stack, p_fill, p_fine, p_coarse, p_sites, p_buffers, p_count, p_density)) {
  print(p)
}
dev.off()
