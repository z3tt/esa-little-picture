library(tidyverse)

file <- "./lc1-countries.csv"

if (!file.exists(file)) {

  country_list = list.files(path = "lc-1/", full.names = TRUE)
  
  lc_countries = map_dfr(country_list, function(country){
    
    country_name = str_split(country, "_")[[1]][1] |> 
      str_split("//")
    country_name = country_name[[1]][2]
    
    read_csv(country) |> 
      pivot_longer(!year, names_to = "landcover", values_to = "count") |> 
      mutate(total = sum(count), .by = c(year)) |>
      mutate(
        landcover = case_when(
          landcover %in% c(
            "> 40% broad-leaved deciduous tree cover",
            "> 40% needle-leaved deciduous tree cover",
            "> 40% needle-leaved evergreen tree cover",
            "15 %-40 % broad-leaved deciduous tree cover",
            "15 %-40 % needle-leaved deciduous tree cover",
            "15 %-40 % needle-leaved evergreen tree cover",
            "> 15 % broad-leaved deciduous tree cover",
            "> 15% broad-leaved evergreen tree cover",
            "> 15% needle-leaved deciduous tree cover",
            "> 15% needle-leaved evergreen tree cover",
            "Flooded tree cover - fresh or brackish water",
            "Flooded tree cover - saline water",
            "Mosaic: > 50% herbaceous cover/ < 50% tree and shrub cover",
            "Mosaic: > 50% natural tree, shrub, herbaceous cover/< 50% cropland",
            "Mosaic: > 50% tree and shrub cover/< 50% herbaceous cover",
            # "Deciduous shrubland", "Evergreen shrubland", "Shrubland",
            # "Grassland", "Lichens and mosses",
            "Mixed leaf-type (broad-leaved and needle- leaved) tree cover"
          ) ~ "trees",
          str_detect(landcover, "fed cropland|> 50% cropland|post-flooding cropland") ~ "crops",
          landcover == "Urban areas" ~ "urban"
        )
      ) |>
      group_by(landcover, year, total) |> 
      summarize(count = sum(count)) |> 
      arrange(landcover, year) |>
      group_by(landcover) |>
      summarise(
        diff_abs = diff(c(first(count), last(count))), 
        diff_prop_time = diff(c(first(count), last(count))) / last(count) * 100,
        diff_prop_area = diff(c(first(count), last(count))) / last(total) * 100,
        last = last(count)
      ) |> 
      mutate(country = country_name)
    
  })
  
  lc_countries_codes <- 
    lc_countries |>
    ## remove duplicates
    filter(!country %in% c("French Guyana", "Republic of Congo", "Guinea Bissau",
                           "Democratic Republic of the Congo", "Equatorial Guinea")) |> 
    mutate(country = case_when(
      country == "CÃ´te d'Ivoire" ~ "Côte d'Ivoire",
      country == "Czech Rep." ~ "Czechia",
      country == "Dem. Rep. Korea" ~ "North Korea",
      country == "Korea" ~ "South Korea",
      country == "Lao PDR" ~ "Laos",
      country == "Swaziland" ~ "eSwatini",
      country == "United States" ~ "United States of America",
      TRUE ~ country
    )) |>
    left_join(
      rnaturalearth::ne_countries(returnclass = "sf"), 
      by = c("country" = "name")
    )
  
  # lc_countries_codes |> filter(is.na(adm0_a3)) |> select(country) |> unique()
  
  write_csv(lc_countries_codes, file)
} else {
  lc_countries_codes <- read_csv(file)
}
