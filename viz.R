library(geofacet)

source("data_prep.R")

theme_set(theme_void())
theme_update(strip.text = element_blank(),
             legend.position = "none",
             plot.margin = margin(rep(60, 4)))

lc_geofacet <-
  lc_countries_codes |>
  mutate(iso_a3 = case_when(
    country == "France" ~ "FRA",
    country == "Kosovo" ~ "XKX",
    country == "N. Cyprus" ~ "CYN",
    country == "Norway" ~ "NOR",
    country == "Somaliland" ~ "SOL",
    TRUE ~ iso_a3
  )) |>
  filter(!is.na(iso_a3), !is.na(landcover)) |> 
  select(landcover, diff = diff_prop_area, country, iso_a3) |> 
  mutate(trend = case_when(diff > 0 ~ "positive", diff < 0 ~ "negative", TRUE ~ "neutral")) |> 
  pivot_wider(id_cols = c(country, iso_a3), names_from = landcover, values_from = c(diff, trend))

## world geofacet
custom_world_grid <-  
  world_countries_grid1 |> 
  mutate(
    col = case_when(
      code_alpha3 %in% c("ESP", "PRT", "ITA") ~ col - 1, 
      code_alpha3 == "AND" ~ 11,
      code_alpha3 %in% c("LIE", "MCO") ~ 12,
      code_alpha3 == "SMR" ~ 13,
      code_alpha3 == "TWN" ~ 25,
      code_alpha3 %in% c("NOR", "SWE", "FIN") ~ col - 1,
      TRUE ~ col
    ),
    row = case_when(
      code_alpha3 %in% c("ITA", "MLT", "ESP", "PRT") ~ row + 1,
      code_alpha3 %in% c("AND", "LIE") ~ 6,
      code_alpha3 %in% c("MCO", "SMR") ~ 7,
      code_alpha3 == "TWN" ~ 8,
      code_alpha3 == "NZL" ~ 20,
      code_alpha3 == "GBR" ~ 5,
      code_alpha3 == "FRA" ~ 6,
      code_alpha3 %in% c("GRL", "ISL") ~ row + 1,
      col <= 8 & code_alpha3 != "GRL" ~ row + 2,
      code_alpha3 %in% c("NOR", "SWE", "FIN") ~ row + 1,
      TRUE ~ row
    ),
    row = row - 1
  ) |> 
  filter(code_alpha3 %in% lc_geofacet$iso_a3)


## visualization urban + trees
lc_geofacet |>
  mutate(
    y_urban = .5,
    s_urban = if_else(trend_urban == "neutral", 7.5, 9),
    y_trees = if_else(trend_trees == "negative", -.3, -.6),
    s_trees = if_else(trend_trees == "neutral", 7.5, 9)
  ) |> 
  ggplot() +
  ggstar::geom_star(
    aes(x = 0, y = y_urban, fill = diff_urban, starshape = trend_urban, size = s_urban), color = "#25004E"
  ) +
  scale_fill_gradient(low = "#172546", high = "#5ECEC5") +
  ggnewscale::new_scale(new_aes = "fill") +
  ggstar::geom_star(
    aes(x = 0, y = y_trees, fill = diff_trees, starshape = trend_trees, size = s_trees), color = "#25004E"
  ) +
  scico::scale_fill_scico(palette = "managua", midpoint = 0) +
  ggstar::scale_starshape_manual(values = c(23, 22, 11)) + 
  scale_size_identity() +
  geofacet::facet_geo(~iso_a3, grid = custom_world_grid) +
  coord_cartesian(clip = "off", xlim = c(-1.25, 1.25), ylim = c(-.75, 1.25))

ggsave(paste0("./plots/geofacet-triangles-raw.png"),
       width = 13, height = 13, dpi = 600, bg = "#25004E")


lc_geofacet |> 
  mutate(
    y_urban = .57,
    s_urban = if_else(trend_urban == "neutral", 6.5, 8.3),
    y_trees = if_else(trend_trees == "negative", -.6, -.9),
    s_trees = if_else(trend_trees == "neutral", 6.5, 8.3)
  ) |> 
  ggplot() +
  ggstar::geom_star(
    aes(x = 0, y = y_urban, fill = diff_urban, starshape = trend_urban, size = s_urban), color = "#25004E"
  ) +
  scale_fill_gradient(low = "#172546", high = "#5ECEC5") +
  ggnewscale::new_scale(new_aes = "fill") +
  ggstar::geom_star(
    aes(x = 0, y = y_trees, fill = diff_trees, starshape = trend_trees, size = s_trees), color = "#25004E"
  ) +
  scico::scale_fill_scico(palette = "managua", midpoint = 0) +
  ggstar::scale_starshape_manual(values = c(23, 22, 11)) + 
  scale_size_identity() +
  geom_text(aes(x = 0, y = 0, label = iso_a3), family = "Bricolage Grotesque", 
            fontface = "bold", color = "grey45", size = 3) +
  geofacet::facet_geo(
    ~iso_a3, 
    grid = custom_world_grid, 
    label = "code_alpha3"
  ) +
  coord_cartesian(clip = "off", xlim = c(-1.25, 1.25), ylim = c(-.75, 1.25))

ggsave("./plots/geofacet-triangles-labs-raw.png", width = 13, height = 13, dpi = 600, bg = "#25004E")

