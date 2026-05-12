library(igraph)   # load before tidyverse to avoid masking conflicts
library(tidyverse)
library(showtext)

font_add_google("IBM Plex Sans", "ibm")
showtext_auto()

# --- Load data ---
tuesdata <- tidytuesdayR::tt_load('2026-05-12')
cities   <- tuesdata$cities
links    <- tuesdata$links

# --- Clean ---
cities_v <- cities |>
  rename(city_name = name, country_code = countrycd,
         longitude = lng, latitude = lat)

# --- Build graph & compute degree ---
g <- igraph::graph_from_data_frame(links, directed = FALSE, vertices = cities_v)

city_degree <- tibble(id = igraph::V(g)$name, degree = igraph::degree(g)) |>
  left_join(cities_v |> select(id, city_name, country, continent), by = "id") |>
  arrange(desc(degree))

# --- Top 20 most connected cities ---
city_degree |>
  slice_head(n = 20) |>
  mutate(
    city_name = fct_reorder(city_name, degree),
    highlight = city_name == "Saint Petersburg"
  ) |>
  ggplot(mapping = aes(x = degree, y = city_name, fill = highlight)) +
  geom_col() +
  geom_text(
    aes(x = 1, label = city_name),
    hjust = 0, color = "white", fontface = "bold", size = 3.2,
    family = "ibm"
  ) +
  geom_text(aes(label = degree), hjust = -0.2, size = 3.2, family = "ibm") +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = scales::label_comma()
  ) +
  scale_fill_manual(values = c("TRUE" = "#009E73", "FALSE" = "grey70")) +
  scale_y_discrete(labels = NULL) +
  labs(
    title    = "Saint Petersburg Leads the World in Sister City Connections",
    subtitle = "Top 20 cities by number of twin city links",
    x        = "Number of twin city links",
    y        = NULL,
    caption  = "Source: TidyTuesday 2026-05-12 · Sister Cities dataset | Visualisation: Emmanuel Prah"
  ) +
  theme_minimal(base_size = 14, base_family = "ibm") +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title         = element_text(face = "bold", hjust = 0),
    plot.subtitle      = element_text(hjust = 0),
    plot.caption       = element_text(color = "grey60", size = 9, hjust = 1),
    plot.margin        = margin(15, 15, 15, 15),
    legend.position    = "none"
  )

ggsave("sister_cities_top20.png", width = 23, height = 13, units = "cm",
       dpi = 150, bg = "white")
