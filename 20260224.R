# ============================================================
# SFI GRANTS ANALYSIS
# TidyTuesday | 2026-02-24
# ============================================================


# ------------------------------------------------------------
# 1. LOAD LIBRARIES
# ------------------------------------------------------------

library(extrafont)
library(tidyverse)
library(scales)
library(grid)
library(forcats)
library(ggplot2)
library(stringr)
library(janitor)
library(showtext)
library(ggtext)
library(systemfonts)


# ------------------------------------------------------------
# 2. LOAD DATA
# ------------------------------------------------------------

tuesdata      <- tidytuesdayR::tt_load('2026-02-24')
sfi_grants_raw <- tuesdata$sfi_grants


# ------------------------------------------------------------
# 3. CLEAN & SELECT COLUMNS
# ------------------------------------------------------------

sfi_grants <- sfi_grants_raw |>
  janitor::clean_names() |>
  dplyr::select(
    start_date,
    end_date,
    proposal_id,
    programme_name,
    sub_programme,
    supplement,
    research_body,
    research_body_ror_id,
    funder_name,
    crossref_funder_registry_id,
    proposal_title,
    current_total_commitment
  )


# ------------------------------------------------------------
# 4. AGGREGATE FUNDING BY INSTITUTION
# ------------------------------------------------------------

grants_received <- sfi_grants |>
  group_by(research_body) |>
  summarise(
    total_funding = sum(current_total_commitment, na.rm = TRUE)
  ) |>
  mutate(
    research_body = fct_reorder(research_body, total_funding)
  )

# Keep only the top 10 institutions by total funding
top_10 <- grants_received |>
  slice_max(total_funding, n = 10) |>
  arrange(desc(total_funding))


# ------------------------------------------------------------
# 5. BUILD PLOT
# ------------------------------------------------------------

# --- 5a. Base bar chart ---
plt <- ggplot(top_10) +
  geom_col(
    aes(x = total_funding, y = research_body),
    fill  = "#d95f02",
    width = 0.6
  )

# --- 5b. X-axis scale & panel theme ---
plt <- plt +
  scale_x_continuous(
    breaks   = seq(0, 1e9 + 1e8, by = 1e8),
    labels = function(x) ifelse(
      x == 0,   "0",
      ifelse(x == 1e9, "1B",
             label_number(scale = 1e-6, suffix = "M", accuracy = 1)(x))
    ),
    expand   = expansion(mult = c(0, 0.05)),
    position = "top"
  ) +
  theme(
    panel.background      = element_rect(fill = "white"),
    panel.grid.major.x    = element_line(color = "#A8BAC4", linewidth = 0.3),
    axis.ticks.length     = unit(0, "mm"),
    axis.title            = element_blank(),
    axis.line.y.left      = element_line(color = "black"),
    axis.text.y           = element_blank(),
    axis.text.x           = element_text(family = "Econ Sans Cnd", size = 16)
  )

# --- 5c. Bar labels (inside for top 6, outside for bottom 4) ---
plt <- plt +
  geom_text(
    data    = top_10[1:6, ],
    aes(x = 0, y = research_body, label = research_body),
    hjust   = 0,
    nudge_x = 5e6,
    colour  = "white",
    family  = "Econ Sans Cnd",
    size    = 5.5
  ) +
  geom_text(
    data    = top_10[7:10, ],
    aes(x = total_funding, y = research_body, label = research_body),
    hjust   = 0,
    nudge_x = 5e6,
    colour  = "#d95f02",
    family  = "Econ Sans Cnd",
    size    = 5.5
  ) +
  coord_cartesian(clip = "off")

# --- 5d. Title, subtitle & caption ---
plt <- plt +
  labs(
    title    = "Trinity College Leads Ireland's STEM Funding Race",
    subtitle = "Total grants awarded by Science Foundation Ireland per institution, 2000–2024",
    caption  = "Data: Ireland's Open Data Portal | LinkedIn: Emmanuel Prah  | GitHub: emmanuelprah"
  ) +
  theme(
    plot.title             = element_text(
      family = "Econ Sans Cnd",
      face   = "bold",
      size   = 20
    ),
    plot.subtitle          = element_text(
      family = "Econ Sans Cnd",
      size   = 18
    ),
    plot.caption           = element_markdown(
      family = "Econ Sans Cnd",
      size   = 12,
      color  = "gray40",
      hjust  = 0,
      margin = margin(t = 10)
    ),
    plot.caption.position  = "plot"
  )


# ------------------------------------------------------------
# 6. Save plot
# ------------------------------------------------------------

ggsave(
  filename = "outputs/20260224.png",
  dpi = 400,
  height = 6,
  width = 12,
  bg = "#FFFFFF"
)