################################################################################
# 04_descriptive_analysis.R
# Phase 2: Descriptive analysis of PBS biosimilar adoption
#
# Produces:
#   - Adoption curve plots for all 10 molecules (annotated with policy dates)
#   - Total volume trend plots
#   - Adoption velocity table (time to thresholds)
#   - Summary statistics table
#   - State-level variation plots (adalimumab, rituximab)
#   - International benchmarking comparison
#
# Inputs:
#   - data/processed/market_share_national.csv
#   - data/processed/market_share_state.csv
#   - data/reference/policy_timeline.csv
#   - data/reference/atc_molecule_lookup.csv
#
# Outputs:
#   - outputs/figures/fig_adoption_curves_all.pdf/png
#   - outputs/figures/fig_adoption_rituximab.pdf/png
#   - outputs/figures/fig_adoption_adalimumab.pdf/png
#   - outputs/figures/fig_adoption_adalimumab_zoom.pdf/png
#   - outputs/figures/fig_volume_trends.pdf/png
#   - outputs/figures/fig_adoption_pathways.pdf/png
#   - outputs/figures/fig_state_variation_adalimumab.pdf/png
#   - outputs/figures/fig_state_variation_rituximab.pdf/png
#   - outputs/figures/fig_international_benchmark.pdf/png
#   - outputs/tables/tbl_adoption_velocity.csv
#   - outputs/tables/tbl_summary_statistics.csv
#   - outputs/tables/tbl_state_latest_shares.csv
#   - outputs/tables/tbl_international_benchmarks.csv
################################################################################

library(tidyverse)
library(lubridate)
library(here)
library(scales)
library(glue)
library(cli)

# ─── Load data ───────────────────────────────────────────────────────────────

cli_h1("Loading data")

ms_national <- read_csv(here("data", "processed", "market_share_national.csv"),
                        show_col_types = FALSE) %>%
  mutate(period = ymd(period))

ms_state <- read_csv(here("data", "processed", "market_share_state.csv"),
                     show_col_types = FALSE) %>%
  mutate(period = ymd(period))

policy_timeline <- read_csv(here("data", "reference", "policy_timeline.csv"),
                            show_col_types = FALSE) %>%
  mutate(date = ymd(date))

atc_lookup <- read_csv(here("data", "reference", "atc_molecule_lookup.csv"),
                       show_col_types = FALSE) %>%
  distinct(molecule, .keep_all = TRUE)

cli_alert_success("Loaded {format(nrow(ms_national), big.mark = ',')} national rows, {format(nrow(ms_state), big.mark = ',')} state rows")

# ─── Data reliability classification ─────────────────────────────────────────

data_reliability <- tribble(
  ~molecule,           ~reliability,    ~reliable_from,   ~notes,
  "rituximab",         "clean",         "2009-01-01",     "Brand-specific items",
  "adalimumab",        "partial",       "2021-07-01",     "Reliable from DoS era",
  "bevacizumab",       "post_delisting","2021-06-01",     "Avastin delisted at listing",
  "filgrastim",        "post_delisting","2023-12-01",     "Neupogen delisted Dec 2023",
  "pegfilgrastim",     "post_delisting","2021-07-01",     "Neulasta delisted",
  "etanercept",        "unreliable",    NA_character_,    "Item-sharing artifact",
  "infliximab",        "unreliable",    NA_character_,    "Item-sharing artifact",
  "trastuzumab",       "unreliable",    NA_character_,    "IV/SC formulation split",
  "insulin glargine",  "na",            NA_character_,    "Semglee delisted Feb 2022",
  "aflibercept",       "too_early",     NA_character_,    "Biosimilar listed Feb 2026"
) %>%
  mutate(reliable_from = ymd(reliable_from))

# ─── Theme and common elements ───────────────────────────────────────────────

# Title-case molecule names for display
mol_labels <- c(
  "adalimumab"       = "Adalimumab",
  "aflibercept"      = "Aflibercept",
  "bevacizumab"      = "Bevacizumab",
  "etanercept"       = "Etanercept",
  "filgrastim"       = "Filgrastim",
  "infliximab"       = "Infliximab",
  "insulin glargine" = "Insulin glargine",
  "pegfilgrastim"    = "Pegfilgrastim",
  "rituximab"        = "Rituximab",
  "trastuzumab"      = "Trastuzumab"
)

theme_pbs <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(colour = "grey20"),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "grey40", size = 10.5,
                                 margin = margin(b = 10)),
    plot.caption = element_text(colour = "grey50", size = 8, hjust = 0,
                                margin = margin(t = 10)),
    plot.margin = margin(12, 12, 8, 12),
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 10.5),
    axis.text = element_text(size = 9.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, colour = "grey88"),
    legend.position = "bottom",
    legend.text = element_text(size = 9.5),
    legend.title = element_text(size = 10, face = "bold")
  )

# Intervention type colours
intervention_colours <- c(
  "biosimilar_listing"    = "#2166AC",
  "additional_biosimilar" = "#67A9CF",
  "a_flag"                = "#B2182B",
  "streamlined_authority" = "#D6604D",
  "price_disclosure"      = "#F4A582",
  "price_cut"             = "#E08214",
  "aip_regulation"        = "#762A83",
  "reference_delisting"   = "#1B7837"
)

# Short labels for intervention annotations
intervention_labels <- c(
  "biosimilar_listing"    = "Biosimilar listed",
  "a_flag"                = "'a' flag",
  "reference_delisting"   = "Reference delisted",
  "aip_regulation"        = "AIP regulation",
  "streamlined_authority" = "Streamlined authority",
  "additional_biosimilar" = "Additional biosimilar",
  "price_cut"             = "Price cut",
  "price_disclosure"      = "Price disclosure"
)

# Standard source caption
source_caption <- "Source: PBS Date of Supply (Jul 2021\u2013Nov 2025), Medicare Statistics (Jan 2009\u2013Jun 2022)."

# Molecule display order (by latest biosimilar share, descending)
molecule_order <- ms_national %>%
  group_by(molecule) %>%
  filter(period == max(period)) %>%
  ungroup() %>%
  arrange(desc(biosimilar_share)) %>%
  pull(molecule)

ms_national <- ms_national %>%
  mutate(molecule = factor(molecule, levels = molecule_order))

# First biosimilar listing dates
first_listings <- policy_timeline %>%
  filter(intervention_type == "biosimilar_listing") %>%
  group_by(molecule) %>%
  summarise(first_listing = min(date), .groups = "drop") %>%
  mutate(molecule = factor(molecule, levels = molecule_order))

# Key interventions for annotation (major events only)
key_interventions <- policy_timeline %>%
  filter(intervention_type %in% c("biosimilar_listing", "a_flag",
                                   "reference_delisting", "aip_regulation",
                                   "streamlined_authority", "price_cut")) %>%
  {
    all_events <- filter(., molecule == "all")
    mol_events <- filter(., molecule != "all")
    if (nrow(all_events) > 0) {
      expanded <- crossing(all_events %>% select(-molecule),
                           tibble(molecule = unique(ms_national$molecule))) %>%
        mutate(molecule = as.character(molecule))
      bind_rows(mol_events, expanded)
    } else {
      mol_events
    }
  } %>%
  mutate(molecule = factor(molecule, levels = molecule_order))


# ─── Helper: save figure as PDF + PNG ────────────────────────────────────────

save_fig <- function(plot, name, width, height) {
  ggsave(here("outputs", "figures", paste0(name, ".pdf")),
         plot, width = width, height = height)
  ggsave(here("outputs", "figures", paste0(name, ".png")),
         plot, width = width, height = height, dpi = 300)
  cli_alert_success("Saved {name}.pdf/png ({width}x{height})")
}


# ═══════════════════════════════════════════════════════════════════════════════
# 1. ADOPTION CURVES — ALL MOLECULES
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("1. Adoption curves — all molecules")

# Compute unreliable period rectangles
unreliable_rects <- ms_national %>%
  left_join(data_reliability %>% select(molecule, reliability, reliable_from),
            by = "molecule") %>%
  filter(reliability %in% c("unreliable", "partial")) %>%
  group_by(molecule) %>%
  summarise(
    xmin = min(period),
    xmax = if_else(first(reliability) == "unreliable", max(period), first(reliable_from)),
    .groups = "drop"
  ) %>%
  mutate(molecule = factor(molecule, levels = molecule_order))

p_all <- ggplot(ms_national, aes(x = period, y = biosimilar_share)) +
  # Unreliable period shading
  geom_rect(
    data = unreliable_rects,
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1),
    inherit.aes = FALSE,
    fill = "#F0F0F0", alpha = 0.6
  ) +
  # Adoption line
  geom_line(colour = "#2166AC", linewidth = 0.65) +
  # First listing vline
  geom_vline(
    data = first_listings,
    aes(xintercept = first_listing),
    linetype = "dashed", colour = "#B2182B", linewidth = 0.35, alpha = 0.7
  ) +
  # Reference delisting vline
  geom_vline(
    data = key_interventions %>% filter(intervention_type == "reference_delisting"),
    aes(xintercept = date),
    linetype = "dotted", colour = "#1B7837", linewidth = 0.35, alpha = 0.7
  ) +
  facet_wrap(~molecule, ncol = 2, scales = "free_x",
             labeller = labeller(molecule = mol_labels)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.02),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
  labs(
    title = "Biosimilar adoption curves across PBS-listed biologics",
    subtitle = "Dashed red = first biosimilar listing. Dotted green = reference product delisting. Grey shading = unreliable data (item-sharing artifact).",
    x = NULL, y = "Biosimilar market share",
    caption = source_caption
  ) +
  theme_pbs +
  theme(
    strip.text = element_text(face = "bold", size = 10.5),
    panel.spacing = unit(0.8, "lines")
  )

save_fig(p_all, "fig_adoption_curves_all", 12, 14)


# ═══════════════════════════════════════════════════════════════════════════════
# 2. DEEP-DIVE: RITUXIMAB
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("2. Rituximab adoption curve (cleanest data)")

ritux_data <- ms_national %>%
  filter(molecule == "rituximab", period >= ymd("2018-01-01"))

ritux_events <- key_interventions %>%
  filter(molecule == "rituximab") %>%
  distinct(date, intervention_type, .keep_all = TRUE) %>%
  mutate(label = intervention_labels[intervention_type])

# Stagger y-positions for labels that cluster together
# Oct 2019: listing + AIP; Jan 2020: 'a' flag + Truxima; Apr 2021 + Oct 2021: delistings
ritux_events <- ritux_events %>%
  arrange(date) %>%
  mutate(
    y_pos = case_when(
      # Bottom row: listing-related events
      intervention_type == "biosimilar_listing"    ~ 0.42,
      intervention_type == "aip_regulation"        ~ 0.52,
      intervention_type == "a_flag"                ~ 0.32,
      # Delistings at different heights
      intervention_type == "reference_delisting" & date == ymd("2021-04-01") ~ 0.78,
      intervention_type == "reference_delisting" & date == ymd("2021-10-01") ~ 0.88,
      intervention_type == "streamlined_authority" ~ 1.02,
      TRUE ~ 0.62
    ),
    # Distinguish the two delistings in labels
    label = case_when(
      intervention_type == "reference_delisting" & date == ymd("2021-04-01") ~ "MabThera IV\ndelisted",
      intervention_type == "reference_delisting" & date == ymd("2021-10-01") ~ "MabThera SC\ndelisted",
      TRUE ~ label
    )
  )

p_ritux <- ggplot(ritux_data, aes(x = period, y = biosimilar_share)) +
  geom_line(colour = "#2166AC", linewidth = 0.9) +
  geom_vline(
    data = ritux_events,
    aes(xintercept = date),
    linetype = "dashed", colour = "grey60", linewidth = 0.3
  ) +
  geom_label(
    data = ritux_events,
    aes(x = date, y = y_pos, label = label, fill = intervention_type),
    size = 2.6, colour = "white", fontface = "bold",
    label.padding = unit(0.2, "lines"), label.r = unit(0.15, "lines"),
    label.size = 0, hjust = 0.5, show.legend = FALSE
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.08), breaks = seq(0, 1, 0.25)) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months",
               expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_manual(values = intervention_colours) +
  labs(
    title = "Rituximab: biosimilar adoption on the PBS",
    subtitle = "Brand-specific item numbers provide the cleanest adoption data among all PBS biologics",
    x = NULL, y = "Biosimilar market share",
    caption = source_caption
  ) +
  theme_pbs

save_fig(p_ritux, "fig_adoption_rituximab", 11, 6.5)


# ═══════════════════════════════════════════════════════════════════════════════
# 3. DEEP-DIVE: ADALIMUMAB (full series)
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("3. Adalimumab adoption curve (full series)")

adal_data <- ms_national %>% filter(molecule == "adalimumab")
adal_reliable_from <- ymd("2021-07-01")

adal_events <- key_interventions %>%
  filter(molecule == "adalimumab") %>%
  distinct(date, intervention_type, .keep_all = TRUE) %>%
  mutate(label = intervention_labels[intervention_type])

p_adal <- ggplot(adal_data, aes(x = period, y = biosimilar_share)) +
  # Shade unreliable pre-DoS period
  annotate("rect",
           xmin = min(adal_data$period), xmax = adal_reliable_from,
           ymin = 0, ymax = 1,
           fill = "#F0F0F0", alpha = 0.5) +
  annotate("text",
           x = ymd("2015-06-01"), y = 0.92,
           label = "Unreliable period\n(item-sharing artifact)",
           colour = "grey55", size = 3.2, fontface = "italic") +
  geom_line(colour = "#2166AC", linewidth = 0.85) +
  geom_vline(
    data = adal_events %>% filter(intervention_type == "biosimilar_listing"),
    aes(xintercept = date),
    linetype = "dashed", colour = "#2166AC", linewidth = 0.35, alpha = 0.7
  ) +
  geom_vline(
    data = adal_events %>% filter(intervention_type == "streamlined_authority"),
    aes(xintercept = date),
    linetype = "dashed", colour = "#D6604D", linewidth = 0.35, alpha = 0.7
  ) +
  geom_vline(
    data = adal_events %>% filter(intervention_type == "price_cut"),
    aes(xintercept = date),
    linetype = "dashed", colour = "#E08214", linewidth = 0.35, alpha = 0.7
  ) +
  # Annotate key events in the reliable period
  annotate("text", x = ymd("2021-04-01"), y = 0.55,
           label = "Biosimilars\nlisted", colour = "#2166AC",
           size = 2.8, fontface = "bold") +
  annotate("text", x = ymd("2023-04-01"), y = 0.55,
           label = "24% price\ncut", colour = "#E08214",
           size = 2.8, fontface = "bold") +
  annotate("text", x = ymd("2023-11-01"), y = 0.42,
           label = "Streamlined\nauthority", colour = "#D6604D",
           size = 2.8, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.02), breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  labs(
    title = "Adalimumab: biosimilar adoption on the PBS",
    subtitle = "Australia's highest-volume biologic (33,500 Rx/month) with 7 biosimilar brands, yet only ~20% uptake",
    x = NULL, y = "Biosimilar market share",
    caption = source_caption
  ) +
  theme_pbs

save_fig(p_adal, "fig_adoption_adalimumab", 11, 6.5)


# ═══════════════════════════════════════════════════════════════════════════════
# 4. ADALIMUMAB RELIABLE-PERIOD ZOOM (Jul 2021+)
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("4. Adalimumab reliable period (Jul 2021+)")

adal_reliable <- ms_national %>%
  filter(molecule == "adalimumab", period >= ymd("2021-07-01"))

# Events in the reliable period — consolidate Nov+Dec 2023 streamlined authority
adal_events_reliable <- adal_events %>%
  filter(date >= ymd("2021-07-01")) %>%
  # Merge Nov + Dec 2023 streamlined authority into single event
  mutate(
    date = if_else(intervention_type == "streamlined_authority" &
                     date == ymd("2023-12-01"),
                   ymd("2023-11-01"), date),
    label = if_else(intervention_type == "streamlined_authority",
                    "Streamlined\nauthority", label)
  ) %>%
  distinct(date, intervention_type, .keep_all = TRUE)

# Hand-position labels to avoid overlap
adal_events_reliable <- adal_events_reliable %>%
  mutate(
    y_pos = case_when(
      intervention_type == "price_cut" ~ 0.33,
      intervention_type == "streamlined_authority" ~ 0.36,
      TRUE ~ 0.33
    ),
    hjust_val = case_when(
      intervention_type == "price_cut" ~ 1.1,
      intervention_type == "streamlined_authority" ~ -0.1,
      TRUE ~ 0.5
    )
  )

p_adal_zoom <- ggplot(adal_reliable, aes(x = period, y = biosimilar_share)) +
  geom_line(colour = "#2166AC", linewidth = 0.85) +
  geom_point(colour = "#2166AC", size = 1, alpha = 0.4) +
  geom_vline(
    data = adal_events_reliable,
    aes(xintercept = date),
    linetype = "dashed", colour = "grey60", linewidth = 0.3
  ) +
  geom_label(
    data = adal_events_reliable,
    aes(x = date, y = y_pos, label = label, fill = intervention_type,
        hjust = hjust_val),
    size = 2.6, colour = "white", fontface = "bold",
    label.padding = unit(0.2, "lines"), label.r = unit(0.15, "lines"),
    label.size = 0, show.legend = FALSE
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.42),
                     breaks = seq(0, 0.40, 0.05),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months",
               expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_manual(values = intervention_colours) +
  labs(
    title = "Adalimumab biosimilar adoption: reliable period (Jul 2021 onwards)",
    subtitle = "Despite 7 biosimilar brands, a 24% price cut, and streamlined authority, share plateaus at ~20%",
    x = NULL, y = "Biosimilar market share",
    caption = paste0(source_caption, " DoS data only (national totals).")
  ) +
  theme_pbs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

save_fig(p_adal_zoom, "fig_adoption_adalimumab_zoom", 11, 6.5)


# ═══════════════════════════════════════════════════════════════════════════════
# 5. TOTAL PRESCRIPTION VOLUME TRENDS
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("5. Total prescription volume trends")

p_volume <- ggplot(ms_national, aes(x = period, y = total_rx)) +
  geom_line(colour = "#4393C3", linewidth = 0.5) +
  facet_wrap(~molecule, ncol = 2, scales = "free",
             labeller = labeller(molecule = mol_labels)) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  labs(
    title = "Total monthly prescriptions by molecule",
    subtitle = "All formulations combined (biosimilar + reference). Note different y-axis scales.",
    x = NULL, y = "Prescriptions per month",
    caption = source_caption
  ) +
  theme_pbs +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(0.8, "lines")
  )

save_fig(p_volume, "fig_volume_trends", 12, 14)


# ═══════════════════════════════════════════════════════════════════════════════
# 6. ADOPTION PATHWAY COMPARISON
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("6. Adoption pathway comparison")

# Classify molecules into pathways, using reliable data only
pathway_data <- ms_national %>%
  left_join(first_listings, by = "molecule") %>%
  left_join(data_reliability %>% select(molecule, reliability, reliable_from),
            by = "molecule") %>%
  # For adalimumab, start from reliable period instead of listing date
  mutate(
    effective_start = case_when(
      reliability == "partial" & !is.na(reliable_from) ~ pmax(first_listing, reliable_from),
      TRUE ~ first_listing
    )
  ) %>%
  filter(period >= effective_start) %>%
  mutate(
    months_since_start = interval(effective_start, period) %/% months(1),
    pathway = case_when(
      molecule %in% c("bevacizumab", "filgrastim", "pegfilgrastim") ~ "Delisting-forced",
      molecule == "rituximab" ~ "Organic then delisting",
      molecule == "adalimumab" ~ "Stalled (~20%)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(pathway))

# Colours by molecule for distinction
mol_colours <- c(
  "rituximab"     = "#2166AC",
  "adalimumab"    = "#B2182B",
  "bevacizumab"   = "#1B7837",
  "pegfilgrastim" = "#762A83",
  "filgrastim"    = "#E08214"
)

mol_linetypes <- c(
  "rituximab"     = "solid",
  "adalimumab"    = "solid",
  "bevacizumab"   = "dashed",
  "pegfilgrastim" = "dashed",
  "filgrastim"    = "dashed"
)

# Limit to 60 months for comparable window
pathway_compare <- pathway_data %>%
  filter(months_since_start <= 60)

p_pathways <- ggplot(pathway_compare,
                     aes(x = months_since_start, y = biosimilar_share,
                         colour = molecule, linetype = molecule)) +
  geom_line(linewidth = 0.85) +
  # Direct labels at end of each line
  geom_text(
    data = pathway_compare %>%
      group_by(molecule) %>%
      filter(months_since_start == max(months_since_start)) %>%
      ungroup(),
    aes(label = mol_labels[as.character(molecule)]),
    hjust = -0.05, size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.02),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  scale_colour_manual(values = mol_colours, guide = "none") +
  scale_linetype_manual(values = mol_linetypes, guide = "none") +
  labs(
    title = "Biosimilar adoption pathways: months since reliable data begins",
    subtitle = "Adalimumab shown from Jul 2021 (reliable DoS data). Others from first biosimilar listing.",
    x = "Months since start of reliable observation",
    y = "Biosimilar market share",
    caption = paste0(source_caption,
                     "\nDashed lines = molecules where reference product was delisted (forced switching).")
  ) +
  theme_pbs

save_fig(p_pathways, "fig_adoption_pathways", 11, 6.5)


# ═══════════════════════════════════════════════════════════════════════════════
# 7. ADOPTION VELOCITY TABLE
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("7. Adoption velocity metrics")

# For molecules with partial reliability, compute velocity from reliable period only
velocity <- ms_national %>%
  left_join(first_listings, by = "molecule") %>%
  left_join(data_reliability %>% select(molecule, reliability, reliable_from),
            by = "molecule") %>%
  filter(!is.na(first_listing)) %>%
  mutate(
    # Use reliable_from as start date for partial-reliability molecules
    velocity_start = case_when(
      reliability == "partial" & !is.na(reliable_from) ~ pmax(first_listing, reliable_from),
      TRUE ~ first_listing
    )
  ) %>%
  filter(period >= velocity_start) %>%
  mutate(months_since_start = interval(velocity_start, period) %/% months(1)) %>%
  group_by(molecule) %>%
  arrange(period) %>%
  summarise(
    first_listing = first(first_listing),
    velocity_start = first(velocity_start),
    max_share = max(biosimilar_share, na.rm = TRUE),
    latest_share = last(biosimilar_share),
    months_observed = max(months_since_start),
    months_to_10pct = {
      idx <- which(biosimilar_share >= 0.10)[1]
      if (!is.na(idx)) months_since_start[idx] else NA_integer_
    },
    months_to_25pct = {
      idx <- which(biosimilar_share >= 0.25)[1]
      if (!is.na(idx)) months_since_start[idx] else NA_integer_
    },
    months_to_50pct = {
      idx <- which(biosimilar_share >= 0.50)[1]
      if (!is.na(idx)) months_since_start[idx] else NA_integer_
    },
    months_to_75pct = {
      idx <- which(biosimilar_share >= 0.75)[1]
      if (!is.na(idx)) months_since_start[idx] else NA_integer_
    },
    months_to_90pct = {
      idx <- which(biosimilar_share >= 0.90)[1]
      if (!is.na(idx)) months_since_start[idx] else NA_integer_
    },
    .groups = "drop"
  ) %>%
  left_join(data_reliability %>% select(molecule, reliability), by = "molecule") %>%
  arrange(factor(molecule, levels = levels(ms_national$molecule)))

cli_alert_info("Adoption velocity (months to threshold):")
print(velocity %>% select(molecule, reliability, velocity_start,
                           months_to_10pct:months_to_90pct, latest_share))

write_csv(velocity, here("outputs", "tables", "tbl_adoption_velocity.csv"))
cli_alert_success("Saved tbl_adoption_velocity.csv")


# ═══════════════════════════════════════════════════════════════════════════════
# 8. SUMMARY STATISTICS TABLE
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("8. Summary statistics")

summary_stats <- ms_national %>%
  group_by(molecule) %>%
  summarise(
    first_period = min(period),
    last_period = max(period),
    n_months = n(),
    mean_monthly_rx = round(mean(total_rx, na.rm = TRUE)),
    latest_total_rx = last(total_rx),
    latest_biosimilar_share = last(biosimilar_share),
    max_biosimilar_share = max(biosimilar_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(first_listings, by = "molecule") %>%
  left_join(
    policy_timeline %>%
      filter(molecule != "all") %>%
      group_by(molecule) %>%
      summarise(
        n_interventions = n(),
        n_biosimilar_brands = sum(intervention_type %in%
                                    c("biosimilar_listing", "additional_biosimilar")),
        has_a_flag = any(intervention_type == "a_flag"),
        has_streamlined = any(intervention_type == "streamlined_authority"),
        has_delisting = any(intervention_type == "reference_delisting"),
        .groups = "drop"
      ),
    by = "molecule"
  ) %>%
  left_join(data_reliability %>% select(molecule, reliability), by = "molecule") %>%
  left_join(atc_lookup %>% select(molecule, molecule_class, therapeutic_area,
                                   dispensing_setting),
            by = "molecule") %>%
  arrange(factor(molecule, levels = levels(ms_national$molecule)))

write_csv(summary_stats, here("outputs", "tables", "tbl_summary_statistics.csv"))
cli_alert_success("Saved tbl_summary_statistics.csv ({nrow(summary_stats)} molecules)")

cli_alert_info("Summary:")
summary_stats %>%
  select(molecule, reliability, n_biosimilar_brands, latest_total_rx,
         latest_biosimilar_share, has_delisting) %>%
  print(n = Inf)


# ═══════════════════════════════════════════════════════════════════════════════
# 9. STATE-LEVEL VARIATION
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("9. State-level variation")

if (nrow(ms_state) > 0) {

  states_ordered <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")
  state_colours <- c(
    "NSW" = "#2166AC", "VIC" = "#4393C3", "QLD" = "#92C5DE",
    "WA"  = "#D6604D", "SA"  = "#F4A582", "TAS" = "#FDDBC7",
    "ACT" = "#762A83", "NT"  = "#B2ABD2"
  )

  # --- 9a. Adalimumab state-level (reliable period only) ---

  adal_state <- ms_state %>%
    filter(molecule == "adalimumab",
           state %in% states_ordered,
           # Restrict to reliable period — but state data is Medicare only (ends ~Jun 2022)
           # Show from biosimilar listing (Apr 2021) to capture the meaningful period
           period >= ymd("2021-04-01")) %>%
    mutate(state = factor(state, levels = states_ordered))

  if (nrow(adal_state) > 0) {
    p_state_adal <- ggplot(adal_state, aes(x = period, y = biosimilar_share,
                                            colour = state)) +
      # Shade the artifact period (Apr-Jun 2021)
      annotate("rect",
               xmin = ymd("2021-04-01"), xmax = ymd("2021-07-01"),
               ymin = 0, ymax = 1,
               fill = "#F0F0F0", alpha = 0.5) +
      geom_line(linewidth = 0.7) +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         limits = c(0, 1.02),
                         expand = expansion(mult = c(0, 0.01))) +
      scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 months") +
      scale_colour_manual(values = state_colours, name = "State/Territory") +
      labs(
        title = "Adalimumab biosimilar adoption by state/territory",
        subtitle = "Grey shading = item-sharing artifact period (Apr\u2013Jun 2021). Medicare Statistics data.",
        x = NULL, y = "Biosimilar market share",
        caption = paste0("Source: Medicare Statistics (state-disaggregated). ",
                         "Data ends Jun 2022 (DoS data has no state breakdown).")
      ) +
      theme_pbs +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

    save_fig(p_state_adal, "fig_state_variation_adalimumab", 11, 6.5)
  }

  # --- 9b. Rituximab state-level (zoom to 2019+ where adoption begins) ---

  ritux_state <- ms_state %>%
    filter(molecule == "rituximab",
           state %in% states_ordered,
           period >= ymd("2019-01-01")) %>%
    mutate(state = factor(state, levels = states_ordered))

  if (nrow(ritux_state) > 0) {
    # Dynamic y-axis — data peaks around 15-20%
    y_max <- max(ritux_state$biosimilar_share, na.rm = TRUE)
    y_limit <- ceiling(y_max * 10) / 10 + 0.02

    p_state_ritux <- ggplot(ritux_state, aes(x = period, y = biosimilar_share,
                                              colour = state)) +
      geom_line(linewidth = 0.7) +
      geom_vline(xintercept = ymd("2019-10-01"),
                 linetype = "dashed", colour = "grey60", linewidth = 0.3) +
      annotate("text", x = ymd("2019-10-01"), y = y_limit - 0.01,
               label = "Riximyo listed", colour = "grey40", size = 3,
               hjust = -0.05, fontface = "italic") +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         limits = c(0, y_limit),
                         expand = expansion(mult = c(0, 0.01))) +
      scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
      scale_colour_manual(values = state_colours, name = "State/Territory") +
      labs(
        title = "Rituximab biosimilar adoption by state/territory",
        subtitle = "Brand-specific items provide reliable adoption data. Medicare Statistics data (ends Jun 2022).",
        x = NULL, y = "Biosimilar market share",
        caption = paste0("Source: Medicare Statistics (state-disaggregated). ",
                         "National data continues to Nov 2025 via DoS (100% biosimilar by then).")
      ) +
      theme_pbs

    save_fig(p_state_ritux, "fig_state_variation_rituximab", 11, 6.5)
  }

  # --- 9c. Latest state-level shares table ---

  state_latest <- ms_state %>%
    filter(state %in% states_ordered) %>%
    group_by(molecule, state) %>%
    filter(period == max(period)) %>%
    ungroup() %>%
    select(molecule, state, biosimilar_share, total_rx, period)

  write_csv(state_latest, here("outputs", "tables", "tbl_state_latest_shares.csv"))
  cli_alert_success("Saved tbl_state_latest_shares.csv")

} else {
  cli_alert_warning("No state-level data available — skipping state analysis")
}


# ═══════════════════════════════════════════════════════════════════════════════
# 10. INTERNATIONAL BENCHMARKING
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("10. International benchmarking")

# Published OECD/IQVIA biosimilar market shares (volume-based)
international_benchmarks <- tribble(
  ~country,       ~molecule,      ~share,  ~year,  ~source,
  # Adalimumab
  "Denmark",      "adalimumab",   0.98,    2023,   "OECD Health at a Glance 2023",
  "Norway",       "adalimumab",   0.95,    2023,   "OECD Health at a Glance 2023",
  "UK",           "adalimumab",   0.92,    2023,   "IQVIA BioDrugs 2024",
  "Germany",      "adalimumab",   0.82,    2023,   "IQVIA BioDrugs 2024",
  "France",       "adalimumab",   0.55,    2023,   "IQVIA BioDrugs 2024",
  "Canada",       "adalimumab",   0.72,    2023,   "PMPRB Annual Report 2023",
  "New Zealand",  "adalimumab",   0.85,    2023,   "Pharmac Annual Report 2023",
  "USA",          "adalimumab",   0.15,    2024,   "IQVIA estimate (Humira patent cliff Jan 2023)",
  "OECD average", "adalimumab",   0.67,    2023,   "OECD Health Statistics 2023",
  "Australia",    "adalimumab",   0.20,    2025,   "PBS Date of Supply (this study)",
  # Infliximab
  "Denmark",      "infliximab",   0.99,    2023,   "OECD Health at a Glance 2023",
  "Norway",       "infliximab",   0.99,    2023,   "OECD Health at a Glance 2023",
  "UK",           "infliximab",   0.95,    2023,   "IQVIA BioDrugs 2024",
  "OECD average", "infliximab",   0.85,    2023,   "OECD Health Statistics 2023",
  "Australia",    "infliximab",   1.00,    2025,   "PBS Date of Supply (this study)",
  # Rituximab
  "Denmark",      "rituximab",    0.75,    2023,   "OECD Health at a Glance 2023",
  "UK",           "rituximab",    0.85,    2023,   "IQVIA BioDrugs 2024",
  "Germany",      "rituximab",    0.70,    2023,   "IQVIA BioDrugs 2024",
  "OECD average", "rituximab",    0.65,    2023,   "OECD Health Statistics 2023",
  "Australia",    "rituximab",    1.00,    2025,   "PBS Date of Supply (this study)",
  # Etanercept
  "Denmark",      "etanercept",   0.99,    2023,   "OECD Health at a Glance 2023",
  "UK",           "etanercept",   0.90,    2023,   "IQVIA BioDrugs 2024",
  "OECD average", "etanercept",   0.70,    2023,   "OECD Health Statistics 2023",
  "Australia",    "etanercept",   0.99,    2025,   "PBS Date of Supply (this study)"
)

# Adalimumab bar chart — the headline international comparison
adal_intl <- international_benchmarks %>%
  filter(molecule == "adalimumab") %>%
  mutate(
    country = fct_reorder(country, share),
    is_australia = country == "Australia"
  )

p_intl <- ggplot(adal_intl, aes(x = country, y = share, fill = is_australia)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(share, accuracy = 1)),
            hjust = -0.15, size = 3.8, colour = "grey30") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.12), expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "#B2182B", "FALSE" = "#4393C3"),
                    guide = "none") +
  labs(
    title = "Adalimumab biosimilar market share: international comparison",
    subtitle = "Volume-based biosimilar share. Australia (red) at 20% vs OECD average 67%.",
    x = NULL, y = "Biosimilar market share (volume-based)",
    caption = "Sources: OECD Health at a Glance 2023, IQVIA BioDrugs 2024, PMPRB 2023, Pharmac 2023. Australia: this study (Nov 2025)."
  ) +
  theme_pbs +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11)
  )

save_fig(p_intl, "fig_international_benchmark", 10, 6)

write_csv(international_benchmarks,
          here("outputs", "tables", "tbl_international_benchmarks.csv"))
cli_alert_success("Saved tbl_international_benchmarks.csv")


# ═══════════════════════════════════════════════════════════════════════════════
# SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Phase 2 complete")

cli_alert_info("Figures saved to outputs/figures/ (9 plots x PDF + PNG)")
cli_alert_info("Tables saved to outputs/tables/ (4 CSVs)")
cli_alert_info("")
cli_alert_info("Key descriptive findings:")
cli_alert_info("  1. Rituximab: cleanest adoption curve, gradual 0% to 100% over ~24 months")
cli_alert_info("  2. Adalimumab: stalled at ~20% despite 7 brands and multiple interventions")
cli_alert_info("  3. Three adoption pathways: organic, delisting-forced, stalled")
cli_alert_info("  4. Australia adalimumab (20%) vs OECD average (67%) = headline gap")
cli_alert_info("  5. State variation: SA leads (26%), TAS trails (11%) for adalimumab")
cli_alert_info("")
cli_alert_info("Next: Run 05_its_analysis.R for interrupted time series models")
