################################################################################
# 15_tables_figures.R
# Manuscript tables and figures, built only from scripts 11, 13 and 14
#
# Inputs:
#   - outputs/tables/tbl_item_class_by_molecule.csv     (script 11)
#   - outputs/tables/tbl_item_brand_composition.csv     (script 11)
#   - outputs/tables/tbl_item_class_monthly.csv         (script 11)
#   - outputs/tables/tbl_item_class_monthly_archived.csv (script 13)
#   - outputs/tables/tbl_break_attribution.csv          (script 14)
#   - data/processed/market_share_national.csv          (conventional series)
#   - data/raw/pbs_prescribing/dos_data_raw.rds, medicare_stats_raw.rds
#   - data/reference/pbs_item_mapping.csv, policy_timeline.csv
# Outputs:
#   - outputs/tables/table1_bounds_nov2025.csv
#   - outputs/tables/table2_break_attribution.csv
#   - outputs/tables/tableA1_item_composition.csv
#   - outputs/figures/fig1_bounds_vs_conventional.{pdf,png}
#   - outputs/figures/fig2_rituximab_item_cohorts.{pdf,png}
#
# Palette: Okabe-Ito (blue #0072B2 bounds, vermillion #D55E00 conventional
# series, grey for earlier items), validated for colour-vision deficiency.
################################################################################

library(tidyverse)
library(here)
library(patchwork)
library(scales)
library(cli)

blue <- "#0072B2"
vermillion <- "#D55E00"
grey_fill <- "#BBBBBB"
mol_order <- c("adalimumab", "etanercept", "infliximab", "rituximab",
               "trastuzumab", "bevacizumab", "filgrastim", "pegfilgrastim",
               "insulin glargine", "aflibercept")

# cairo is unavailable on this machine: write with the base pdf device, then
# embed fonts with Ghostscript so the PDFs meet journal requirements
save_fig <- function(p, name, width, height) {
  f <- here("outputs", "figures", paste0(name, ".pdf"))
  ggsave(f, p, width = width, height = height, device = pdf, useDingbats = FALSE)
  tmp <- tempfile(fileext = ".pdf")
  embedFonts(f, outfile = tmp,
             options = "-dPDFSETTINGS=/prepress -dEmbedAllFonts=true -dSubsetFonts=true")
  file.copy(tmp, f, overwrite = TRUE)
  ggsave(here("outputs", "figures", paste0(name, ".png")), p,
         width = width, height = height, dpi = 300, bg = "white")
}

pct <- function(x) if_else(is.na(x), NA_character_, sprintf("%.1f", 100 * x))

# ─── Table 1: November 2025 bounds vs conventional estimate ─────────────────

by_mol <- read_csv(here("outputs", "tables", "tbl_item_class_by_molecule.csv"),
                   show_col_types = FALSE)
composition <- read_csv(here("outputs", "tables", "tbl_item_brand_composition.csv"),
                        col_types = cols(.default = "c"))

table1 <- tibble(molecule = mol_order) %>%
  left_join(by_mol, by = "molecule") %>%
  mutate(
    conventional = pct(latest_previous_share),
    lower = pct(latest_lower),
    upper = pct(latest_upper),
    width_pp = if_else(is.na(latest_lower), NA_character_,
                       sprintf("%.1f", 100 * (latest_upper - latest_lower))),
    mixed_pct = pct(latest_mixed),
    identification = case_when(
      molecule == "aflibercept" ~ "First biosimilar listed February 2026, after the data end",
      latest_upper - latest_lower < 0.005 & latest_lower > 0.995 ~ "Point-identified: no reference brand listed",
      latest_upper - latest_lower < 0.005 & latest_upper < 0.005 ~ "Point-identified: no biosimilar brand listed",
      latest_upper - latest_lower < 0.005 ~ "Point-identified: brands on separate items",
      TRUE ~ "Bounded: reference and biosimilar brands share items"
    )
  ) %>%
  select(molecule, total_rx = latest_total_rx, conventional, lower, upper,
         width_pp, mixed_pct, identification)

write_csv(table1, here("outputs", "tables", "table1_bounds_nov2025.csv"))
print(table1, width = Inf)

# ─── Table 2: breaks in the conventional series ─────────────────────────────

breaks <- read_csv(here("outputs", "tables", "tbl_break_attribution.csv"),
                   col_types = cols(ci_lower = "D", ci_upper = "D", window_start = "D",
                                    window_end = "D", change_month = "D"))

table2 <- breaks %>%
  arrange(factor(molecule, levels = mol_order), change_month) %>%
  transmute(molecule,
            change_month = format(change_month, "%b %Y"),
            share_before = pct(share_3m_before),
            share_after = pct(share_3m_after),
            change_pp = sprintf("%+.1f", 100 * change_6m),
            coincides_with, policy_events, item_events)

write_csv(table2, here("outputs", "tables", "table2_break_attribution.csv"))
print(table2, width = Inf)

# ─── Appendix Table A1: item composition, February 2026 Schedule ────────────

tableA1 <- composition %>%
  count(molecule, item_class) %>%
  pivot_wider(names_from = item_class, values_from = n, values_fill = 0) %>%
  mutate(total_items = rowSums(across(where(is.numeric)))) %>%
  left_join(composition %>% filter(item_class != "not_in_schedule") %>%
              separate_longer_delim(brands, "; ") %>%
              group_by(molecule) %>%
              summarise(brands_listed = paste(sort(unique(brands)), collapse = ", "),
                        .groups = "drop"),
            by = "molecule") %>%
  arrange(factor(molecule, levels = mol_order)) %>%
  select(molecule, total_items, any_of(c("reference_only", "biosimilar_only", "mixed",
                                          "not_in_schedule", "unrecognised_brand")),
         brands_listed)

write_csv(tableA1, here("outputs", "tables", "tableA1_item_composition.csv"))
print(tableA1, width = Inf)

# ─── Figure 1: bounds vs conventional series, four molecules ────────────────

timeline <- read_csv(here("data", "reference", "policy_timeline.csv"),
                     show_col_types = FALSE) %>% mutate(date = as.Date(date))
first_bio <- timeline %>% filter(intervention_type == "biosimilar_listing") %>%
  group_by(molecule) %>% summarise(first_bio_date = min(date), .groups = "drop")

fig_mols <- c("rituximab", "adalimumab", "etanercept", "infliximab")

conventional <- read_csv(here("data", "processed", "market_share_national.csv"),
                         show_col_types = FALSE) %>%
  filter(molecule %in% fig_mols) %>%
  select(molecule, period, conventional = biosimilar_share)

archived <- read_csv(here("outputs", "tables", "tbl_item_class_monthly_archived.csv"),
                     show_col_types = FALSE) %>%
  select(molecule, period, archived_lower, archived_upper)

snapshot <- read_csv(here("outputs", "tables", "tbl_item_class_monthly.csv"),
                     show_col_types = FALSE, guess_max = Inf) %>%
  filter(molecule %in% fig_mols) %>%
  select(molecule, period, snapshot_lower = bound_lower, snapshot_upper = bound_upper)

fig1_data <- conventional %>%
  left_join(first_bio, by = "molecule") %>%
  filter(period >= first_bio_date %m-% months(12)) %>%
  left_join(archived, by = c("molecule", "period")) %>%
  left_join(snapshot, by = c("molecule", "period")) %>%
  # before first listing the share is 0 by definition
  mutate(across(c(archived_lower, archived_upper),
                ~ if_else(period < first_bio_date, 0, .x)),
         molecule = factor(str_to_sentence(molecule), levels = str_to_sentence(fig_mols)))

fig1_breaks <- breaks %>%
  mutate(molecule = factor(str_to_sentence(molecule), levels = str_to_sentence(fig_mols)))

theme_fig <- theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
        strip.text = element_text(face = "bold", hjust = 0, size = 9),
        axis.title = element_text(size = 8.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0, size = 7, colour = "grey30"))

fig1 <- ggplot(fig1_data, aes(period)) +
  geom_ribbon(aes(ymin = archived_lower, ymax = archived_upper,
                  fill = "Bounds from archived item listings"), alpha = 0.25) +
  # edges drawn too, so bounds that coincide (point identification) stay visible
  geom_line(aes(y = archived_lower), colour = blue, linewidth = 0.3, alpha = 0.7, na.rm = TRUE) +
  geom_line(aes(y = archived_upper), colour = blue, linewidth = 0.3, alpha = 0.7, na.rm = TRUE) +
  geom_line(aes(y = snapshot_lower, linetype = "Bounds from February 2026 Schedule"),
            colour = blue, linewidth = 0.35, na.rm = TRUE) +
  geom_line(aes(y = snapshot_upper, linetype = "Bounds from February 2026 Schedule"),
            colour = blue, linewidth = 0.35, na.rm = TRUE) +
  geom_vline(data = fig1_breaks, aes(xintercept = change_month),
             linetype = "dotted", colour = "grey40", linewidth = 0.4) +
  geom_line(aes(y = conventional, colour = "Conventional item-classified share"),
            linewidth = 0.6) +
  facet_wrap(~ molecule, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = blue) +
  scale_colour_manual(values = vermillion) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  scale_x_date(date_labels = "%Y", breaks = breaks_pretty(n = 4),
               expand = expansion(mult = c(0.02, 0.07))) +
  labs(x = NULL, y = "Biosimilar share of prescriptions") +
  theme_fig +
  theme(panel.spacing.x = unit(1.2, "lines")) +
  guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2),
         colour = guide_legend(order = 3))

save_fig(fig1, "fig1_bounds_vs_conventional", 7.0, 5.2)

# ─── Figure 2: rituximab volume by item cohort ──────────────────────────────

mapping <- read_csv(here("data", "reference", "pbs_item_mapping.csv"),
                    col_types = cols(.default = "c"))
dos <- readRDS(here("data", "raw", "pbs_prescribing", "dos_data_raw.rds"))
med <- readRDS(here("data", "raw", "pbs_prescribing", "medicare_stats_raw.rds"))

ritux <- bind_rows(
  dos %>% select(item_number, period, prescriptions, state, source),
  med %>% select(item_number, period, prescriptions, state, source)
) %>%
  filter(state == "National") %>%
  group_by(item_number, period) %>%
  filter(n_distinct(source) == 1 | source == "dos") %>%
  ungroup() %>%
  inner_join(mapping %>% filter(molecule == "rituximab") %>% select(item_number), by = "item_number") %>%
  filter(prescriptions > 0)

cohort_start <- as.Date("2022-09-01")
first_month <- ritux %>% group_by(item_number) %>% summarise(first = min(period), .groups = "drop")

fig2_data <- ritux %>%
  left_join(first_month, by = "item_number") %>%
  mutate(cohort = if_else(first >= cohort_start, "Items first dispensed from September 2022",
                          "Items dispensed before September 2022")) %>%
  filter(period >= as.Date("2019-01-01"), period <= as.Date("2023-06-01")) %>%
  group_by(period, cohort) %>% summarise(rx = sum(prescriptions), .groups = "drop") %>%
  complete(period, cohort, fill = list(rx = 0)) %>%
  group_by(period) %>% mutate(share = rx / sum(rx)) %>% ungroup() %>%
  mutate(cohort = factor(cohort, levels = c("Items dispensed before September 2022",
                                            "Items first dispensed from September 2022")))

fig2_conv <- conventional %>% filter(molecule == "rituximab",
                                     period >= as.Date("2019-01-01"),
                                     period <= as.Date("2023-06-01"))

ritux_events <- tibble(
  date = as.Date(c("2019-10-01", "2021-04-01", "2021-10-01", "2022-09-01")),
  label = c("Riximyo\nlisted", "MabThera IV\nremoved", "MabThera SC\nremoved",
            "Unrestricted\nBenefit")
)

fig2 <- ggplot() +
  geom_area(data = fig2_data, aes(period, share, fill = cohort),
            colour = "white", linewidth = 0.3) +
  geom_line(data = fig2_conv, aes(period, conventional,
                                  colour = "Conventional item-classified share"),
            linewidth = 0.7) +
  geom_vline(data = ritux_events, aes(xintercept = date), linetype = "dashed",
             colour = "grey25", linewidth = 0.35) +
  geom_label(data = ritux_events, aes(date, 0.97, label = label), size = 2.3,
             linewidth = 0, fill = "white", alpha = 0.85, lineheight = 0.9,
             vjust = 1) +
  scale_fill_manual(values = c(grey_fill, blue)) +
  scale_colour_manual(values = vermillion) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1), expand = c(0, 0)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months",
               expand = expansion(mult = c(0, 0.03))) +
  labs(x = NULL, y = "Share of rituximab prescriptions") +
  theme_fig +
  theme(legend.direction = "vertical") +
  guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2))

save_fig(fig2, "fig2_rituximab_item_cohorts", 7.0, 4.0)

cli_alert_success("Wrote Tables 1, 2, A1 and Figures 1, 2")
