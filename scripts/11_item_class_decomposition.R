################################################################################
# 11_item_class_decomposition.R
# Decomposes monthly PBS volume by item brand composition, and bounds the true
# biosimilar share that item-level statistics can and cannot identify
#
# PBS item numbers are not brand-specific: one item can list the reference
# product and its 'a'-flagged biosimilars together, and public dispensing
# statistics report by item, not brand. Each item is therefore classed by the
# brands it lists in the PBS Schedule snapshot:
#   reference_only  - lists only reference brand(s)
#   biosimilar_only - lists only biosimilar brand(s)
#   mixed           - lists reference and biosimilar brands together
#   not_in_schedule - absent from the snapshot; brand composition unknown
#
# Bounds on true biosimilar share in month t (after first biosimilar listing):
#   lower = biosimilar_only / total
#   upper = 1 - reference_only / total
# Mixed and not_in_schedule volume could be either. Before first listing the
# share is 0 by definition (no biosimilar existed).
#
# Inputs:
#   - data/raw/pbs_prescribing/dos_data_raw.rds
#   - data/raw/pbs_prescribing/medicare_stats_raw.rds
#   - data/raw/pbs_schedule/pbs-api-csv/tables_as_csv/items.csv
#   - data/raw/pbs_schedule/pbs-api-csv/tables_as_csv/schedules.csv
#   - data/reference/pbs_item_mapping.csv
#   - data/reference/policy_timeline.csv
#
# Outputs:
#   - outputs/tables/tbl_item_brand_composition.csv
#   - outputs/tables/tbl_item_class_monthly.csv
#   - outputs/tables/tbl_item_class_summary.csv
#   - outputs/tables/tbl_item_class_by_molecule.csv
#
# Assumption: item brand composition is taken from ONE schedule snapshot and
# applied to every month. Items are reassigned between brands over time (e.g.
# adalimumab items that are biosimilar-only today carried pre-listing volume),
# so the bounds hold only where the snapshot composition held. Scripts 12 and
# 13 test this against archived item pages: the snapshot holds from 2023 for
# the four molecules checked, but not earlier.
################################################################################

library(tidyverse)
library(here)
library(cli)

# ─── Brand dictionaries (same lists as 01_item_mapping.R) ────────────────────

reference_brands <- c(
  "neupogen", "remicade", "enbrel", "mabthera", "herceptin",
  "neulasta", "avastin", "lantus", "optisulin", "toujeo",
  "humira", "eylea"
)

biosimilar_brands <- c(
  "nivestim", "zarzio", "tevagrastim", "releuko",
  "inflectra", "renflexis", "remsima", "ixifi",
  "brenzys", "erelzi", "nepexto",
  "riximyo", "truxima", "ruxience",
  "ogivri", "herzuma", "kanjinti", "ontruzant", "trazimera",
  "ziextenzo", "pelgraz", "fulphila",
  "mvasi", "abevmy", "vegzelma",
  "semglee",
  "amgevita", "hadlima", "hyrimoz", "idacio", "yuflyma", "adalicip", "abrilada",
  "afqlir"
)

brand_type <- function(b) {
  b <- tolower(b)
  case_when(
    str_detect(b, paste(biosimilar_brands, collapse = "|")) ~ "biosimilar",
    str_detect(b, paste(reference_brands, collapse = "|")) ~ "reference",
    TRUE ~ "unrecognised"
  )
}

# ─── Schedule snapshot: brands listed per item ───────────────────────────────

cli_h1("Item brand composition from PBS Schedule snapshot")

sched_dir <- here("data", "raw", "pbs_schedule", "pbs-api-csv", "tables_as_csv")
snapshot <- read_csv(file.path(sched_dir, "schedules.csv"), show_col_types = FALSE)
cli_alert_info("Schedule snapshot effective {snapshot$effective_date[1]} (schedule code {snapshot$schedule_code[1]})")

item_mapping <- read_csv(here("data", "reference", "pbs_item_mapping.csv"),
                         col_types = cols(.default = "c"))

schedule_items <- read_csv(file.path(sched_dir, "items.csv"),
                           col_types = cols(.default = "c")) %>%
  mutate(pbs_code = str_pad(pbs_code, 6, "left", "0")) %>%
  filter(pbs_code %in% item_mapping$item_number) %>%
  mutate(btype = brand_type(brand_name))

composition <- schedule_items %>%
  group_by(item_number = pbs_code) %>%
  summarise(
    brands = paste(sort(unique(brand_name)), collapse = "; "),
    n_brands = n_distinct(brand_name),
    has_ref = any(btype == "reference"),
    has_bio = any(btype == "biosimilar"),
    has_unrecognised = any(btype == "unrecognised"),
    first_listed_date = min(first_listed_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(item_class = case_when(
    has_ref & has_bio ~ "mixed",
    has_ref ~ "reference_only",
    has_bio ~ "biosimilar_only",
    TRUE ~ "unrecognised_brand"
  ))

composition_out <- item_mapping %>%
  select(item_number, molecule, previous_type = type) %>%
  left_join(composition, by = "item_number") %>%
  mutate(item_class = coalesce(item_class, "not_in_schedule"))

if (any(composition_out$item_class == "unrecognised_brand")) {
  cli_alert_warning("Items with brands not in either dictionary:")
  composition_out %>% filter(item_class == "unrecognised_brand") %>%
    distinct(molecule, brands) %>% print(n = Inf)
}

write_csv(composition_out, here("outputs", "tables", "tbl_item_brand_composition.csv"))

composition_out %>%
  count(molecule, item_class) %>%
  pivot_wider(names_from = item_class, values_from = n, values_fill = 0) %>%
  print(n = Inf)

# ─── Monthly national volume by item class ───────────────────────────────────

cli_h1("Monthly national volume by item class")

dos <- readRDS(here("data", "raw", "pbs_prescribing", "dos_data_raw.rds"))
med <- readRDS(here("data", "raw", "pbs_prescribing", "medicare_stats_raw.rds"))

# National rows only (Medicare Statistics also carries jurisdiction rows that
# sum to the national total). Where the sources overlap, prefer DoS, as in
# 03_market_share.R.
rx <- bind_rows(
  dos %>% select(item_number, period, prescriptions, state, source),
  med %>% select(item_number, period, prescriptions, state, source)
) %>%
  filter(state == "National") %>%
  group_by(item_number, period) %>%
  filter(n_distinct(source) == 1 | source == "dos") %>%
  ungroup() %>%
  inner_join(composition_out %>% select(item_number, molecule, item_class),
             by = "item_number")

first_bio <- read_csv(here("data", "reference", "policy_timeline.csv"),
                      show_col_types = FALSE) %>%
  filter(intervention_type == "biosimilar_listing") %>%
  group_by(molecule) %>%
  summarise(first_bio_date = min(as.Date(date)), .groups = "drop")

# Verified dates from which NO reference brand of the molecule was PBS-listed.
# From then on true biosimilar share is 1 by definition, whatever the items
# say. Only dates confirmed from primary sources go here (see
# data/reference/sources/README.md); timeline "reference_delisting" rows are
# not used directly because some are partial (e.g. one formulation only).
reference_withdrawn <- tribble(
  ~molecule,   ~ref_withdrawn_date,
  "rituximab", as.Date("2021-10-01")   # MabThera IV 1 Apr 2021, SC 1 Oct 2021
)

monthly <- rx %>%
  group_by(molecule, period, item_class) %>%
  summarise(rx = sum(prescriptions), .groups = "drop") %>%
  complete(nesting(molecule, period),
           item_class = c("reference_only", "biosimilar_only", "mixed",
                          "not_in_schedule", "unrecognised_brand"),
           fill = list(rx = 0)) %>%
  pivot_wider(names_from = item_class, values_from = rx) %>%
  mutate(total_rx = reference_only + biosimilar_only + mixed +
           not_in_schedule + unrecognised_brand) %>%
  left_join(first_bio, by = "molecule") %>%
  left_join(reference_withdrawn, by = "molecule") %>%
  mutate(
    post_listing = period >= first_bio_date,
    ref_withdrawn = !is.na(ref_withdrawn_date) & period >= ref_withdrawn_date,
    share_biosimilar_only = biosimilar_only / total_rx,
    share_reference_only = reference_only / total_rx,
    share_mixed = mixed / total_rx,
    share_not_in_schedule = not_in_schedule / total_rx,
    bound_lower = case_when(!post_listing ~ 0, ref_withdrawn ~ 1,
                            TRUE ~ share_biosimilar_only),
    bound_upper = case_when(!post_listing ~ 0, ref_withdrawn ~ 1,
                            TRUE ~ 1 - share_reference_only),
    bound_width = bound_upper - bound_lower
  ) %>%
  arrange(molecule, period)

write_csv(monthly, here("outputs", "tables", "tbl_item_class_monthly.csv"))

# ─── Summary at the dates the manuscript reports ─────────────────────────────

cli_h1("Summary")

anchor_months <- as.Date(c("2020-10-01", "2021-10-01", "2022-08-01",
                           "2022-09-01", "2022-10-01", "2024-07-01",
                           "2025-02-01", "2025-11-01"))

previous <- read_csv(here("data", "processed", "market_share_national.csv"),
                     show_col_types = FALSE) %>%
  select(molecule, period, previous_share = biosimilar_share)

summary_tbl <- monthly %>%
  filter(period %in% anchor_months | period == max(period)) %>%
  left_join(previous, by = c("molecule", "period")) %>%
  select(molecule, period, total_rx, previous_share,
         share_biosimilar_only, share_mixed, share_reference_only,
         share_not_in_schedule, bound_lower, bound_upper, bound_width) %>%
  distinct()

write_csv(summary_tbl, here("outputs", "tables", "tbl_item_class_summary.csv"))

summary_tbl %>%
  filter(molecule %in% c("adalimumab", "rituximab")) %>%
  mutate(across(where(is.double) & !total_rx, ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ─── Per-molecule identifiability ────────────────────────────────────────────
# Post-listing months only. "Unidentified" = bounds span the full 0-1 range.

by_molecule <- monthly %>%
  filter(post_listing) %>%
  group_by(molecule) %>%
  summarise(
    first_listing = first(first_bio_date),
    months_post_listing = n(),
    months_unidentified = sum(bound_lower < 0.005 & bound_upper > 0.995),
    months_point_identified = sum(bound_width < 0.005),
    latest_period = max(period),
    latest_total_rx = total_rx[period == max(period)],
    latest_lower = bound_lower[period == max(period)],
    latest_upper = bound_upper[period == max(period)],
    latest_mixed = share_mixed[period == max(period)],
    latest_not_in_schedule = share_not_in_schedule[period == max(period)],
    .groups = "drop"
  ) %>%
  left_join(previous %>% group_by(molecule) %>%
              filter(period == max(period)) %>%
              select(molecule, latest_previous_share = previous_share),
            by = "molecule")

write_csv(by_molecule, here("outputs", "tables", "tbl_item_class_by_molecule.csv"))
by_molecule %>% mutate(across(where(is.double), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

cli_alert_success("Wrote tbl_item_brand_composition.csv, tbl_item_class_monthly.csv, tbl_item_class_summary.csv, tbl_item_class_by_molecule.csv")
