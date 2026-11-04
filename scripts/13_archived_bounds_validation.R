################################################################################
# 13_archived_bounds_validation.R
# Tests script 11's single-snapshot assumption against archived item brand
# compositions (script 12), and rebuilds the bounds with time-varying classes
#
# For each item-month the class is taken from the archived snapshots:
#   - the class of the snapshots either side, where they agree and are no more
#     than two years apart
#   - after the last snapshot, its class for up to 400 days; before the first,
#     the first snapshot's class if it is within 200 days
#   - "uncertain" where the snapshots either side disagree (the change fell
#     somewhere between them) or the nearest snapshot is too far away
#   - "not_archived" for items outside script 12's target set
# Uncertain and not_archived volume, like mixed volume, counts toward neither
# bound, so the archived bounds are conservative.
#
# Inputs:
#   - outputs/tables/tbl_archived_item_brands.csv   (script 12)
#   - outputs/tables/tbl_item_brand_composition.csv (script 11)
#   - data/raw/pbs_prescribing/dos_data_raw.rds
#   - data/raw/pbs_prescribing/medicare_stats_raw.rds
#   - data/reference/policy_timeline.csv
#   - data/processed/market_share_national.csv      (conventional series)
# Outputs:
#   - outputs/tables/tbl_archived_vs_snapshot.csv
#   - outputs/tables/tbl_archived_misclassification.csv
#   - outputs/tables/tbl_item_class_monthly_archived.csv
#   - outputs/tables/tbl_archived_bounds_by_molecule.csv
#   - outputs/tables/tbl_rituximab_mabthera_exit.csv
################################################################################

library(tidyverse)
library(here)
library(cli)

max_carry_days <- 400   # after the last snapshot (snapshots are roughly half-yearly)
max_bracket_days <- 730 # between two agreeing snapshots

archived <- read_csv(here("outputs", "tables", "tbl_archived_item_brands.csv"),
                     col_types = cols(item_number = "c", snapshot_date = "D", .default = "c"))
composition <- read_csv(here("outputs", "tables", "tbl_item_brand_composition.csv"),
                        col_types = cols(.default = "c"))
molecules <- unique(archived$molecule)

first_bio <- read_csv(here("data", "reference", "policy_timeline.csv"),
                      show_col_types = FALSE) %>%
  filter(intervention_type == "biosimilar_listing") %>%
  group_by(molecule) %>%
  summarise(first_bio_date = min(as.Date(date)), .groups = "drop")

# Same verified withdrawal dates as script 11
reference_withdrawn <- tribble(
  ~molecule,   ~ref_withdrawn_date,
  "rituximab", as.Date("2021-10-01")
)

# ─── Item-month volume (national, DoS preferred), as in script 11 ───────────

dos <- readRDS(here("data", "raw", "pbs_prescribing", "dos_data_raw.rds"))
med <- readRDS(here("data", "raw", "pbs_prescribing", "medicare_stats_raw.rds"))

rx <- bind_rows(
  dos %>% select(item_number, period, prescriptions, state, source),
  med %>% select(item_number, period, prescriptions, state, source)
) %>%
  filter(state == "National") %>%
  group_by(item_number, period) %>%
  filter(n_distinct(source) == 1 | source == "dos") %>%
  ungroup() %>%
  inner_join(composition %>% filter(molecule %in% molecules) %>%
               select(item_number, molecule, snapshot_class = item_class),
             by = "item_number") %>%
  group_by(molecule, item_number, snapshot_class, period) %>%
  summarise(rx = sum(prescriptions), .groups = "drop") %>%
  inner_join(first_bio, by = "molecule") %>%
  filter(period >= first_bio_date)

# ─── Time-varying archived class per item-month ─────────────────────────────

arch <- archived %>% select(item_number, snapshot_date, archived_class) %>%
  arrange(item_number, snapshot_date)

archived_items <- unique(arch$item_number)

# Month end is the reference point: a snapshot taken during the month counts
item_months <- rx %>% distinct(item_number, period) %>%
  mutate(month_end = ceiling_date(period, "month") - 1)

prev_snap <- item_months %>%
  inner_join(arch, by = "item_number", relationship = "many-to-many") %>%
  filter(snapshot_date <= month_end) %>%
  group_by(item_number, period) %>% slice_max(snapshot_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(item_number, period, prev_date = snapshot_date, prev_class = archived_class)

next_snap <- item_months %>%
  inner_join(arch, by = "item_number", relationship = "many-to-many") %>%
  filter(snapshot_date > month_end) %>%
  group_by(item_number, period) %>% slice_min(snapshot_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(item_number, period, next_date = snapshot_date, next_class = archived_class)

item_class_tv <- item_months %>%
  left_join(prev_snap, by = c("item_number", "period")) %>%
  left_join(next_snap, by = c("item_number", "period")) %>%
  mutate(
    prev_age = as.numeric(month_end - prev_date),
    next_gap = as.numeric(next_date - month_end),
    archived_class_tv = case_when(
      !item_number %in% archived_items ~ "not_archived",
      # between two agreeing snapshots
      !is.na(prev_class) & !is.na(next_class) & next_class == prev_class &
        as.numeric(next_date - prev_date) <= max_bracket_days ~ prev_class,
      # after the last snapshot
      !is.na(prev_class) & is.na(next_class) & prev_age <= max_carry_days ~ prev_class,
      # before the first snapshot: take it if close enough
      is.na(prev_class) & !is.na(next_class) & next_gap <= max_carry_days / 2 ~ next_class,
      TRUE ~ "uncertain"
    )
  ) %>%
  select(item_number, period, archived_class_tv, prev_date, prev_class, next_date, next_class)

rx_tv <- rx %>% left_join(item_class_tv, by = c("item_number", "period"))

# ─── 1. Agreement with the snapshot class, volume weighted ──────────────────

cli_h1("Archived vs February 2026 snapshot class")

agreement <- rx_tv %>%
  filter(archived_class_tv %in% c("reference_only", "biosimilar_only", "mixed")) %>%
  mutate(year = year(period)) %>%
  group_by(molecule, year, snapshot_class, archived_class_tv) %>%
  summarise(rx = sum(rx), .groups = "drop") %>%
  group_by(molecule, year) %>%
  mutate(share_of_year_rx = rx / sum(rx)) %>%
  ungroup()

write_csv(agreement, here("outputs", "tables", "tbl_archived_vs_snapshot.csv"))

# Of the volume whose class the archive identifies: share where the snapshot
# class matches, where the snapshot says biosimilar_only but the archive lists
# a reference brand (inflates the lower bound), and where the snapshot says
# reference_only but the archive lists a biosimilar (deflates the upper bound)
misclass <- agreement %>%
  group_by(molecule, year) %>%
  summarise(
    archived_classified_rx = sum(rx),
    agree = sum(share_of_year_rx[snapshot_class == archived_class_tv]),
    snap_bio_arch_has_ref = sum(share_of_year_rx[snapshot_class == "biosimilar_only" &
                                                 archived_class_tv != "biosimilar_only"]),
    snap_ref_arch_has_bio = sum(share_of_year_rx[snapshot_class == "reference_only" &
                                                 archived_class_tv != "reference_only"]),
    snap_unknown = sum(share_of_year_rx[!snapshot_class %in% c("reference_only", "biosimilar_only", "mixed")]),
    .groups = "drop")

write_csv(misclass, here("outputs", "tables", "tbl_archived_misclassification.csv"))
misclass %>% mutate(across(where(is.double) & !archived_classified_rx, ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ─── 2. Bounds with time-varying archived classes ───────────────────────────

cli_h1("Bounds from archived classes")

conventional <- read_csv(here("data", "processed", "market_share_national.csv"),
                         show_col_types = FALSE) %>%
  select(molecule, period, conventional_share = biosimilar_share)

snapshot_bounds <- read_csv(here("outputs", "tables", "tbl_item_class_monthly.csv"),
                            show_col_types = FALSE, guess_max = Inf) %>%
  filter(molecule %in% molecules) %>%
  select(molecule, period, snapshot_lower = bound_lower, snapshot_upper = bound_upper)

monthly_tv <- rx_tv %>%
  group_by(molecule, period, archived_class_tv) %>%
  summarise(rx = sum(rx), .groups = "drop") %>%
  complete(nesting(molecule, period),
           archived_class_tv = c("reference_only", "biosimilar_only", "mixed",
                                 "uncertain", "not_archived"),
           fill = list(rx = 0)) %>%
  pivot_wider(names_from = archived_class_tv, values_from = rx) %>%
  mutate(total_rx = reference_only + biosimilar_only + mixed + uncertain + not_archived) %>%
  left_join(reference_withdrawn, by = "molecule") %>%
  mutate(
    ref_withdrawn = !is.na(ref_withdrawn_date) & period >= ref_withdrawn_date,
    share_uncertain = uncertain / total_rx,
    share_not_archived = not_archived / total_rx,
    share_mixed = mixed / total_rx,
    archived_lower = if_else(ref_withdrawn, 1, biosimilar_only / total_rx),
    archived_upper = if_else(ref_withdrawn, 1, 1 - reference_only / total_rx)
  ) %>%
  left_join(snapshot_bounds, by = c("molecule", "period")) %>%
  left_join(conventional, by = c("molecule", "period")) %>%
  mutate(
    snapshot_bounds_hold = archived_lower <= snapshot_upper + 1e-9 &
      archived_upper >= snapshot_lower - 1e-9,
    conventional_in_archived = conventional_share >= archived_lower - 1e-9 &
      conventional_share <= archived_upper + 1e-9
  ) %>%
  select(-ref_withdrawn_date) %>%
  arrange(molecule, period)

write_csv(monthly_tv, here("outputs", "tables", "tbl_item_class_monthly_archived.csv"))

by_molecule <- monthly_tv %>%
  group_by(molecule) %>%
  summarise(
    months = n(),
    months_unidentified = sum(archived_lower < 0.005 & archived_upper > 0.995),
    months_point_identified = sum(archived_upper - archived_lower < 0.005),
    months_snapshot_bounds_disjoint = sum(!snapshot_bounds_hold, na.rm = TRUE),
    months_conventional_outside = sum(!conventional_in_archived, na.rm = TRUE),
    months_with_conventional = sum(!is.na(conventional_share)),
    mean_share_uncertain = mean(share_uncertain),
    max_share_uncertain = max(share_uncertain),
    max_share_not_archived = max(share_not_archived),
    latest_period = max(period),
    latest_lower = archived_lower[period == max(period)],
    latest_upper = archived_upper[period == max(period)],
    .groups = "drop"
  )

write_csv(by_molecule, here("outputs", "tables", "tbl_archived_bounds_by_molecule.csv"))
by_molecule %>% mutate(across(where(is.double), ~ round(.x, 3))) %>% print(width = Inf)

# ─── 3. Rituximab: when MabThera left each item ─────────────────────────────

cli_h1("Rituximab: MabThera exit by item")

ritux_item_rx <- rx %>% filter(molecule == "rituximab") %>%
  group_by(item_number) %>% summarise(post_listing_rx = sum(rx), .groups = "drop")

mabthera_exit <- archived %>%
  filter(molecule == "rituximab") %>%
  mutate(has_mabthera = str_detect(tolower(brands), "mabthera")) %>%
  group_by(item_number) %>%
  arrange(snapshot_date, .by_group = TRUE) %>%
  summarise(
    first_snapshot = min(snapshot_date),
    last_snapshot = max(snapshot_date),
    last_with_mabthera = suppressWarnings(max(snapshot_date[has_mabthera])),
    first_without_after = suppressWarnings(min(snapshot_date[!has_mabthera &
                                               snapshot_date > max(snapshot_date[has_mabthera])])),
    ever_mabthera = any(has_mabthera),
    first_brands = first(brands),
    last_brands = last(brands),
    .groups = "drop"
  ) %>%
  mutate(across(c(last_with_mabthera, first_without_after), ~ if_else(is.finite(.x), .x, as.Date(NA)))) %>%
  left_join(ritux_item_rx, by = "item_number") %>%
  arrange(desc(post_listing_rx))

write_csv(mabthera_exit, here("outputs", "tables", "tbl_rituximab_mabthera_exit.csv"))
print(mabthera_exit, n = Inf, width = Inf)

cli_alert_success("Wrote tbl_archived_vs_snapshot.csv, tbl_archived_misclassification.csv, tbl_item_class_monthly_archived.csv, tbl_archived_bounds_by_molecule.csv, tbl_rituximab_mabthera_exit.csv")
