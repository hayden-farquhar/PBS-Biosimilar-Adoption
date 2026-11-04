################################################################################
# 14_break_demonstration.R
# Demonstrates what a structural-break analysis of the conventional
# item-classified share detects: policy events or item events
#
# The conventional classification (scripts 01 and 03) gives each item one fixed
# category, except that items labelled biosimilar are counted as reference
# before the molecule's first biosimilar listing. The conventional share can
# therefore move only when dispensing moves between items, or in the listing
# month, when every biosimilar-labelled item switches category at once.
#
# Bai-Perron breaks (strucchange, h = 0.15, as in the earlier analysis) are
# located on the conventional series for each molecule with archived item
# compositions. strucchange dates a break at the last month of the earlier
# segment, so the change itself is in the following month (change_month).
# Its 95% confidence interval cannot be computed for most of these step-shaped
# series, so each change month is checked against events within +/- 3 months:
#   - policy events from the policy timeline;
#   - item events: months in which items dispensed for the first time carry at
#     least 5% of the molecule's volume, or items carrying at least 5% of the
#     previous month's volume stop being dispensed;
#   - relabelling: in the listing month, the share of volume on items labelled
#     biosimilar that had been dispensed before listing (their volume moves
#     from the reference to the biosimilar category with no change in
#     dispensing).
#
# Inputs:
#   - data/processed/market_share_national.csv   (conventional series)
#   - data/raw/pbs_prescribing/dos_data_raw.rds
#   - data/raw/pbs_prescribing/medicare_stats_raw.rds
#   - data/reference/pbs_item_mapping.csv
#   - data/reference/policy_timeline.csv
# Outputs:
#   - outputs/tables/tbl_item_events.csv
#   - outputs/tables/tbl_break_attribution.csv
#   - outputs/tables/tbl_policy_nearest_break.csv
################################################################################

library(tidyverse)
library(here)
library(strucchange)
library(cli)

molecules <- c("rituximab", "adalimumab", "etanercept", "infliximab")
event_threshold <- 0.05
window_months <- 3
pre_months <- 36   # detection window opens this many months before first listing

timeline <- read_csv(here("data", "reference", "policy_timeline.csv"),
                     show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

first_bio <- timeline %>%
  filter(intervention_type == "biosimilar_listing") %>%
  group_by(molecule) %>%
  summarise(first_bio_date = min(date), .groups = "drop")

# Timeline rows for "all" apply to every molecule, except the streamlined
# authority row, which is anti-TNF only. Exact duplicates of a molecule's own
# row (same date and intervention type) are dropped.
anti_tnf <- c("adalimumab", "etanercept", "infliximab")
all_rows <- timeline %>% filter(molecule == "all")
policy <- bind_rows(
  timeline %>% filter(molecule %in% molecules),
  map_dfr(molecules, function(m) {
    all_rows %>% filter(m %in% anti_tnf | !str_detect(description, "anti-TNF")) %>%
      mutate(molecule = m)
  })
) %>%
  distinct(molecule, date, intervention_type, .keep_all = TRUE) %>%
  select(molecule, policy_date = date, intervention_type, description)

# ─── Item events ─────────────────────────────────────────────────────────────

mapping <- read_csv(here("data", "reference", "pbs_item_mapping.csv"),
                    col_types = cols(.default = "c"))
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
  inner_join(mapping %>% filter(molecule %in% molecules) %>% select(item_number, molecule, type),
             by = "item_number") %>%
  group_by(molecule, item_number, type, period) %>%
  summarise(rx = sum(prescriptions), .groups = "drop") %>%
  filter(rx > 0)

data_start <- min(rx$period)
data_end <- max(rx$period)
item_span <- rx %>% group_by(molecule, item_number) %>%
  summarise(first_month = min(period), last_month = max(period), .groups = "drop")

total <- rx %>% group_by(molecule, period) %>% summarise(total = sum(rx), .groups = "drop") %>%
  arrange(molecule, period) %>% group_by(molecule) %>% mutate(prev_total = lag(total)) %>% ungroup()

# Items first dispensed in the data's first month are not new
entries <- rx %>% inner_join(item_span, by = c("molecule", "item_number")) %>%
  filter(period == first_month, first_month > data_start) %>%
  group_by(molecule, period) %>%
  summarise(new_item_rx = sum(rx), n_new_items = n(), .groups = "drop")

# An exit month is the first month with no dispensing; its size is the item's
# volume in its last month, relative to that month's total
exits <- rx %>% inner_join(item_span, by = c("molecule", "item_number")) %>%
  filter(period == last_month, last_month < data_end) %>%
  mutate(period = period %m+% months(1)) %>%
  group_by(molecule, period) %>%
  summarise(exiting_item_rx = sum(rx), n_exiting_items = n(), .groups = "drop")

# Listing-month relabelling
relabel <- rx %>%
  inner_join(first_bio, by = "molecule") %>%
  inner_join(item_span, by = c("molecule", "item_number")) %>%
  filter(period == first_bio_date, type == "biosimilar", first_month < first_bio_date) %>%
  group_by(molecule, period) %>%
  summarise(relabelled_rx = sum(rx), n_relabelled_items = n(), .groups = "drop")

item_events <- total %>%
  left_join(entries, by = c("molecule", "period")) %>%
  left_join(exits, by = c("molecule", "period")) %>%
  left_join(relabel, by = c("molecule", "period")) %>%
  replace_na(list(new_item_rx = 0, n_new_items = 0, exiting_item_rx = 0,
                  n_exiting_items = 0, relabelled_rx = 0, n_relabelled_items = 0)) %>%
  mutate(new_item_share = new_item_rx / total,
         exiting_item_share = coalesce(exiting_item_rx / prev_total, 0),
         relabelled_share = relabelled_rx / total) %>%
  filter(new_item_share >= event_threshold | exiting_item_share >= event_threshold |
           relabelled_share >= event_threshold)

write_csv(item_events, here("outputs", "tables", "tbl_item_events.csv"))
cli_alert_info("{nrow(item_events)} item events across {n_distinct(item_events$molecule)} molecules")
print(item_events %>% select(molecule, period, new_item_share, exiting_item_share, relabelled_share))

# ─── Breaks on the conventional series ───────────────────────────────────────

conventional <- read_csv(here("data", "processed", "market_share_national.csv"),
                         show_col_types = FALSE) %>%
  filter(molecule %in% molecules) %>%
  select(molecule, period, share = biosimilar_share)

find_breaks <- function(mol) {
  start <- first_bio$first_bio_date[first_bio$molecule == mol] %m-% months(pre_months)
  d <- conventional %>% filter(molecule == mol, period >= start) %>%
    arrange(period) %>% mutate(t = row_number())
  bp <- breakpoints(share ~ t, data = d, h = 0.15)
  if (is.na(bp$breakpoints[1])) return(tibble())
  ci <- suppressWarnings(confint(bp, level = 0.95)$confint)
  b <- bp$breakpoints
  tibble(
    molecule = mol,
    window_start = min(d$period), window_end = max(d$period),
    break_no = seq_along(b),
    change_month = d$period[pmin(b + 1, nrow(d))],
    ci_lower = if_else(is.na(ci[, 1]), as.Date(NA), d$period[pmin(pmax(ci[, 1] + 1, 1), nrow(d))]),
    ci_upper = if_else(is.na(ci[, 3]), as.Date(NA), d$period[pmin(ci[, 3] + 1, nrow(d))]),
    share_before = d$share[b],
    share_after = d$share[pmin(b + 1, nrow(d))],
    share_3m_before = d$share[pmax(b - 2, 1)],
    share_3m_after = d$share[pmin(b + 3, nrow(d))]
  )
}

breaks <- map_dfr(molecules, find_breaks) %>%
  mutate(win_lower = change_month %m-% months(window_months),
         win_upper = change_month %m+% months(window_months))

in_window <- function(b, events, date_col) {
  b %>% inner_join(events, by = "molecule", relationship = "many-to-many") %>%
    filter(.data[[date_col]] >= win_lower, .data[[date_col]] <= win_upper)
}

policy_hits <- in_window(breaks, policy, "policy_date") %>%
  group_by(molecule, break_no) %>%
  summarise(policy_events = paste0(format(policy_date, "%Y-%m"), " ", description, collapse = " | "),
            n_policy = n(), .groups = "drop")

item_hits <- in_window(breaks, item_events %>% rename(event_month = period), "event_month") %>%
  group_by(molecule, break_no) %>%
  summarise(item_events = paste0(format(event_month, "%Y-%m"),
                                 " new ", round(100 * new_item_share, 1), "%",
                                 ", exiting ", round(100 * exiting_item_share, 1), "%",
                                 ", relabelled ", round(100 * relabelled_share, 1), "%",
                                 collapse = " | "),
            n_item = n(), .groups = "drop")

attribution <- breaks %>%
  left_join(policy_hits, by = c("molecule", "break_no")) %>%
  left_join(item_hits, by = c("molecule", "break_no")) %>%
  replace_na(list(n_policy = 0, n_item = 0)) %>%
  mutate(coincides_with = case_when(
    n_policy > 0 & n_item > 0 ~ "policy and item event",
    n_policy > 0 ~ "policy only",
    n_item > 0 ~ "item event only",
    TRUE ~ "neither"),
    change_6m = share_3m_after - share_3m_before) %>%
  select(-win_lower, -win_upper)

write_csv(attribution, here("outputs", "tables", "tbl_break_attribution.csv"))

# For each policy event after first listing: distance to the nearest break
nearest <- policy %>%
  inner_join(first_bio, by = "molecule") %>%
  filter(policy_date > first_bio_date) %>%
  left_join(breaks %>% select(molecule, change_month), by = "molecule",
            relationship = "many-to-many") %>%
  mutate(months_to_break = (year(change_month) - year(policy_date)) * 12 +
           month(change_month) - month(policy_date)) %>%
  group_by(molecule, policy_date, intervention_type, description) %>%
  slice_min(abs(months_to_break), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(break_within_window = abs(months_to_break) <= window_months) %>%
  arrange(molecule, policy_date)

write_csv(nearest, here("outputs", "tables", "tbl_policy_nearest_break.csv"))

attribution %>%
  mutate(across(starts_with("share"), ~ round(.x, 3))) %>%
  select(molecule, change_month, ci_lower, ci_upper, share_before, share_after,
         change_6m, coincides_with, policy_events, item_events) %>%
  print(n = Inf, width = Inf)
nearest %>% select(molecule, policy_date, intervention_type, change_month, months_to_break) %>%
  print(n = Inf)

cli_alert_success("Wrote tbl_item_events.csv, tbl_break_attribution.csv, tbl_policy_nearest_break.csv")
